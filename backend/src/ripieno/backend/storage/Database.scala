package ripieno.backend.storage

import java.nio.file.Path
import java.sql.*
import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.util.control.NonFatal

import org.flywaydb.core.Flyway

/** The library's SQLite database, its schema brought up to date by Flyway when
  * it opens (`backend/resources/db/migration`).
  *
  * One connection, one caller at a time: a single player's library, whose
  * requests are short. Plain SQL with `?` parameters; a value is a String, an
  * Int, a Long, a Double, an Option of one, or null.
  */
final class Database private (connection: Connection) {
  private val lock = ReentrantLock()

  /** Runs `work` in one transaction: all of it is kept, or none. */
  def transaction[A](work: Database.Session => A): A = {
    lock.lock()
    try {
      connection.setAutoCommit(false)
      try {
        val result = work(Database.Session(connection))
        connection.commit()
        result
      } catch {
        case NonFatal(error) =>
          connection.rollback()
          throw error
      } finally {
        connection.setAutoCommit(true)
      }
    } finally {
      lock.unlock()
    }
  }

  /** Reads, in a transaction of its own. */
  def read[A](work: Database.Session => A): A = transaction(work)

  def close(): Unit = connection.close()
}

object Database {

  def open(file: Path): Database = {
    val url = s"jdbc:sqlite:$file"
    Flyway
      .configure()
      .dataSource(url, null, null)
      .locations("classpath:db/migration")
      .load()
      .migrate()
    val connection = DriverManager.getConnection(url)
    Using.resource(connection.createStatement()) { statement =>
      // readers do not wait for the writer, and references are kept
      statement.execute("PRAGMA journal_mode = WAL")
      statement.execute("PRAGMA foreign_keys = ON")
      statement.execute("PRAGMA busy_timeout = 5000")
    }
    Database(connection)
  }

  /** A row as read: its columns by name. */
  final class Row(results: ResultSet) {
    def string(column: String): String = results.getString(column)
    def stringOption(column: String): Option[String] =
      Option(results.getString(column))
    def int(column: String): Int = results.getInt(column)
    def long(column: String): Long = results.getLong(column)
    def intOption(column: String): Option[Int] = {
      val value = results.getInt(column)
      Option.when(!results.wasNull())(value)
    }
    def doubleOption(column: String): Option[Double] = {
      val value = results.getDouble(column)
      Option.when(!results.wasNull())(value)
    }
    def boolean(column: String): Boolean = results.getInt(column) != 0
  }

  final class Session(connection: Connection) {

    def query[A](sql: String, parameters: Any*)(read: Row => A): List[A] =
      Using.resource(prepared(sql, parameters)) { statement =>
        Using.resource(statement.executeQuery()) { results =>
          val rows = ListBuffer.empty[A]
          val row = Row(results)
          while (results.next()) {
            rows += read(row)
          }
          rows.toList
        }
      }

    def queryOne[A](sql: String, parameters: Any*)(read: Row => A): Option[A] =
      query(sql, parameters*)(read).headOption

    /** Runs a statement; the rows it changed. */
    def update(sql: String, parameters: Any*): Int =
      Using.resource(prepared(sql, parameters))(_.executeUpdate())

    /** Inserts a row; its id. */
    def insert(sql: String, parameters: Any*): Long =
      Using.resource(prepared(sql, parameters)) { statement =>
        statement.executeUpdate()
        Using.resource(connection.createStatement()) { last =>
          Using.resource(last.executeQuery("SELECT last_insert_rowid()")) {
            results =>
              results.next()
              results.getLong(1)
          }
        }
      }

    private def prepared(
        sql: String,
        parameters: Seq[Any]
    ): PreparedStatement = {
      val statement = connection.prepareStatement(sql)
      parameters.zipWithIndex.foreach { (value, index) =>
        bind(statement, index + 1, value)
      }
      statement
    }

    private def bind(statement: PreparedStatement, at: Int, value: Any): Unit =
      value match {
        case null           => statement.setNull(at, Types.NULL)
        case None           => statement.setNull(at, Types.NULL)
        case Some(inner)    => bind(statement, at, inner)
        case text: String   => statement.setString(at, text)
        case number: Int    => statement.setInt(at, number)
        case number: Long   => statement.setLong(at, number)
        case number: Double => statement.setDouble(at, number)
        case truth: Boolean => statement.setInt(at, if (truth) 1 else 0)
        case other          =>
          throw IllegalArgumentException(s"No SQL value for $other")
      }
  }
}
