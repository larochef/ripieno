package ripieno.backend.library

import ripieno.backend.storage.{BlobStore, Database}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant
import scala.jdk.CollectionConverters.*
import scala.util.Using

import com.typesafe.scalalogging.Logger

/** The works of the catalog's folder (`CatalogBuilder`), brought into the
  * database at start-up and when its files change. The catalog holds only
  * scores that may be shared: the player's own scans are imported into the
  * library, never listed here.
  */
final class CatalogIndex(
    catalog: Path,
    database: Database,
    blobs: BlobStore,
    library: LibraryStore
) {
  import CatalogIndex.*

  private val logger = Logger(getClass)

  def refresh(): Unit = synchronized {
    val fingerprint = fingerprintOf(catalog)
    val known = database.read(
      _.queryOne("SELECT value FROM setting WHERE key = 'catalog-index'")(
        _.string("value")
      )
    )
    if (!known.contains(fingerprint)) {
      val started = System.nanoTime()
      val works = CatalogBuilder(catalog).works
      database.transaction { session =>
        works.foreach(work => index(session, work))
        session.update(
          "INSERT INTO setting (key, value) VALUES ('catalog-index', ?) " +
            "ON CONFLICT (key) DO UPDATE SET value = excluded.value",
          fingerprint
        )
      }
      logger.info(
        s"Catalog indexed: ${works.size} works in ${(System.nanoTime() - started) / 1000000} ms"
      )
    }
  }

  private def index(session: Database.Session, work: CatalogWork): Unit = {
    val existing = session.queryOne(
      "SELECT id FROM work WHERE catalog_key = ?",
      work.key
    )(_.long("id"))
    existing match {
      case Some(id) =>
        session.update(
          "UPDATE work SET title = ?, composer = ?, arranger = ?, collection = ?, edition = ?, licence = ? WHERE id = ?",
          work.title,
          work.composer,
          work.arranger,
          work.collection,
          work.edition,
          work.licence,
          id
        )
        // a name the player changed stays; one the catalog now numbers
        // (Violin, Violin: Violin 1, Violin 2) follows it
        work.parts.zipWithIndex.foreach { (part, index) =>
          session.update(
            "UPDATE part SET instrument = ?, name = CASE WHEN name = ? THEN ? ELSE name END " +
              "WHERE work_id = ? AND position = ?",
            part.instrument,
            part.name.replaceFirst(" \\d+$", ""),
            part.name,
            id,
            index + 1
          )
        }
        library.indexSearch(session, id)
      case None =>
        val files =
          work.movements.map(movement => catalog.resolve(movement.file))
        if (files.forall(Files.isRegularFile(_))) {
          val id = session.insert(
            "INSERT INTO work (origin, catalog_key, title, composer, arranger, collection, edition, licence, created_at) " +
              "VALUES ('catalog', ?, ?, ?, ?, ?, ?, ?, ?)",
            work.key,
            work.title,
            work.composer,
            work.arranger,
            work.collection,
            work.edition,
            work.licence,
            Instant.now().toString
          )
          val parts = work.parts.zipWithIndex.map { (part, index) =>
            session.insert(
              "INSERT INTO part (work_id, position, name, instrument) VALUES (?, ?, ?, ?)",
              id,
              index + 1,
              part.name,
              part.instrument
            )
          }
          work.movements.zip(files).foreach { (movement, file) =>
            val media = LibraryStore.mediaTypeOf(file.getFileName.toString)
            val score = blobs.refer(session, file, media)
            val movementId = session.insert(
              "INSERT INTO movement (work_id, position, title, score, measures, legacy_path) VALUES (?, ?, ?, ?, ?, ?)",
              id,
              movement.position,
              movement.title,
              score,
              movement.measures,
              movement.file
            )
            parts.zip(movement.xmlParts).foreach { (part, xmlPart) =>
              session.update(
                "INSERT INTO movement_part (movement_id, part_id, xml_part) VALUES (?, ?, ?)",
                movementId,
                part,
                xmlPart
              )
            }
          }
          library.indexSearch(session, id)
        }
    }
  }
}

object CatalogIndex {

  /** What the catalog's files are: each one's path, size and time, hashed. */
  def fingerprintOf(catalog: Path): String = {
    val listing = Using
      .resource(Files.walk(catalog))(_.iterator.asScala.toList)
      .filter(Files.isRegularFile(_))
      .map(file =>
        s"${catalog.relativize(file)} ${Files.size(file)} ${Files.getLastModifiedTime(file).toMillis}"
      )
      .sorted
      .mkString("\n")
    BlobStore.hashOf(listing.getBytes(StandardCharsets.UTF_8))
  }
  final case class CatalogPart(name: String, instrument: Option[String])

  final case class CatalogMovement(
      position: Int,
      title: String,
      file: String,
      measures: Option[Int],
      xmlParts: List[String]
  )

  final case class CatalogWork(
      key: String,
      title: String,
      composer: Option[String],
      arranger: Option[String],
      collection: Option[String],
      licence: Option[String],
      edition: Option[String],
      parts: List[CatalogPart],
      movements: List[CatalogMovement]
  )

}
