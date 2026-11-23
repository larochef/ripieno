package ripieno.backend.library

import ripieno.backend.scores.ScoreLibrary
import ripieno.backend.storage.{BlobStore, Database}
import ripieno.shared.*

import java.nio.file.Files
import java.time.Instant
import java.util.Base64
import scala.util.Try

/** The works of the library, their parts and movements, and the MusicXML the
  * page is given of them: one movement, or several one after the other.
  */
final class LibraryStore(database: Database, blobs: BlobStore) {
  import LibraryStore.*

  def shelves(): Either[String, Shelves] =
    Right(database.read { session =>
      val recent = session
        .query(
          "SELECT p.movement_id, p.played_at, m.work_id, m.title, pt.name AS part " +
            "FROM practice p JOIN movement m ON m.id = p.movement_id LEFT JOIN part pt ON pt.id = p.part_id " +
            "WHERE p.played_at IS NOT NULL ORDER BY p.played_at DESC LIMIT 60"
        )(row =>
          (
            row.long("work_id"),
            row.long("movement_id"),
            row.string("title"),
            row.stringOption("part"),
            row.string("played_at")
          )
        )
        // the movements opened together, by work: those of its last time
        .groupBy(_._1)
        .toList
        .map { (work, rows) =>
          val last = rows.map(_._5).max
          val together = rows.filter(_._5 == last).sortBy(_._2)
          (work, together, last)
        }
        .sortBy(_._3)(using Ordering[String].reverse)
        .take(6)
        .flatMap { (work, together, last) =>
          summary(session, work).map(found =>
            RecentWork(
              found,
              together.map(_._2),
              together.map(_._3),
              together.flatMap(_._4).headOption,
              last
            )
          )
        }
      val mine = summaries(
        session,
        "WHERE w.origin = 'mine' ORDER BY w.created_at DESC"
      )
      val collections = session.query(
        "SELECT collection, COUNT(*) AS works, " +
          "CASE WHEN COUNT(DISTINCT composer) = 1 THEN MAX(composer) END AS composer FROM work " +
          "WHERE origin = 'catalog' AND collection IS NOT NULL GROUP BY collection ORDER BY collection"
      )(row =>
        CollectionSummary(
          row.string("collection"),
          row.stringOption("composer"),
          row.int("works")
        )
      )
      val single = summaries(
        session,
        "WHERE w.origin = 'catalog' AND w.collection IS NULL ORDER BY w.composer, w.title"
      )
      val instruments = session.query(
        "SELECT DISTINCT instrument FROM part WHERE instrument IS NOT NULL ORDER BY instrument"
      )(_.string("instrument"))
      Shelves(recent, mine, collections, single, instruments)
    })

  def search(
      words: String,
      instrument: Option[String]
  ): Either[String, List[WorkSummary]] = {
    val terms = """[\p{L}\p{N}]+""".r
      .findAllIn(words)
      .map(term => s"\"$term\"*")
      .mkString(" ")
    val byInstrument = instrument.filter(_.nonEmpty)
    Right(database.read { session =>
      (terms.nonEmpty, byInstrument) match {
        case (false, None) => Nil
        case (true, _)     =>
          summaries(
            session,
            "WHERE w.id IN (SELECT rowid FROM work_search WHERE work_search MATCH ?) " +
              byInstrument.fold("")(_ =>
                "AND w.id IN (SELECT work_id FROM part WHERE instrument = ?) "
              ) + "ORDER BY w.origin DESC, w.composer, w.title LIMIT 200",
            (terms +: byInstrument.toList)*
          )
        case (false, Some(kind)) =>
          summaries(
            session,
            "WHERE w.id IN (SELECT work_id FROM part WHERE instrument = ?) " +
              "ORDER BY w.origin DESC, w.composer, w.title LIMIT 200",
            kind
          )
      }
    })
  }

  def collection(name: String): Either[String, List[WorkSummary]] =
    Right(database.read { session =>
      summaries(session, "WHERE w.collection = ? ORDER BY w.title", name)
    })

  def work(id: Long): Either[String, WorkDetail] =
    database.read(session => detail(session, id)).toRight(s"No work $id")

  def renamePart(
      work: Long,
      part: Long,
      name: String
  ): Either[String, WorkDetail] =
    if (name.trim.isEmpty) Left("A part needs a name")
    else
      database.transaction { session =>
        val changed = session.update(
          "UPDATE part SET name = ? WHERE id = ? AND work_id = ?",
          name.trim,
          part,
          work
        )
        if (changed == 0) Left(s"No part $part in work $work")
        else {
          indexSearch(session, work)
          detail(session, work).toRight(s"No work $work")
        }
      }

  /** Takes a work of the player's own out of the database, with its files that
    * no other work shares; the movements it had.
    */
  def deleteWork(id: Long): Either[String, List[Long]] = {
    val deleted = database.transaction { session =>
      session.queryOne("SELECT origin FROM work WHERE id = ?", id)(
        _.string("origin")
      ) match {
        case None                             => Left(s"No work $id")
        case Some(origin) if origin != "mine" =>
          Left("Only a work of your own can be deleted")
        case Some(_) =>
          val movements = session.query(
            "SELECT id FROM movement WHERE work_id = ?",
            id
          )(_.long("id"))
          val held = session.query(
            "SELECT score AS hash FROM movement WHERE work_id = ? " +
              "UNION SELECT blob FROM source WHERE work_id = ?",
            id,
            id
          )(_.string("hash"))
          session.update("DELETE FROM work_search WHERE rowid = ?", id)
          session.update("DELETE FROM work WHERE id = ?", id)
          Right((movements, blobs.forgetUnused(session, held)))
      }
    }
    deleted.map { case (movements, files) =>
      files.foreach(Files.deleteIfExists)
      movements
    }
  }

  def played(work: Long, played: Played): Either[String, Unit] =
    database.transaction { session =>
      val now = Instant.now().toString
      val partId = played.part.flatMap(name =>
        session.queryOne(
          "SELECT id FROM part WHERE work_id = ? AND name = ?",
          work,
          name
        )(_.long("id"))
      )
      session.update(
        "UPDATE work SET last_played_at = ? WHERE id = ?",
        now,
        work
      )
      played.movements.foreach { movement =>
        session.update(
          "INSERT INTO practice (movement_id, part_id, played_at) VALUES (?, ?, ?) " +
            "ON CONFLICT (movement_id) DO UPDATE SET part_id = excluded.part_id, played_at = excluded.played_at",
          movement,
          partId,
          now
        )
      }
      Right(())
    }

  /** Whether the page may ask for this path: movements of the library. */
  def exists(path: String): Boolean =
    Library.movementsOf(path).exists { ids =>
      database.read(session =>
        ids.forall(id =>
          session
            .queryOne("SELECT 1 AS found FROM movement WHERE id = ?", id)(
              _.int("found")
            )
            .isDefined
        )
      )
    }

  /** The movements of a path as one MusicXML score: one after the other, each
    * opening with its title, the work's parts named as the player named them.
    */
  def musicXml(path: String): Either[String, String] =
    Library
      .movementsOf(path)
      .toRight(s"No movements in $path")
      .flatMap { ids =>
        val loaded = database.read { session =>
          ids.map { id =>
            session
              .queryOne(
                "SELECT m.work_id, m.title, m.score, b.media_type FROM movement m JOIN blob b ON b.hash = m.score WHERE m.id = ?",
                id
              )(row =>
                (
                  row.long("work_id"),
                  row.string("title"),
                  row.string("score"),
                  row.string("media_type")
                )
              )
              .toRight(s"No movement $id")
              .flatMap { (work, title, hash, media) =>
                blobs
                  .path(session, hash)
                  .toRight(s"The score of movement $id is missing")
                  .map { file =>
                    val parts = session.query(
                      "SELECT mp.xml_part, p.name FROM movement_part mp JOIN part p ON p.id = mp.part_id " +
                        "WHERE mp.movement_id = ? ORDER BY p.position",
                      id
                    )(row => (row.string("xml_part"), row.string("name")))
                    (work, title, file, extensionOf(media), parts)
                  }
              }
          }
        }
        val failed = loaded.collectFirst { case Left(reason) => reason }
        failed match {
          case Some(reason) => Left(reason)
          case None         =>
            val movements = loaded.collect { case Right(found) => found }
            Try(
              movements.map { (_, title, file, extension, parts) =>
                ScoreAssembler.Movement(
                  title,
                  ScoreLibrary.musicXmlOf(file, extension),
                  parts
                )
              }
            ).toEither.left
              .map(error => s"Could not read the score: ${error.getMessage}")
              .flatMap(read =>
                Try(
                  ScoreAssembler.assembled(read, titled = read.sizeIs > 1)
                ).toEither.left
                  .map(error =>
                    s"Could not put the movements together: ${error.getMessage}"
                  )
              )
        }
      }

  /** Makes a work of the player's own of score files, one movement each. */
  def importScores(scoresImport: ScoreImport): Either[String, WorkDetail] =
    if (scoresImport.files.isEmpty) Left("No file to import")
    else {
      val decoded = scoresImport.files.map(file =>
        (file.name, Try(Base64.getDecoder.decode(file.base64)).toOption)
      )
      decoded.collectFirst { case (name, None) => name } match {
        case Some(name) => Left(s"$name could not be read")
        case None       =>
          // each read first: a file the engraver could not be given is refused
          val read = decoded.collect { case (name, Some(bytes)) =>
            val extension =
              name.substring(name.lastIndexOf('.') + 1).toLowerCase
            (
              name,
              bytes,
              extension,
              Try(ScoreLibrary.musicXmlOfBytes(bytes, extension)).toEither
            )
          }
          read.collectFirst { case (name, _, _, Left(error)) =>
            s"$name could not be read: ${error.getMessage}"
          } match {
            case Some(reason) => Left(reason)
            case None         =>
              database.transaction { session =>
                val now = Instant.now().toString
                val work = session.insert(
                  "INSERT INTO work (origin, title, composer, licence, created_at) VALUES ('mine', ?, ?, 'private copy', ?)",
                  scoresImport.title.trim,
                  scoresImport.composer.map(_.trim).filter(_.nonEmpty),
                  now
                )
                val first = read.collectFirst { case (_, _, _, Right(xml)) =>
                  xml
                }.get
                val partNames =
                  numbered(ScoreAssembler.partsOf(first).map(_._2))
                val parts = partNames.zipWithIndex.map { case (name, index) =>
                  session.insert(
                    "INSERT INTO part (work_id, position, name, instrument) VALUES (?, ?, ?, ?)",
                    work,
                    index + 1,
                    name,
                    Library.instrumentOf(name)
                  )
                }
                read.zipWithIndex.foreach {
                  case ((name, bytes, _, xml), index) =>
                    val hash = blobs.put(session, bytes, mediaTypeOf(name))
                    session.insert(
                      "INSERT INTO source (work_id, blob, name, imported_at) VALUES (?, ?, ?, ?)",
                      work,
                      hash,
                      name,
                      now
                    )
                    val xmlParts =
                      xml.toOption.fold(Nil)(ScoreAssembler.partsOf)
                    val movement = session.insert(
                      "INSERT INTO movement (work_id, position, title, score, measures) VALUES (?, ?, ?, ?, ?)",
                      work,
                      index + 1,
                      ScoreAssembler
                        .titleOf(xml.toOption.getOrElse(""))
                        .getOrElse(
                          if (read.sizeIs > 1) s"Movement ${index + 1}"
                          else "Whole piece"
                        ),
                      hash,
                      xml.toOption.map(ScoreAssembler.measuresOf)
                    )
                    parts.zip(xmlParts).foreach { case (part, (xmlPart, _)) =>
                      session.update(
                        "INSERT INTO movement_part (movement_id, part_id, xml_part) VALUES (?, ?, ?)",
                        movement,
                        part,
                        xmlPart
                      )
                    }
                }
                indexSearch(session, work)
                detail(session, work).toRight(s"No work $work")
              }
          }
      }
    }

  /** Writes the work's row of the search index again. */
  def indexSearch(session: Database.Session, work: Long): Unit = {
    session.update("DELETE FROM work_search WHERE rowid = ?", work)
    session.update(
      "INSERT INTO work_search (rowid, title, subtitle, composer, arranger, collection, parts, movements) " +
        "SELECT w.id, w.title, w.subtitle, w.composer, w.arranger, w.collection, " +
        "(SELECT group_concat(name, ' ') FROM part WHERE work_id = w.id), " +
        "(SELECT group_concat(title, ' ') FROM movement WHERE work_id = w.id) FROM work w WHERE w.id = ?",
      work
    )
    ()
  }

  private def summaries(
      session: Database.Session,
      where: String,
      parameters: Any*
  ): List[WorkSummary] = {
    val rows = session.query(
      "SELECT w.id, w.title, w.composer, w.collection, w.origin, " +
        "(SELECT COUNT(*) FROM movement WHERE work_id = w.id) AS movements, " +
        "(SELECT group_concat(name, '\u001f') FROM (SELECT name FROM part WHERE work_id = w.id ORDER BY position)) AS parts " +
        s"FROM work w $where",
      parameters*
    )(row =>
      WorkSummary(
        row.long("id"),
        row.string("title"),
        row.stringOption("composer"),
        row.stringOption("collection"),
        row.string("origin"),
        row.int("movements"),
        row.stringOption("parts").fold(Nil)(_.split('\u001f').toList)
      )
    )
    rows
  }

  private def summary(
      session: Database.Session,
      id: Long
  ): Option[WorkSummary] =
    summaries(session, "WHERE w.id = ?", id).headOption

  private def detail(session: Database.Session, id: Long): Option[WorkDetail] =
    session
      .queryOne("SELECT * FROM work WHERE id = ?", id)(row =>
        (
          row.string("title"),
          row.stringOption("subtitle"),
          row.stringOption("composer"),
          row.stringOption("arranger"),
          row.stringOption("collection"),
          row.stringOption("edition"),
          row.stringOption("licence"),
          row.string("origin")
        )
      )
      .map {
        (
            title,
            subtitle,
            composer,
            arranger,
            collection,
            edition,
            licence,
            origin
        ) =>
          val parts = session.query(
            "SELECT id, position, name, instrument FROM part WHERE work_id = ? ORDER BY position",
            id
          )(row =>
            PartSummary(
              row.long("id"),
              row.int("position"),
              row.string("name"),
              row.stringOption("instrument")
            )
          )
          val movements = session.query(
            "SELECT m.id, m.position, m.title, m.measures, " +
              "(SELECT COUNT(*) FROM recording_movement WHERE movement_id = m.id) AS recordings, " +
              "(SELECT COUNT(*) FROM import_check WHERE movement_id = m.id AND resolved = 0) AS checks " +
              "FROM movement m WHERE m.work_id = ? ORDER BY m.position",
            id
          )(row =>
            MovementSummary(
              row.long("id"),
              row.int("position"),
              row.string("title"),
              row.intOption("measures"),
              row.int("recordings"),
              row.int("checks")
            )
          )
          val sources = session.query(
            "SELECT name FROM source WHERE work_id = ? ORDER BY id",
            id
          )(_.string("name"))
          WorkDetail(
            id,
            title,
            subtitle,
            composer,
            arranger,
            collection,
            edition,
            licence,
            origin,
            parts,
            movements,
            sources
          )
      }
}

object LibraryStore {
  def mediaTypeOf(name: String): String =
    name.substring(name.lastIndexOf('.') + 1).toLowerCase match {
      case "mxl"              => "application/vnd.recordare.musicxml"
      case "musicxml" | "xml" => "application/vnd.recordare.musicxml+xml"
      case "abc"              => "text/vnd.abc"
      case "pdf"              => "application/pdf"
      case other              => s"application/x-$other"
    }

  def extensionOf(mediaType: String): String = mediaType match {
    case "application/vnd.recordare.musicxml"     => "mxl"
    case "application/vnd.recordare.musicxml+xml" => "musicxml"
    case "text/vnd.abc"                           => "abc"
    case other => other.stripPrefix("application/x-")
  }

  /** Names given to several parts, numbered: Violin, Violin is Violin 1, Violin 2.
    */
  private def numbered(names: List[String]): List[String] = {
    val counts = names.groupBy(identity).view.mapValues(_.size).toMap
    names
      .foldLeft((List.empty[String], Map.empty[String, Int])) {
        case ((done, seen), name) =>
          val rank = seen.getOrElse(name, 0) + 1
          val shown = if (counts(name) > 1) s"$name $rank" else name
          (shown :: done, seen.updated(name, rank))
      }
      ._1
      .reverse
  }

}
