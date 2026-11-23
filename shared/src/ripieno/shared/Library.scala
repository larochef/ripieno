package ripieno.shared

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.Schema

/** A work as the library lists it. `origin` is "catalog" (provided with the
  * application) or "mine" (imported by the player).
  */
final case class WorkSummary(
    id: Long,
    title: String,
    composer: Option[String],
    collection: Option[String],
    origin: String,
    movements: Int,
    parts: List[String]
)

/** A collection of works provided together (a book of tunes, an opus), and how
  * many it holds.
  */
final case class CollectionSummary(
    name: String,
    composer: Option[String],
    works: Int
)

/** What the player opened last of a work: those movements, playing that part.
  */
final case class RecentWork(
    work: WorkSummary,
    movements: List[Long],
    movementTitles: List[String],
    part: Option[String],
    playedAt: String
)

/** The library's home: what to go on with, the player's own works, and what the
  * application provides, in collections or on their own.
  */
final case class Shelves(
    recent: List[RecentWork],
    mine: List[WorkSummary],
    collections: List[CollectionSummary],
    single: List[WorkSummary],
    instruments: List[String]
)

final case class PartSummary(
    id: Long,
    position: Int,
    name: String,
    instrument: Option[String]
)

/** A movement of a work: its measures, the recordings made of it and the places
  * an import left to check in it.
  */
final case class MovementSummary(
    id: Long,
    position: Int,
    title: String,
    measures: Option[Int],
    recordings: Int,
    checks: Int
)

final case class WorkDetail(
    id: Long,
    title: String,
    subtitle: Option[String],
    composer: Option[String],
    arranger: Option[String],
    collection: Option[String],
    edition: Option[String],
    licence: Option[String],
    origin: String,
    parts: List[PartSummary],
    movements: List[MovementSummary],
    sources: List[String]
)

/** That the player opened these movements of a work, playing that part. */
final case class Played(movements: List[Long], part: Option[String])

/** A score file the player imports: its name and its content in base 64. */
final case class ImportedFile(name: String, base64: String)

/** Scores to make a work of: one movement per file, in order. */
final case class ScoreImport(
    title: String,
    composer: Option[String],
    files: List[ImportedFile]
)

/** A score to export: those movements, that part only or all of them, as pages
  * of SVG engraved by the page, to be made into one PDF.
  */
final case class ExportPages(name: String, pages: List[String])

object Library {
  given JsonValueCodec[WorkSummary] = JsonCodecMaker.make
  given JsonValueCodec[List[WorkSummary]] = JsonCodecMaker.make
  given JsonValueCodec[Shelves] = JsonCodecMaker.make
  given JsonValueCodec[WorkDetail] = JsonCodecMaker.make
  given JsonValueCodec[Played] = JsonCodecMaker.make
  given JsonValueCodec[ScoreImport] = JsonCodecMaker.make
  given JsonValueCodec[ExportPages] = JsonCodecMaker.make
  given Schema[WorkSummary] = Schema.derived
  given Schema[CollectionSummary] = Schema.derived
  given Schema[RecentWork] = Schema.derived
  given Schema[Shelves] = Schema.derived
  given Schema[PartSummary] = Schema.derived
  given Schema[MovementSummary] = Schema.derived
  given Schema[WorkDetail] = Schema.derived
  given Schema[Played] = Schema.derived
  given Schema[ImportedFile] = Schema.derived
  given Schema[ScoreImport] = Schema.derived
  given Schema[ExportPages] = Schema.derived

  /** The path the page opens movements by, one after the other. */
  def movementsPath(ids: Seq[Long]): String = s"movements/${ids.mkString(",")}"

  /** The movements a path names, if it names movements. */
  def movementsOf(path: String): Option[List[Long]] =
    Option
      .when(path.startsWith("movements/"))(
        path.stripPrefix("movements/").split(',').toList.map(_.toLongOption)
      )
      .filter(ids => ids.nonEmpty && ids.forall(_.isDefined))
      .map(_.flatten)

  /** The instrument a part's name says, as a key ("violin", "cello"), if any.
    */
  def instrumentOf(name: String): Option[String] = {
    val lowered = name.toLowerCase
    instruments.collectFirst {
      case (word, kind) if lowered.contains(word) => kind
    }
  }

  // the first word found names the instrument: "violoncello" before "violin"
  private val instruments = List(
    "violoncell" -> "cello",
    "cello" -> "cello",
    "viola" -> "viola",
    "bratsche" -> "viola",
    "violin" -> "violin",
    "violone" -> "bass",
    "violon" -> "violin",
    "guitar" -> "guitar",
    "chitarra" -> "guitar",
    "mandolin" -> "mandolin",
    "piano" -> "piano",
    "klavier" -> "piano",
    "keyboard" -> "piano",
    "cembalo" -> "harpsichord",
    "continuo" -> "continuo",
    "bass" -> "bass",
    "flute" -> "flute"
  )
}
