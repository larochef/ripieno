package ripieno.frontend.model

/** The note value a tempo counts, as a metronome mark writes it. */
enum Beat(val symbol: String, val quarters: Double) {
  case Half extends Beat("𝅗𝅥", 2)
  case DottedHalf extends Beat("𝅗𝅥.", 3)
  case Quarter extends Beat("♩", 1)
  case DottedQuarter extends Beat("♩.", 1.5)
  case Eighth extends Beat("♪", 0.5)
  case DottedEighth extends Beat("♪.", 0.75)
}

/** So many beats a minute: "♩ = 112". */
final case class Tempo(beat: Beat, beatsPerMinute: Int)

object Tempo {
  // of the written tempo: the players keep a natural sound within
  val slowest = 0.25
  val fastest = 2.0

  /** That tempo, brought within what can be played of one written at `written`.
    */
  def within(written: Int, beatsPerMinute: Int): Int =
    math.min(
      math.max(beatsPerMinute, math.ceil(written * slowest).toInt),
      math.floor(written * fastest).toInt
    )
}

/** A section of a score at its own tempo: its name, if it has one, the measure
  * it starts at, and the tempo it is written at, if it is, and the one it is
  * played at, in beats a minute of the score's `beat`.
  */
final case class SectionTempo(
    index: Int,
    name: Option[String],
    measure: String,
    beat: Beat,
    written: Option[Int],
    chosen: Int
) {

  /** That tempo, brought within what this section can be played at. */
  def within(beatsPerMinute: Int): Int =
    Tempo.within(written.getOrElse(chosen), beatsPerMinute)
}
