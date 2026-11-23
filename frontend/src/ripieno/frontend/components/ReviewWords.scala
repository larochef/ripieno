package ripieno.frontend.components

import ripieno.frontend.i18n.{NoteNames, Tr}
import ripieno.frontend.model.Tempo
import ripieno.shared.{PerformanceReview, ReviewedNote}

/** How the review of a recording says what it found. */
object ReviewWords {

  // the colours of intonation, slightly off then far off: warm when sharp, cool
  // when flat; a wrong note, higher or lower, is the deepest of its side
  private val sharp = ((0xf6, 0xb2, 0x6b), (0xe6, 0x51, 0x00))
  private val flat = ((0x80, 0xcb, 0xc4), (0x00, 0x89, 0x7b))
  private val wrongHigher = "#b71c1c"
  private val wrongLower = "#004d40"
  private val slightly = 25.0
  private val far = 50.0

  /** The colour of a note out of tune by `value` cents: its hue says sharp or
    * flat, its depth how far, from a little (25 cents) to a lot (50 and more).
    */
  def intonationColour(value: Int): String =
    mixed(
      if (value > 0) sharp else flat,
      (math.abs(value) - slightly) / (far - slightly)
    )

  /** The colour of a wrong note, the side of the colours of intonation it is
    * on: higher than written with the sharp notes, lower with the flat ones.
    */
  def wrongColour(higher: Boolean): String =
    if (higher) wrongHigher else wrongLower

  /** Whether a wrong note was heard higher than the pitch written, in MIDI. */
  def higher(note: ReviewedNote, written: Option[Int]): Option[Boolean] =
    for {
      heard <- note.heard
      pitch <- written
    } yield heard > pitch

  /** The wrong note heard, and which way it is from the one written. */
  def wrongNote(note: ReviewedNote, written: Option[Int]): String = {
    val way = higher(note, written).fold("")(up =>
      if (up) Tr(", higher than written", ", plus haut que la note écrite")
      else Tr(", lower than written", ", plus bas que la note écrite")
    )
    note.heard.fold(Tr("a wrong note", "une fausse note"))(heard =>
      Tr(
        s"${NoteNames.ofMidi(heard)} heard$way",
        s"${NoteNames.ofMidi(heard)} entendu$way"
      )
    )
  }

  // the colours of rhythm, slightly off then far off: violet when early, blue when late
  private val early = ((0xb3, 0x9d, 0xdb), (0x31, 0x1b, 0x92))
  private val late = ((0x90, 0xca, 0xf9), (0x0d, 0x47, 0xa1))
  // shares of a beat
  val slightlyOffTime = 0.15
  val farOffTime = 0.4

  /** The colour of a note starting `millis` early (below 0) or late, in beats
    * of `beatMillis`: its hue says early or late, its depth how far, from a
    * little (15 % of a beat) to a lot (40 % and more).
    */
  def rhythmColour(millis: Int, beatMillis: Double): String =
    mixed(
      if (millis < 0) early else late,
      (math.abs(millis) / beatMillis - slightlyOffTime) /
        (farOffTime - slightlyOffTime)
    )

  /** A beat of the recording, in milliseconds: the written tempo's, at the pace
    * the player kept overall.
    */
  def playedBeatMillis(review: PerformanceReview, written: Tempo): Double =
    60000.0 / written.beatsPerMinute *
      (if (review.tempo.overallRatio > 0) review.tempo.overallRatio else 1)

  // From the first colour to the second, `depth` from 0 to 1.
  private def mixed(
      colours: ((Int, Int, Int), (Int, Int, Int)),
      depth: Double
  ): String = {
    val ((r1, g1, b1), (r2, g2, b2)) = colours
    val within = math.max(0.0, math.min(1.0, depth))
    def channel(a: Int, b: Int) = math.round(a + (b - a) * within).toInt
    f"#${channel(r1, r2)}%02x${channel(g1, g2)}%02x${channel(b1, b2)}%02x"
  }

  def cents(value: Int): String =
    if (value > 0) Tr(s"$value cents sharp", s"$value cents trop haut")
    else Tr(s"${-value} cents flat", s"${-value} cents trop bas")

  /** A note's rhythm as the review gives it, "early" or "late", in words. */
  def timing(kind: String): String = kind match {
    case "early" => Tr("early", "en avance")
    case "late"  => Tr("late", "en retard")
    case other   => other
  }

  /** How far and which way a note started off time: 80 ms early. */
  def offTime(kind: String, millis: Int): String =
    s"${math.abs(millis)} ms ${timing(kind)}"

  /** A drift of tempo as the review gives it, "faster" or "slower", in words.
    */
  def drift(kind: String): String = kind match {
    case "faster" => Tr("faster", "plus rapide")
    case "slower" => Tr("slower", "plus lent")
    case other    => other
  }

  /** What is wrong with a note, said in full; none for a note heard as written.
    */
  def ofNote(note: ReviewedNote, written: Option[Int]): Option[String] = {
    val pitch = note.verdict match {
      case "wrong" =>
        Some(
          Tr(
            s"A wrong note: ${wrongNote(note, written)}",
            s"Fausse note : ${wrongNote(note, written)}"
          )
        )
      case "intonation" =>
        note.cents.map(value =>
          Tr(s"Out of tune: ${cents(value)}", s"Pas juste : ${cents(value)}")
        )
      case "missed" =>
        Some(
          Tr(
            "Not heard: missed, or covered by another note",
            "Pas entendue : manquée, ou couverte par une autre note"
          )
        )
      case "uncertain" =>
        Some(Tr("Not clearly heard", "Pas clairement entendue"))
      case _ => None
    }
    val rhythm = note.rhythm
      .zip(note.onsetMillis)
      .map((kind, millis) => offTime(kind, millis))
    Option(List(pitch, rhythm).flatten).filter(_.nonEmpty).map(_.mkString(", "))
  }
}
