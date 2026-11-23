package ripieno.frontend.score

import ripieno.frontend.i18n.{NoteNames, Tr}
import ripieno.frontend.model.SuspectMeasure
import ripieno.shared.{MeasureCheck, WrittenPitch}

/** The measures the server found doubtful in a score (`MeasureChecks` on the
  * server), told in the page's language.
  */
object MeasureChecks {

  def suspects(checks: List[MeasureCheck]): List[SuspectMeasure] =
    checks.map(check =>
      SuspectMeasure(
        check.part,
        check.measure,
        check.kind,
        detail(check),
        check.notes
      )
    )

  private def detail(check: MeasureCheck): String =
    check.tiedFrom match {
      case Some(from) if check.tiedTo.isEmpty =>
        Tr(
          s"a tie from ${name(from)} to a rest",
          s"une liaison de tenue de ${name(from)} vers un silence"
        )
      case Some(from) =>
        val to = check.tiedTo.map(name).mkString(", ")
        Tr(
          s"a tie from ${name(from)} to $to: a slur, or a wrong note?",
          s"une liaison de tenue de ${name(from)} à $to : une liaison d'expression, ou une fausse note ?"
        )
      case None =>
        Tr(
          s"${beatsText(check.beatsFound)} beats instead of ${check.beatsWritten}",
          s"${beatsText(check.beatsFound)} temps au lieu de ${check.beatsWritten}"
        )
    }

  private def name(pitch: WrittenPitch): String =
    NoteNames.written(pitch.step, pitch.alter, pitch.octave)

  // 3.5 is "3 ½"
  private def beatsText(beats: Double): String = {
    val whole = math.floor(beats + 1e-6).toInt
    val rest = beats - whole
    val fraction = List(
      0.25 -> "¼",
      1.0 / 3 -> "⅓",
      0.5 -> "½",
      2.0 / 3 -> "⅔",
      0.75 -> "¾"
    ).collectFirst {
      case (value, sign) if math.abs(rest - value) < 0.01 => sign
    }
    if (rest < 0.01) whole.toString
    else
      fraction.fold(f"$beats%.2f")(sign =>
        if (whole == 0) sign else s"$whole $sign"
      )
  }
}
