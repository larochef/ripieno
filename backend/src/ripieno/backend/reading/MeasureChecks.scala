package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*
import ripieno.shared.{MeasureCheck, WrittenPitch}

import org.w3c.dom.Element

/** What a reading of a score most often gets wrong, found by its own
  * consistency: a measure longer or shorter than its time signature says, and a
  * tie between two different notes (a slur read as a tie, or a wrong pitch).
  */
object MeasureChecks {

  /** The measures of the parts (in score order) to check. */
  def checks(parts: List[Element]): List[MeasureCheck] =
    parts.zipWithIndex.flatMap((part, index) => ofPart(part, index))

  private def ofPart(part: Element, index: Int): List[MeasureCheck] = {
    val measures = children(part, "measure")
    var divisions = 1.0
    var meter: Option[(Int, Int)] = Some((4, 4))
    val lengths = measures.map { measure =>
      at(measure, "attributes", "divisions")
        .flatMap(_.getTextContent.trim.toDoubleOption)
        .foreach(divisions = _)
      at(measure, "attributes", "time").foreach { time =>
        meter = for {
          beats <- text(time, "beats").flatMap(_.toIntOption)
          beatType <- text(time, "beat-type").flatMap(_.toIntOption)
        } yield (beats, beatType)
      }
      (length(measure) / divisions, meter)
    }
    val wrongLengths = measures.zip(lengths).zipWithIndex.flatMap {
      case ((measure, (quarters, Some((beats, beatType)))), position) =>
        val expected = beats * 4.0 / beatType
        val beat = 4.0 / beatType
        val shortAllowed = position == 0 ||
          position == measures.size - 1 ||
          measure.getAttribute("implicit") == "yes" ||
          endsSection(measure) ||
          measures.lift(position - 1).exists(endsSection) ||
          measures.lift(position + 1).exists(startsRepeat)
        val tooLong = quarters > expected + 1e-6
        val tooShort = quarters < expected - 1e-6 && quarters > 0 &&
          !shortAllowed
        Option.when(tooLong || tooShort)(
          MeasureCheck(
            index,
            measure.getAttribute("number"),
            if (tooLong) "too-long" else "too-short",
            quarters / beat,
            beats,
            None,
            Nil,
            noteIds(measure)
          )
        )
      case _ => None
    }
    wrongLengths ++ wrongTies(part, index)
  }

  // How far the measure runs, in divisions: its furthest point, voices and
  // backups counted.
  private def length(measure: Element): Double = {
    var cursor = 0.0
    var furthest = 0.0
    children(measure).foreach { element =>
      val duration =
        text(element, "duration").flatMap(_.toDoubleOption).getOrElse(0.0)
      element.getTagName match {
        case "note" if !has(element, "chord") && !has(element, "grace") =>
          cursor += duration
        case "backup"  => cursor -= duration
        case "forward" => cursor += duration
        case _         => ()
      }
      furthest = math.max(furthest, cursor)
    }
    furthest
  }

  private def endsSection(measure: Element): Boolean =
    children(measure, "barline").exists(barline =>
      barline.getAttribute("location") != "left" && (
        child(barline, "repeat").exists(
          _.getAttribute("direction") == "backward"
        ) ||
          text(barline, "bar-style").exists(
            Set("light-heavy", "light-light").contains
          )
      )
    )

  private def startsRepeat(measure: Element): Boolean =
    children(measure, "barline")
      .filter(_.getAttribute("location") == "left")
      .flatMap(children(_, "repeat"))
      .exists(_.getAttribute("direction") == "forward")

  // A tie starting on a note whose pitch the next notes of its voice don't
  // have.
  private def wrongTies(part: Element, index: Int): List[MeasureCheck] = {
    val notes = children(part, "measure").flatMap(children(_, "note"))
    val voiced = notes.filterNot(has(_, "grace"))
    def voiceOf(note: Element) = text(note, "voice").getOrElse("")
    def staffOf(note: Element) = text(note, "staff").getOrElse("")
    voiced.zipWithIndex.flatMap { (note, position) =>
      val tied =
        children(note, "tie").exists(_.getAttribute("type") == "start")
      if (!tied) None
      else {
        pitch(note).flatMap { from =>
          // the next chord of the same voice and staff
          val after = voiced
            .drop(position + 1)
            .filter(other =>
              voiceOf(other) == voiceOf(note) && staffOf(other) == staffOf(note)
            )
          val next = after.dropWhile(has(_, "chord")).headOption.toList
          val chord = next ++ after
            .dropWhile(has(_, "chord"))
            .drop(1)
            .takeWhile(has(_, "chord"))
          val pitches = chord.flatMap(pitch)
          Option.when(chord.nonEmpty && !pitches.contains(from)) {
            MeasureCheck(
              index,
              closest(note, "measure").fold("")(_.getAttribute("number")),
              "tie",
              0,
              0,
              Some(from),
              pitches,
              note.getAttribute("id") :: chord.map(_.getAttribute("id"))
            )
          }
        }
      }
    }
  }

  private def pitch(note: Element): Option[WrittenPitch] =
    child(note, "pitch").map { written =>
      WrittenPitch(
        text(written, "step").getOrElse(""),
        text(written, "alter")
          .flatMap(_.toDoubleOption)
          .fold(0)(math.round(_).toInt),
        text(written, "octave").flatMap(_.toIntOption).getOrElse(4)
      )
    }

  private def noteIds(measure: Element): List[String] =
    children(measure, "note")
      .filter(_.hasAttribute("id"))
      .map(_.getAttribute("id"))
}
