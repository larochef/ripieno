package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*
import ripieno.shared.*

import org.w3c.dom.*

/** What the page needs of a score besides its timing and its interpretation:
  * its parts, its first metronome mark's note, each measure's time signature,
  * its rests, its markings and the measures to check (`MeasureChecks`).
  */
object ScoreReader {

  def reading(document: Document): ScoreReading = {
    val scoreParts = parts(document)
    ScoreReading(
      partsOf(document, scoreParts),
      beatQuarters(document),
      meters(scoreParts),
      scoreParts.flatMap(rests),
      scoreParts.zipWithIndex.flatMap(directions),
      MeasureChecks.checks(scoreParts)
    )
  }

  private def partsOf(
      document: Document,
      scoreParts: List[Element]
  ): List[ScorePart] =
    at(document.getDocumentElement, "part-list").toList
      .flatMap(children(_, "score-part"))
      .zipWithIndex
      .map { (part, index) =>
        val id = part.getAttribute("id")
        val staves = scoreParts
          .find(_.getAttribute("id") == id)
          .toList
          .flatMap(children(_, "measure"))
          .flatMap(all(_, "attributes", "staves"))
          .flatMap(_.getTextContent.trim.toIntOption)
          .maxOption
          .getOrElse(1)
        ScorePart(
          id,
          text(part, "part-name")
            .filter(_.nonEmpty)
            .getOrElse(s"Part ${index + 1}"),
          staves
        )
      }

  // the note value, in quarter notes, the score's first metronome mark counts,
  // when it is one the toolbar can show
  private def beatQuarters(document: Document): Option[Double] =
    descendants(document.getDocumentElement, "metronome").headOption
      .flatMap(child(_, "beat-unit"))
      .flatMap { unit =>
        val dotted = Iterator
          .iterate[Node](unit.getNextSibling)(_.getNextSibling)
          .takeWhile(_ != null)
          .collectFirst { case next: Element => next }
          .exists(_.getTagName == "beat-unit-dot")
        val plain = unit.getTextContent.trim match {
          case "half"    => Some(2.0)
          case "quarter" => Some(1.0)
          case "eighth"  => Some(0.5)
          case _         => None
        }
        plain.map(quarters => if (dotted) quarters * 1.5 else quarters)
      }

  // the time signature of every measure, by its number, as the first part
  // writes them
  private def meters(scoreParts: List[Element]): List[MeasureMeter] =
    scoreParts.headOption.toList
      .flatMap(children(_, "measure"))
      .foldLeft((List.empty[MeasureMeter], Meter(4, 4))) {
        case ((found, current), measure) =>
          val meter = at(measure, "attributes", "time")
            .flatMap(time =>
              for {
                beats <- text(time, "beats").flatMap(_.toIntOption)
                beatType <- text(time, "beat-type").flatMap(_.toIntOption)
              } yield Meter(beats, beatType)
            )
            .getOrElse(current)
          (MeasureMeter(measure.getAttribute("number"), meter) :: found, meter)
      }
      ._1
      .reverse

  // every rest of a part with an id, where it stands in its measure
  private def rests(part: Element): List[RestPlace] = {
    var divisions = 1.0
    children(part, "measure").flatMap { measure =>
      at(measure, "attributes", "divisions")
        .flatMap(_.getTextContent.trim.toDoubleOption)
        .foreach(divisions = _)
      var position = 0.0
      children(measure).flatMap { element =>
        val quarters =
          text(element, "duration").flatMap(_.toDoubleOption).getOrElse(0.0) /
            divisions
        val started = position
        element.getTagName match {
          case "note" if !has(element, "chord") && !has(element, "grace") =>
            position += quarters
            Option
              .when(has(element, "rest") && element.hasAttribute("id"))(
                RestPlace(
                  element.getAttribute("id"),
                  measure.getAttribute("number"),
                  started,
                  quarters
                )
              )
              .toList
          case "backup"  => position -= quarters; Nil
          case "forward" => position += quarters; Nil
          case _         => Nil
        }
      }
    }
  }

  // a part's markings, where they stand in their measures
  private def directions(part: Element, index: Int): List[Direction] =
    children(part, "measure")
      .foldLeft((List.empty[Direction], 1.0)) {
        case ((found, divisions), measure) =>
          val counted = at(measure, "attributes", "divisions")
            .flatMap(_.getTextContent.trim.toDoubleOption)
            .getOrElse(divisions)
          val number = measure.getAttribute("number")
          // where each element stands: notes move on but chords do not,
          // backup goes back
          val (_, marked) =
            children(measure).foldLeft((0.0, List.empty[Direction])) {
              case ((position, list), element) =>
                def duration =
                  text(element, "duration")
                    .flatMap(_.toDoubleOption)
                    .getOrElse(0.0) / counted
                element.getTagName match {
                  case "note"
                      if !has(element, "chord") && !has(element, "grace") =>
                    (position + duration, list)
                  case "backup"    => (position - duration, list)
                  case "forward"   => (position + duration, list)
                  case "direction" =>
                    val here = all(element, "direction-type")
                      .flatMap(children)
                      .flatMap { kind =>
                        kind.getTagName match {
                          case "dynamics" =>
                            children(kind).headOption.map(mark =>
                              Direction(
                                index,
                                number,
                                position,
                                "dynamics",
                                mark.getTagName
                              )
                            )
                          case "wedge" =>
                            Option(kind.getAttribute("type"))
                              .filter(_.nonEmpty)
                              .map(
                                Direction(index, number, position, "wedge", _)
                              )
                          case "words" =>
                            Option(kind.getTextContent.trim)
                              .filter(_.nonEmpty)
                              .map(
                                Direction(index, number, position, "words", _)
                              )
                          case _ => None
                        }
                      }
                    (position, list ++ here)
                  case _ => (position, list)
                }
            }
          (found ++ marked, counted)
      }
      ._1
}
