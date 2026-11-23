package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*

import org.w3c.dom.{Document, Element}

/** Where the elements of a score stand in time, in quarter notes from its
  * start, as written: repeats not unfolded.
  */
object ScoreTimes {

  /** Each child of each measure of a part with its time from the part's start.
    */
  def timed(part: Element): List[(Element, Double)] = {
    var divisions = 1.0
    var measureStart = 0.0
    children(part, "measure").flatMap { measure =>
      at(measure, "attributes", "divisions")
        .flatMap(_.getTextContent.trim.toDoubleOption)
        .foreach(divisions = _)
      var position = 0.0
      var longest = 0.0
      var lastStart = 0.0
      val timedChildren = children(measure).map { element =>
        def duration =
          text(element, "duration").flatMap(_.toDoubleOption).getOrElse(0.0) /
            divisions
        // a chord's later notes start with its first
        val at = if (has(element, "chord")) lastStart else position
        element.getTagName match {
          case "note" if !has(element, "chord") && !has(element, "grace") =>
            lastStart = position
            position += duration
          case "backup"  => position -= duration
          case "forward" => position += duration
          case _         => ()
        }
        longest = math.max(longest, position)
        (element, measureStart + at)
      }
      measureStart += longest
      timedChildren
    }
  }

  /** The start of each measure of the first part, by time, with its number. */
  def measuresOf(document: Document): List[(Double, String)] =
    parts(document).headOption.toList.flatMap { part =>
      timed(part)
        .groupBy(_._1.getParentNode)
        .view
        .mapValues(_.map(_._2).min)
        .toList
        .collect { case (measure: Element, at) =>
          at -> measure.getAttribute("number")
        }
        .sortBy(_._1)
    }

}
