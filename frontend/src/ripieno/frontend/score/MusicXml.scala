package ripieno.frontend.score

import ripieno.shared.ScorePart

import org.scalajs.dom.*

/** What the page rewrites of a MusicXML score for engraving it: a part alone,
  * or the others smaller.
  */
object MusicXml {

  /** The score with that part alone; the engraver has no way of hiding the
    * others.
    */
  def only(musicXml: String, part: ScorePart): String = {
    val document = parse(musicXml)
    keepTempos(document, part)
    val otherParts =
      elements(document, "part-list > score-part, score-partwise > part")
        .filter(_.getAttribute("id") != part.id)
    val partGroups = elements(document, "part-list > part-group")
    (otherParts ++ partGroups).foreach(element =>
      element.parentNode.removeChild(element)
    )
    new XMLSerializer().serializeToString(document)
  }

  /** The score with every part but `part` engraved at `percent` of its size,
    * each staff of theirs, as cue staves.
    */
  def withOthersSmaller(
      musicXml: String,
      part: ScorePart,
      percent: Int
  ): String = {
    val document = parse(musicXml)
    for {
      other <- elements(document, "score-partwise > part")
      if other.getAttribute("id") != part.id
      measure <- Option(other.querySelector(":scope > measure"))
    } {
      val attributes = Option(measure.querySelector(":scope > attributes"))
        .getOrElse {
          val made = document.createElement("attributes")
          measure.insertBefore(made, measure.firstChild)
          made
        }
      val staves = Option(attributes.querySelector(":scope > staves"))
        .flatMap(_.textContent.trim.toIntOption)
        .getOrElse(1)
      (1 to staves).foreach { staff =>
        val details = document.createElement("staff-details")
        details.setAttribute("number", staff.toString)
        val size = document.createElement("staff-size")
        size.textContent = percent.toString
        details.appendChild(size)
        attributes.appendChild(details)
      }
    }
    new XMLSerializer().serializeToString(document)
  }

  // A score often marks its tempo in one part only, the first: the part kept
  // alone takes those marks, measure by measure, so that it is played and
  // timed at the tempo the whole score is.
  private def keepTempos(document: Document, part: ScorePart): Unit = {
    val parts = elements(document, "score-partwise > part")
    for {
      first <- parts.headOption
      kept <- parts.find(_.getAttribute("id") == part.id)
      if first ne kept
    } {
      val keptMeasures = kept.querySelectorAll(":scope > measure").toList
      first
        .querySelectorAll(":scope > measure")
        .toList
        .zip(keptMeasures)
        .foreach { (from, to) =>
          val tempos = from
            .querySelectorAll(":scope > direction")
            .toList
            .filter(direction =>
              direction.querySelector("metronome, sound[tempo]") != null &&
                to.querySelector(
                  ":scope > direction sound[tempo], :scope > direction metronome"
                ) == null
            )
          // before the measure's first note, where a tempo mark mostly stands
          val first = Option(
            to.querySelector(":scope > note, :scope > backup, :scope > forward")
          )
          tempos.reverse.foreach { direction =>
            val copy = direction.cloneNode(deep = true)
            first.fold(to.appendChild(copy))(to.insertBefore(copy, _))
          }
        }
    }
  }

  private def parse(musicXml: String): Document = {
    new DOMParser().parseFromString(musicXml, MIMEType.`application/xml`)
  }

  private def elements(document: Document, selector: String): List[Element] = {
    document.querySelectorAll(selector).toList
  }
}
