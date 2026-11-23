package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*
import ripieno.shared.{Edit, Edition}

import org.w3c.dom.*

/** Editions played over a MusicXML score, which itself is never changed.
  *
  * Every note of the source is given an id from where it stands (its part, its
  * measure and its rank in the measure), which Verovio keeps in the SVG: an
  * edit names the note it changes by that id, the same each time the source is
  * read, and a click on the sheet names it too.
  */
object ScoreEditing {

  /** Gives an id to every note of the score that has none. */
  def identify(document: Document): Unit =
    for {
      (part, p) <- parts(document).zipWithIndex
      (measure, m) <- children(part, "measure").zipWithIndex
      (note, n) <- children(measure, "note").zipWithIndex
      if !note.hasAttribute("id")
    } note.setAttribute("id", s"n$p-$m-$n")

  /** Plays the visible editions over the score, in order; the edits that found
    * no note to change (made over another state of the source).
    */
  def edit(document: Document, editions: List[Edition]): List[Edit] =
    editions
      .filter(_.visible)
      .flatMap(_.edits)
      .filterNot(edit => applied(document, edit))

  // Applies an edit; false when its note is not in the score.
  private def applied(document: Document, edit: Edit): Boolean =
    note(document, edit.note).fold(false) { target =>
      edit match {
        case Edit.SetPitch(_, step, alter, octave, accidental) =>
          setPitch(target, step, alter, octave, accidental)
        case Edit.SetDuration(_, value, dots) =>
          setDuration(target, value, dots)
        case Edit.MakeRest(_) =>
          removeChildren(target, "pitch", "accidental", "tie")
          remove(all(target, "notations", "tied"))
          if (!has(target, "rest")) {
            target.insertBefore(
              document.createElement("rest"),
              target.getFirstChild
            )
          }
        case Edit.SetTie(_, tied) =>
          setTie(target, tied)
        case Edit.SetBeam(_, joined) =>
          setBeam(target, joined)
        case Edit.AddNote(_, id, step, alter, octave, chord) =>
          val added = target.cloneNode(true).asInstanceOf[Element]
          added.setAttribute("id", id)
          removeChildren(added, "notations", "lyric", "tie", "chord", "beam")
          // after the target and the notes of its chord
          var after: Element = target
          while (nextElement(after).exists(has(_, "chord"))) {
            after = nextElement(after).get
          }
          target.getParentNode.insertBefore(added, after.getNextSibling)
          if (chord) {
            added.insertBefore(
              document.createElement("chord"),
              added.getFirstChild
            )
          }
          setPitch(added, step, alter, octave, None)
        case Edit.SetFingering(_, fingering) =>
          remove(all(target, "notations", "technical", "fingering"))
          fingering.foreach(f =>
            within(target, "notations", "technical", "fingering")
              .setTextContent(f)
          )
        case Edit.SetBowing(_, bowing) =>
          remove(
            all(target, "notations", "technical", "up-bow") ++
              all(target, "notations", "technical", "down-bow")
          )
          bowing.foreach(b => within(target, "notations", "technical", b))
        case Edit.AddSlur(_, to) =>
          note(document, to).foreach { end =>
            // numbers of their own, above those a score uses
            val numbered = descendants(document.getDocumentElement, "slur")
              .count(_.hasAttribute("number"))
            val number = (6 + numbered % 10).toString
            val start = within(target, "notations", "slur")
            start.setAttribute("type", "start")
            start.setAttribute("number", number)
            val stop = within(end, "notations", "slur")
            stop.setAttribute("type", "stop")
            stop.setAttribute("number", number)
          }
        case Edit.RemoveSlurs(_) =>
          remove(all(target, "notations", "slur"))
        case Edit.SetDynamics(_, dynamics) =>
          dynamics.foreach { marking =>
            val direction = before(target, "below")
            direction.getFirstChild
              .appendChild(document.createElement("dynamics"))
              .appendChild(document.createElement(marking))
          }
        case Edit.AddText(_, words) =>
          val direction = before(target, "above")
          direction.getFirstChild
            .appendChild(document.createElement("words"))
            .setTextContent(words)
      }
      true
    }

  // The order MusicXML gives a note's children: a new one goes in its place.
  private val noteOrder = List(
    "grace",
    "chord",
    "pitch",
    "unpitched",
    "rest",
    "duration",
    "tie",
    "instrument",
    "voice",
    "type",
    "dot",
    "accidental",
    "time-modification",
    "stem",
    "notehead",
    "staff",
    "beam",
    "notations",
    "lyric",
    "play"
  )

  private def placed(note: Element, tag: String): Element = {
    val element = note.getOwnerDocument.createElement(tag)
    val later = noteOrder.dropWhile(_ != tag).drop(1).toSet
    val next = children(note).find(child => later.contains(child.getTagName))
    note.insertBefore(element, next.orNull)
    element
  }

  private def setPitch(
      note: Element,
      step: String,
      alter: Int,
      octave: Int,
      accidental: Option[String]
  ): Unit = {
    val document = note.getOwnerDocument
    removeChildren(note, "rest", "pitch", "accidental")
    val pitch = placed(note, "pitch")
    pitch.appendChild(document.createElement("step")).setTextContent(step)
    if (alter != 0) {
      pitch
        .appendChild(document.createElement("alter"))
        .setTextContent(alter.toString)
    }
    pitch
      .appendChild(document.createElement("octave"))
      .setTextContent(octave.toString)
    accidental.foreach(shown =>
      placed(note, "accidental").setTextContent(shown)
    )
  }

  private val quarters = Map(
    "whole" -> 4.0,
    "half" -> 2.0,
    "quarter" -> 1.0,
    "eighth" -> 0.5,
    "16th" -> 0.25,
    "32nd" -> 0.125,
    "64th" -> 0.0625
  )

  private def setDuration(note: Element, value: String, dots: Int): Unit = {
    removeChildren(note, "type", "dot", "time-modification")
    placed(note, "type").setTextContent(value)
    (0 until dots).foreach(_ => placed(note, "dot"))
    val length = quarters.getOrElse(value, 1.0) * (2 - math.pow(0.5, dots))
    val duration = child(note, "duration").getOrElse(placed(note, "duration"))
    duration.setTextContent(
      math.max(1, math.round(length * divisions(note))).toString
    )
  }

  private def setTie(note: Element, tied: Boolean): Unit = {
    val next = nextInVoice(note)
    def ofType(elements: List[Element], kind: String) =
      elements.filter(_.getAttribute("type") == kind)
    remove(
      ofType(children(note, "tie"), "start") ++
        ofType(all(note, "notations", "tied"), "start")
    )
    next.foreach(following =>
      remove(
        ofType(children(following, "tie"), "stop") ++
          ofType(all(following, "notations", "tied"), "stop")
      )
    )
    if (tied) {
      next.foreach { following =>
        placed(note, "tie").setAttribute("type", "start")
        within(note, "notations", "tied").setAttribute("type", "start")
        placed(following, "tie").setAttribute("type", "stop")
        within(following, "notations", "tied").setAttribute("type", "stop")
      }
    }
  }

  // How many beams a note value takes.
  private val beams =
    Map("eighth" -> 1, "16th" -> 2, "32nd" -> 3, "64th" -> 4)

  // The beam between a note and the next one of its voice in the measure made
  // or broken; the beams of the groups on either side drawn again, each level
  // from the values of their notes.
  private def setBeam(note: Element, joined: Boolean): Unit = {
    val voice = text(note, "voice")
    val staff = text(note, "staff")
    val notes = closest(note, "measure").toList
      .flatMap(children(_, "note"))
      .filter(other =>
        !has(other, "chord") && !has(other, "grace") &&
          text(other, "voice") == voice && text(other, "staff") == staff
      )
    val at = notes.indexWhere(_ eq note)
    def level(of: Element): Int =
      text(of, "type").flatMap(beams.get).getOrElse(0)
    if (at >= 0 && at + 1 < notes.size) {
      // whether each note is beamed to the next, as the score has it
      val linked = notes.map(other =>
        children(other, "beam")
          .find(_.getAttribute("number") == "1")
          .exists(beam =>
            Set("begin", "continue").contains(beam.getTextContent.trim)
          )
      )
      val wanted = linked.updated(
        at,
        joined && level(notes(at)) > 0 && level(notes(at + 1)) > 0
      )
      val starts = 0 +: notes.indices.drop(1).filterNot(i => wanted(i - 1))
      val groups = starts
        .zip(starts.drop(1) :+ notes.size)
        .map((from, until) => notes.slice(from, until))
      groups
        .filter(group =>
          group.exists(_ eq notes(at)) || group.exists(_ eq notes(at + 1))
        )
        .foreach { group =>
          group.foreach(member => removeChildren(member, "beam"))
          if (group.sizeIs > 1) {
            (1 to 4).foreach { depth =>
              val needs = group.map(level(_) >= depth)
              group.indices.filter(needs).foreach { i =>
                val before = i > 0 && needs(i - 1)
                val after = i < group.size - 1 && needs(i + 1)
                val value = (before, after) match {
                  case (false, true)  => "begin"
                  case (true, true)   => "continue"
                  case (true, false)  => "end"
                  case (false, false) =>
                    if (i < group.size - 1) "forward hook" else "backward hook"
                }
                val beam = placed(group(i), "beam")
                beam.setAttribute("number", depth.toString)
                beam.setTextContent(value)
              }
            }
          }
        }
    }
  }

  // The next note of the same voice in the part, chords aside.
  private def nextInVoice(note: Element): Option[Element] = {
    val voice = text(note, "voice")
    val notes = closest(note, "part").toList.flatMap(descendants(_, "note"))
    notes
      .drop(notes.indexWhere(_ eq note) + 1)
      .find(other => !has(other, "chord") && text(other, "voice") == voice)
  }

  // The note's divisions of a quarter, from the last attributes before it.
  private def divisions(note: Element): Double = {
    val measures = closest(note, "part").toList.flatMap(children(_, "measure"))
    val upTo = measures.take(measures.indexWhere(_ eq note.getParentNode) + 1)
    upTo.reverse
      .flatMap(measure => at(measure, "attributes", "divisions"))
      .headOption
      .flatMap(_.getTextContent.trim.toDoubleOption)
      .getOrElse(1.0)
  }

  // A direction just before the note, its type to be filled.
  private def before(note: Element, placement: String): Element = {
    val document = note.getOwnerDocument
    val direction = document.createElement("direction")
    direction.setAttribute("placement", placement)
    direction.appendChild(document.createElement("direction-type"))
    text(note, "staff").foreach(staff =>
      direction
        .appendChild(document.createElement("staff"))
        .setTextContent(staff)
    )
    // a chord's notes follow its first: the marking goes before that one
    var first = note
    while (has(first, "chord") && previousElement(first).isDefined) {
      first = previousElement(first).get
    }
    note.getParentNode.insertBefore(direction, first)
    direction
  }

  // The element at the end of a path of children, made where missing.
  private def within(note: Element, path: String*): Element =
    path.foldLeft(note) { (parent, tag) =>
      children(parent, tag).headOption
        .filter(_ =>
          tag != path.last || tag == "notations" || tag == "technical"
        )
        .getOrElse {
          if (parent eq note) placed(note, tag)
          else
            parent
              .appendChild(note.getOwnerDocument.createElement(tag))
              .asInstanceOf[Element]
        }
    }

  private def nextElement(element: Element): Option[Element] =
    Iterator
      .iterate[Node](element.getNextSibling)(_.getNextSibling)
      .takeWhile(_ != null)
      .collectFirst { case found: Element => found }

  private def previousElement(element: Element): Option[Element] =
    Iterator
      .iterate[Node](element.getPreviousSibling)(_.getPreviousSibling)
      .takeWhile(_ != null)
      .collectFirst { case found: Element => found }

  private def removeChildren(element: Element, tags: String*): Unit =
    remove(children(element).filter(child => tags.contains(child.getTagName)))

  private def remove(elements: List[Element]): Unit =
    elements.foreach(found => found.getParentNode.removeChild(found))

  private def note(document: Document, id: String): Option[Element] =
    descendants(document.getDocumentElement, "note")
      .find(_.getAttribute("id") == id)
}
