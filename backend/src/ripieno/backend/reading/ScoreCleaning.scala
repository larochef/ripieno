package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*

import scala.collection.mutable

import org.w3c.dom.{Document, Element}

/** What the engraver and the players need of a score that some files leave out:
  * each part's instrument, and every beam closed.
  */
object ScoreCleaning {

  /** Gives each part with no MIDI instrument the General MIDI program its name
    * says (counted from 1), on a channel of its own: the engraver's MIDI then
    * plays a violin as a violin, and the conductor knows its players.
    */
  def instrument(document: Document): Unit =
    at(document.getDocumentElement, "part-list").toList
      .flatMap(children(_, "score-part"))
      .zipWithIndex
      .filter((part, _) => descendants(part, "midi-instrument").isEmpty)
      .foreach { (part, index) =>
        val name = text(part, "part-name").getOrElse("")
        program(name).foreach { program =>
          val id = child(part, "score-instrument")
            .map(_.getAttribute("id"))
            .getOrElse {
              val instrument = added(part, "score-instrument")
              instrument.setAttribute("id", s"${part.getAttribute("id")}-I1")
              added(instrument, "instrument-name").setTextContent(name)
              instrument.getAttribute("id")
            }
          val midi = added(part, "midi-instrument")
          midi.setAttribute("id", id)
          // channel 10 is the drums'
          val channel = index % 15 + 1
          added(midi, "midi-channel").setTextContent(
            (if (channel >= 10) channel + 1 else channel).toString
          )
          added(midi, "midi-program").setTextContent(program.toString)
        }
      }

  /** Closes every beam left open. Some exports (Sibelius through Dolet) begin
    * and continue beams but hardly ever end them, and the engraver then leaves
    * the notes unbeamed: the last note of a beam, followed by another that the
    * beam does not reach, ends it; a beam continued that never began begins; an
    * end with nothing open is no beam.
    */
  def closeBeams(document: Document): Unit =
    for {
      part <- parts(document)
      measure <- children(part, "measure")
    } {
      // the last beamed note of each beam still open, by voice and level
      val open = mutable.Map.empty[(String, String), (Element, Boolean)]
      def close(key: (String, String)): Unit =
        open.remove(key).foreach { (beam, alone) =>
          if (alone) beam.getParentNode.removeChild(beam)
          else beam.setTextContent("end")
        }
      children(measure, "note")
        .filter(note => !has(note, "chord") && !has(note, "grace"))
        .foreach { note =>
          val voice = text(note, "voice").getOrElse("1")
          val beams = children(note, "beam")
            .map(beam =>
              Option(beam.getAttribute("number"))
                .filter(_.nonEmpty)
                .getOrElse("1") -> beam
            )
            .toMap
          // a level this note has no beam at ends that level's beam
          open.keys
            .filter((v, level) => v == voice && !beams.contains(level))
            .toList
            .foreach(close)
          beams.foreach { (level, beam) =>
            val key = (voice, level)
            beam.getTextContent.trim match {
              case "begin" =>
                close(key)
                open(key) = (beam, true)
              case "continue" =>
                // a beam continued that never began begins here
                if (!open.contains(key)) {
                  beam.setTextContent("begin")
                  open(key) = (beam, true)
                } else open(key) = (beam, false)
              case "end" =>
                if (open.contains(key)) open.remove(key)
                // an end with nothing open: no beam
                else beam.getParentNode.removeChild(beam)
              case _ => ()
            }
          }
        }
      open.keys.toList.foreach(close)
    }

  // the General MIDI program, counted from 1, of the words a part's name holds
  private val programs = List(
    List("cello", "violoncelle") -> 43,
    List(
      "violone",
      "contrabass",
      "kontrabass",
      "double bass",
      "contrebasse"
    ) -> 44,
    List("continuo", "basso", "bass") -> 43,
    List("viola", "alto", "bratsche") -> 42,
    List("violin", "violino", "violon", "geige") -> 41,
    List("cembalo", "harpsichord", "clavecin") -> 7,
    List("flaut", "flute", "flöte") -> 74,
    List("oboe", "hautbois") -> 69,
    List("fagott", "bassoon", "basson") -> 71,
    List("organ", "organo", "orgue", "orgel") -> 20
  )

  private def program(partName: String): Option[Int] = {
    val name = partName.toLowerCase
    programs.collectFirst {
      case (words, program) if words.exists(name.contains) => program
    }
  }

  private def added(parent: Element, tag: String): Element =
    parent
      .appendChild(parent.getOwnerDocument.createElement(tag))
      .asInstanceOf[Element]
}
