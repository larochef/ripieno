package ripieno.frontend.score

import org.scalajs.dom.*

/** A note of the score as it stands, editions played: what the editor shows and
  * changes. `keyFifths` is the key signature it is under.
  */
final case class NoteDetails(
    id: String,
    rest: Boolean,
    step: String,
    alter: Int,
    octave: Int,
    value: String,
    dots: Int,
    tied: Boolean,
    beamed: Boolean,
    fingering: Option[String],
    bowing: Option[String],
    keyFifths: Int
)

/** What the editor shows of a note of the score as the server prepared it (its
  * editions played, every note with its id: `ScoreEditing` on the server).
  */
object ScoreEdits {

  def details(musicXml: String, id: String): Option[NoteDetails] = {
    val document = parse(musicXml)
    note(document, id).map { note =>
      val pitch = Option(note.querySelector("pitch"))
      NoteDetails(
        id,
        rest = note.querySelector("rest") != null,
        step = pitch.flatMap(p => text(p, "step")).getOrElse("C"),
        alter = pitch
          .flatMap(p => text(p, "alter"))
          .flatMap(_.toDoubleOption)
          .fold(0)(_.toInt),
        octave = pitch
          .flatMap(p => text(p, "octave"))
          .flatMap(_.toIntOption)
          .getOrElse(4),
        value = text(note, "type").getOrElse("quarter"),
        dots = note.querySelectorAll(":scope > dot").length,
        tied = note.querySelectorAll(":scope > tie[type='start']").length > 0,
        beamed = Option(note.querySelector(":scope > beam[number='1']"))
          .exists(beam =>
            Set("begin", "continue").contains(beam.textContent.trim)
          ),
        fingering = Option(
          note.querySelector("notations > technical > fingering")
        ).map(_.textContent.trim),
        bowing = Option(
          note.querySelector(
            "notations > technical > up-bow, notations > technical > down-bow"
          )
        ).map(_.tagName),
        keyFifths = keyFifths(note)
      )
    }
  }

  private def keyFifths(note: Element): Int = {
    val part = note.closest("part")
    val measures = part.querySelectorAll(":scope > measure").toList
    val upTo = measures.take(measures.indexWhere(_ eq note.parentNode) + 1)
    upTo.reverse
      .flatMap(m => Option(m.querySelector("attributes > key > fifths")))
      .headOption
      .flatMap(_.textContent.trim.toIntOption)
      .getOrElse(0)
  }

  private def note(document: Document, id: String): Option[Element] =
    Option(document.querySelector(s"note[id='$id']"))

  private def text(element: Element, tag: String): Option[String] =
    Option(element.querySelector(s":scope > $tag")).map(_.textContent.trim)

  private def parse(musicXml: String): Document =
    new DOMParser().parseFromString(musicXml, MIMEType.`application/xml`)
}
