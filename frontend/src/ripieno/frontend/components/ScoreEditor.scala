package ripieno.frontend.components

import ripieno.frontend.i18n.{NoteNames, Tr}
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.NoteDetails
import ripieno.shared.{Edit, Edition}

import scala.scalajs.js

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Edits the score in editions over it: which editions are shown, which one
  * edits go to, and the changes to the note selected on the sheet.
  *
  * Keys, a note selected: ↑ ↓ a step (with Shift, an octave), # b n its
  * accidental, 0 to 5 its fingering, t its tie, j its beam to the next note,
  * Delete makes it a rest, Escape lets it go; Ctrl+Z takes the last edit back.
  */
class ScoreEditor(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val editing: Signal[Boolean] =
    events.collect { case EditingSet(on) => on }.startWith(false)

  private val editions: Signal[(List[Edition], Option[String])] = events
    .collect { case EditionsChanged(_, editions, active) => (editions, active) }
    .startWith((Nil, None))

  private val selected: Signal[Option[NoteDetails]] =
    events
      .collect {
        case NoteSelected(note) => note
        case _: ScoreOpened     => None
      }
      .startWith(None)

  // the next note clicked ends the slur started on this one
  private val slurFrom: Var[Option[String]] = Var(None)
  private val newEditionName: Var[String] = Var("")

  private val steps = Vector("C", "D", "E", "F", "G", "A", "B")
  private val sharpsOrder = "FCGDAEB"

  // The alteration the key signature gives a step.
  private def keyAlter(step: String, fifths: Int): Int =
    if (fifths > 0 && sharpsOrder.take(fifths).contains(step)) 1
    else if (fifths < 0 && sharpsOrder.reverse.take(-fifths).contains(step)) -1
    else 0

  // A step up or down, the key signature's alteration for the new step.
  private def moved(note: NoteDetails, by: Int): Edit = {
    val index = steps.indexOf(note.step) + by
    val octave = note.octave + Math.floorDiv(index, 7)
    val step = steps(Math.floorMod(index, 7))
    Edit.SetPitch(note.id, step, keyAlter(step, note.keyFifths), octave, None)
  }

  private def altered(note: NoteDetails, alter: Int): Edit = {
    val shown = alter match {
      case 1  => "sharp"
      case -1 => "flat"
      case _  => "natural"
    }
    // shown only where the key would say otherwise
    val accidental =
      Option.when(keyAlter(note.step, note.keyFifths) != alter)(shown)
    Edit.SetPitch(note.id, note.step, alter, note.octave, accidental)
  }

  private def octaveMoved(note: NoteDetails, by: Int): Edit =
    Edit.SetPitch(note.id, note.step, note.alter, note.octave + by, None)

  // A note value as MusicXML writes it, in words.
  private def valueName(value: String): String = value match {
    case "whole"   => Tr("whole", "ronde")
    case "half"    => Tr("half", "blanche")
    case "quarter" => Tr("quarter", "noire")
    case "eighth"  => Tr("eighth", "croche")
    case "16th"    => Tr("16th", "double croche")
    case "32nd"    => Tr("32nd", "triple croche")
    case "64th"    => Tr("64th", "quadruple croche")
    case other     => other
  }

  // A bowing as MusicXML writes it, in words.
  private def bowingName(bowing: String): String = bowing match {
    case "down-bow" => Tr("down-bow", "tiré")
    case "up-bow"   => Tr("up-bow", "poussé")
    case other      => other
  }

  private def describe(note: NoteDetails): String = {
    val pitch =
      if (note.rest) Tr("rest", "silence")
      else NoteNames.written(note.step, note.alter, note.octave)
    val dots = "." * note.dots
    val extras = List(
      note.fingering.map(f => Tr(s"finger $f", s"doigt $f")),
      note.bowing.map(bowingName),
      Option.when(note.tied)(Tr("tied", "lié"))
    ).flatten
    (s"$pitch, ${valueName(note.value)}$dots" :: extras).mkString(", ")
  }

  private def apply(edit: Edit): Unit = bus.send(ApplyEdit(edit))

  private def noteButton(label: String, hint: String)(
      edit: NoteDetails => Edit
  ): HtmlElement =
    button(
      cls := "button is-small",
      title := hint,
      label,
      onClick
        .compose(_.sample(selected)) --> (_.foreach(note => apply(edit(note))))
    )

  private val durations = List(
    "whole" -> "𝅝",
    "half" -> "𝅗𝅥",
    "quarter" -> "♩",
    "eighth" -> "♪",
    "16th" -> "𝅘𝅥𝅯",
    "32nd" -> "𝅘𝅥𝅰"
  )

  private val dynamicsChoices =
    List("pp", "p", "mp", "mf", "f", "ff", "sfz", "fp")

  private val editionsControl: HtmlElement = div(
    cls := "editor-group",
    span(cls := "part-choice-label", Tr("Editions", "Versions")),
    children <-- editions.map((all, active) =>
      all.map(edition =>
        div(
          cls := "buttons has-addons mb-0",
          button(
            cls := "button is-small mb-0",
            cls("is-link is-selected") := active.contains(edition.name),
            title := Tr(
              "Edits go to this edition",
              "Les modifications vont dans cette version"
            ),
            edition.name,
            onClick --> (_ => bus.send(UseEdition(edition.name)))
          ),
          button(
            cls := "button is-small mb-0",
            title := (
              if (edition.visible) Tr("Hide it", "La masquer")
              else Tr("Show it", "L'afficher")
            ),
            if (edition.visible) Tr("shown", "affichée")
            else Tr("hidden", "masquée"),
            onClick --> (_ =>
              bus.send(ShowEdition(edition.name, !edition.visible))
            )
          )
        )
      )
    ),
    input(
      cls := "input is-small editor-name",
      placeholder := Tr("New edition", "Nouvelle version"),
      controlled(
        value <-- newEditionName,
        onInput.mapToValue --> newEditionName
      ),
      onKeyDown.filter(_.key == "Enter").compose(_.sample(newEditionName)) --> {
        name =>
          bus.send(NewEdition(name))
          newEditionName.set("")
      }
    ),
    button(
      cls := "button is-small",
      title := Tr(
        "Take back the last edit (Ctrl+Z)",
        "Annuler la dernière modification (Ctrl+Z)"
      ),
      Tr("Undo", "Annuler"),
      onClick --> (_ => bus.send(UndoEdit))
    )
  )

  private val noteControls: HtmlElement = div(
    cls := "editor-group",
    child.text <-- selected.combineWith(slurFrom.signal).map {
      case (_, Some(_)) =>
        Tr(
          "Click the note the slur ends on",
          "Cliquez sur la note où finit la liaison"
        )
      case (Some(note), _) => describe(note)
      case (None, _)       =>
        Tr("Click a note to edit it", "Cliquez sur une note pour la modifier")
    },
    div(
      cls := "editor-buttons",
      hidden <-- selected.map(_.isEmpty),
      noteButton(
        "↑",
        Tr(
          "A step up (↑; with Shift, an octave)",
          "Un degré plus haut (↑ ; avec Maj, une octave)"
        )
      )(moved(_, 1)),
      noteButton("↓", Tr("A step down (↓)", "Un degré plus bas (↓)"))(
        moved(_, -1)
      ),
      noteButton("♯", Tr("Sharp (#)", "Dièse (#)"))(altered(_, 1)),
      noteButton("♭", Tr("Flat (b)", "Bémol (b)"))(altered(_, -1)),
      noteButton("♮", Tr("Natural (n)", "Bécarre (n)"))(altered(_, 0)),
      durations.map((value, symbol) =>
        noteButton(symbol, valueName(value))(note =>
          Edit.SetDuration(note.id, value, 0)
        )
      ),
      noteButton("·", Tr("Dotted", "Pointée"))(note =>
        Edit.SetDuration(note.id, note.value, (note.dots + 1) % 3)
      ),
      noteButton(
        Tr("Rest", "Silence"),
        Tr("Make it a rest (Delete)", "En faire un silence (Suppr)")
      )(note => Edit.MakeRest(note.id)),
      noteButton(
        Tr("Tie", "Lier"),
        Tr("Tie it to the next note (t)", "La lier à la note suivante (t)")
      )(note => Edit.SetTie(note.id, !note.tied)),
      noteButton(
        Tr("Beam", "Ligature"),
        Tr(
          "Beam it with the next note, or break the beam between them (j)",
          "La relier à la note suivante par une ligature, ou couper la ligature entre elles (j)"
        )
      )(note => Edit.SetBeam(note.id, !note.beamed)),
      noteButton(
        Tr("+ after", "+ après"),
        Tr(
          "A note after it, of its length",
          "Une note après elle, de même durée"
        )
      )(note =>
        Edit.AddNote(
          note.id,
          s"a${js.Date.now().toLong}",
          note.step,
          note.alter,
          note.octave,
          chord = false
        )
      ),
      noteButton(
        Tr("+ chord", "+ accord"),
        Tr(
          "A note a third above it, in a chord",
          "Une note une tierce au-dessus, en accord"
        )
      )(note => {
        val index = steps.indexOf(note.step) + 2
        val step = steps(Math.floorMod(index, 7))
        Edit.AddNote(
          note.id,
          s"a${js.Date.now().toLong}",
          step,
          keyAlter(step, note.keyFifths),
          note.octave + Math.floorDiv(index, 7),
          chord = true
        )
      }),
      (0 to 5).map(finger =>
        noteButton(
          finger.toString,
          Tr(s"Fingering $finger", s"Doigté $finger")
        )(note => Edit.SetFingering(note.id, Some(finger.toString)))
      ),
      noteButton(
        Tr("no finger", "sans doigt"),
        Tr("No fingering", "Sans doigté")
      )(note => Edit.SetFingering(note.id, None)),
      noteButton("⊓", Tr("Down-bow", "Tiré"))(note =>
        Edit.SetBowing(
          note.id,
          Option.when(!note.bowing.contains("down-bow"))("down-bow")
        )
      ),
      noteButton("∨", Tr("Up-bow", "Poussé"))(note =>
        Edit.SetBowing(
          note.id,
          Option.when(!note.bowing.contains("up-bow"))("up-bow")
        )
      ),
      button(
        cls := "button is-small",
        title := Tr(
          "A slur from this note to the next one clicked",
          "Une liaison de cette note à la prochaine cliquée"
        ),
        Tr("Slur to…", "Liaison jusqu'à…"),
        onClick.compose(_.sample(selected)) --> (_.foreach(note =>
          slurFrom.set(Some(note.id))
        ))
      ),
      noteButton(
        Tr("No slur", "Sans liaison"),
        Tr(
          "Take away the slurs starting or ending here",
          "Retirer les liaisons qui commencent ou finissent ici"
        )
      )(note => Edit.RemoveSlurs(note.id)),
      div(
        cls := "select is-small",
        select(
          option(value := "", Tr("Dynamics…", "Nuance…")),
          dynamicsChoices.map(marking => option(value := marking, marking)),
          onChange.mapToValue.compose(_.withCurrentValueOf(selected)) --> {
            (marking, note) =>
              note
                .filter(_ => marking.nonEmpty)
                .foreach(n => apply(Edit.SetDynamics(n.id, Some(marking))))
          },
          value <-- selected.map(_ => "")
        )
      ),
      input(
        cls := "input is-small editor-name",
        placeholder := Tr("Words (Enter)", "Texte (Entrée)"),
        onKeyDown
          .filter(_.key == "Enter")
          .map(_.target.asInstanceOf[dom.html.Input])
          .compose(_.withCurrentValueOf(selected)) --> { (field, note) =>
          note
            .filter(_ => field.value.trim.nonEmpty)
            .foreach(n => apply(Edit.AddText(n.id, field.value.trim)))
          field.value = ""
        }
      )
    )
  )

  // A key does what its button does, but not while a field is typed in.
  private def typing(event: dom.KeyboardEvent): Boolean =
    event.target match {
      case element: dom.Element => element.matches("input, select, textarea")
      case _                    => false
    }

  private def keyed(event: dom.KeyboardEvent, note: NoteDetails): Option[Edit] =
    (event.key, event.shiftKey) match {
      case ("ArrowUp", false)   => Some(moved(note, 1))
      case ("ArrowDown", false) => Some(moved(note, -1))
      case ("ArrowUp", true)    => Some(octaveMoved(note, 1))
      case ("ArrowDown", true)  => Some(octaveMoved(note, -1))
      case ("#", _)             => Some(altered(note, 1))
      case ("b", false)         => Some(altered(note, -1))
      case ("n", false)         => Some(altered(note, 0))
      case ("t", false)         => Some(Edit.SetTie(note.id, !note.tied))
      case ("j", false)         => Some(Edit.SetBeam(note.id, !note.beamed))
      case ("Delete" | "Backspace", _) => Some(Edit.MakeRest(note.id))
      case (digit, false) if digit.length == 1 && "012345".contains(digit) =>
        Some(Edit.SetFingering(note.id, Some(digit)))
      case _ => None
    }

  private val keys = documentEvents(_.onKeyDown)
    .filter(event => !typing(event))
    .withCurrentValueOf(editing, selected) --> { (event, on, note) =>
    if (on) {
      if ((event.ctrlKey || event.metaKey) && event.key == "z") {
        event.preventDefault()
        bus.send(UndoEdit)
      } else if (event.key == "Escape") {
        slurFrom.set(None)
        bus.send(SelectNote(None))
      } else {
        note.flatMap(keyed(event, _)).foreach { edit =>
          event.preventDefault()
          apply(edit)
        }
      }
    }
  }

  // A note clicked while a slur waits for its end is that end.
  private val slurEnds = events
    .collect { case NoteSelected(Some(note)) => note }
    .withCurrentValueOf(slurFrom.signal) --> { (note, from) =>
    from.filter(_ != note.id).foreach { start =>
      slurFrom.set(None)
      apply(Edit.AddSlur(start, note.id))
    }
  }

  override lazy val node: HtmlElement = div(
    cls := "score-editor",
    hidden <-- editing.map(!_),
    div(
      cls := "editor-panel",
      hidden <-- editing.map(!_),
      editionsControl,
      noteControls
    ),
    keys,
    slurEnds
  )
}
