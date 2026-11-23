package ripieno.frontend.components

import ripieno.frontend.i18n.*
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.utils.RememberedSettings
import ripieno.shared.WorkDetail

import com.raquo.laminar.api.L.*

/** The tuner beside the sheet, while it listens: the note heard and how far it
  * is from the one in tune, at the A chosen; and the open strings of an
  * instrument, to be heard: by default the player's own, else the first of the
  * score with strings, else the one last chosen.
  */
class TunerPanel(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val on: Signal[Boolean] =
    events.collect { case TunerSet(now) => now }.startWith(false)

  private val reference: Signal[Double] = events
    .collect { case ReferencePitchSet(hertz) => hertz }
    .startWith(RememberedSettings.referencePitch)

  private val heard: Signal[Option[Double]] =
    events.collect { case PitchHeard(hertz) => hertz }.startWith(None)

  // the note nearest to what is heard, and how many cents off it
  private val note: Signal[Option[(Int, Double, Double)]] =
    heard.combineWith(reference).map { (hertz, a) =>
      hertz.map { f =>
        val exact = 69 + 12 * math.log(f / a) / math.log(2)
        val nearest = math.round(exact).toInt
        (nearest, (exact - nearest) * 100, f)
      }
    }

  private val references = List(415.0, 430.0, 435.0, 440.0, 441.0, 442.0, 443.0)

  private val work: Signal[Option[WorkDetail]] =
    events.collect { case WorkLoaded(found) => Some(found) }.startWith(None)

  private val myPart: Signal[Option[Int]] =
    events.collect { case PartChosen(part, _) => part }.startWith(None)

  // the score's instruments with strings, the player's own first
  private val present: Signal[List[String]] =
    work.combineWith(myPart).map { (found, mine) =>
      val parts = found.fold(Nil)(_.parts)
      val own = mine.flatMap(parts.lift).toList
      (own ++ parts)
        .flatMap(_.instrument)
        .filter(Tuning.of(_).isDefined)
        .distinct
    }

  // the player's choice holds until another work is opened
  private val chosen: Signal[Option[Option[String]]] = events
    .collect {
      case TunerInstrumentSet(instrument) => Some(instrument)
      case _: WorkLoaded                  => None
    }
    .startWith(None)

  private val instrument: Signal[Option[String]] =
    chosen.combineWith(present).map { (choice, inScore) =>
      choice.getOrElse(
        inScore.headOption.orElse(RememberedSettings.tunerInstrument)
      )
    }

  private val noInstrument = "-"

  private def instrumentChoice(inScore: List[String]): List[HtmlElement] = {
    val others = Tuning.openStrings.map(_._1).filterNot(inScore.contains)
    def each(kind: String) = option(value := kind, Instruments.name(kind))
    List(
      Option.when(inScore.nonEmpty)(
        optGroup(
          labelAttr := Tr("In this score", "Dans cette partition"),
          inScore.map(each)
        )
      ),
      Some(
        optGroup(
          labelAttr := Tr("Others", "Autres"),
          option(value := noInstrument, Tr("The A alone", "Le la seul")),
          others.map(each)
        )
      )
    ).flatten
  }

  private def inTune(cents: Double): String =
    if (math.abs(cents) <= TunerPanel.inTuneCents) "is-in-tune"
    else if (cents > 0) "is-sharp"
    else "is-flat"

  override lazy val node: HtmlElement = div(
    cls := "tuner-panel",
    // followed while the tuner is closed, so as to be there when it opens
    present --> (_ => ()),
    instrument --> (_ => ()),
    child.maybe <-- on.map(listening =>
      Option.when(listening)(
        div(
          cls := "tuner",
          div(
            cls := "review-header",
            span(cls := "review-heading", Tr("Tuner", "Accordeur")),
            button(
              cls := "delete",
              onClick --> (_ => bus.send(SetTuner(false)))
            )
          ),
          div(
            cls := "tuner-note",
            child.text <-- note.map(
              _.fold("–")((pitch, _, _) => NoteNames.ofMidi(pitch))
            )
          ),
          div(
            cls := "tuner-scale",
            div(cls := "tuner-centre"),
            div(
              cls := "tuner-needle",
              cls <-- note.map(
                _.fold("is-silent")((_, cents, _) => inTune(cents))
              ),
              styleAttr <-- note.map(found =>
                s"left: ${50 + found.fold(0.0)(_._2)}%"
              )
            )
          ),
          div(
            cls := "tuner-cents",
            child.text <-- note.map(
              _.fold(Tr("Play a note", "Jouez une note")) { (_, cents, hertz) =>
                val off =
                  if (math.abs(cents) <= TunerPanel.inTuneCents)
                    Tr("in tune", "juste")
                  else ReviewWords.cents(math.round(cents).toInt)
                f"$off · $hertz%.1f Hz"
              }
            )
          ),
          div(
            cls := "metronome-row",
            span(cls := "metronome-label", s"${NoteNames.step("A")} ="),
            div(
              cls := "select is-small",
              select(
                idAttr := "reference-pitch",
                references.map(hertz =>
                  option(value := hertz.toString, f"$hertz%.0f Hz")
                ),
                value <-- reference.map(_.toString),
                onChange.mapToValue.map(_.toDoubleOption) --> (
                  _.foreach(hertz => bus.send(SetReferencePitch(hertz)))
                )
              )
            )
          ),
          div(
            cls := "metronome-row",
            span(cls := "metronome-label", Tr("Strings of", "Cordes de")),
            div(
              cls := "select is-small",
              select(
                idAttr := "tuner-instrument",
                children <-- present.map(instrumentChoice),
                value <-- instrument.map(_.getOrElse(noInstrument)),
                onChange.mapToValue --> (kind =>
                  bus.send(
                    SetTunerInstrument(Option.when(kind != noInstrument)(kind))
                  )
                )
              )
            )
          ),
          div(
            cls := "metronome-row",
            span(cls := "metronome-label", Tr("Hear", "Entendre")),
            children <-- instrument
              .map(
                _.flatMap(Tuning.of).getOrElse(Tuning.reference)
              )
              .map(
                _.map(pitch =>
                  button(
                    cls := "button is-small",
                    NoteNames.ofMidiClass(pitch),
                    title := Tr(
                      s"Sounds ${NoteNames.ofMidi(pitch)} for a few seconds",
                      s"Fait entendre ${NoteNames.ofMidi(pitch)} quelques secondes"
                    ),
                    onClick --> (_ => bus.send(PlayReferenceNote(pitch)))
                  )
                )
              )
          )
        )
      )
    )
  )
}

object TunerPanel {
  // this close, a note is in tune
  val inTuneCents = 5.0
}
