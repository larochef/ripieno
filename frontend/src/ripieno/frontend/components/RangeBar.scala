package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*

import com.raquo.laminar.api.L.*

/** What to do with the notes selected on the sheet (Shift-click): hear them as
  * written, or as the recording opened played them, with the beats or not.
  */
class RangeBar(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val selected: Signal[Option[ScoreRange]] =
    events.collect { case RangeSelected(range) => range }.startWith(None)

  private val recordingOpen: Signal[Boolean] = events
    .collect {
      case _: PerformanceReviewed => true
      case _: ScoreLoading        => false
    }
    .startWith(false)

  private val beats: Var[Boolean] = Var(false)

  private def measures(range: ScoreRange): String =
    if (range.firstMeasure == range.lastMeasure) {
      Tr(s"measure ${range.firstMeasure}", s"mesure ${range.firstMeasure}")
    } else {
      Tr(
        s"measures ${range.firstMeasure} to ${range.lastMeasure}",
        s"mesures ${range.firstMeasure} à ${range.lastMeasure}"
      )
    }

  private val noScore: Signal[Boolean] =
    events.collect { case _: ScoreOpened => false }.startWith(true)

  private val controls: HtmlElement = span(
    cls := "range-controls",
    hidden <-- selected.map(_.isEmpty),
    span(cls := "part-choice-label", Tr("Selection", "Sélection")),
    span(child.text <-- selected.map(_.fold("")(measures))),
    button(
      cls := "button is-small is-link",
      title := Tr(
        "The notes selected as the score writes them; with a recording open, at the pace you took over them",
        "Les notes sélectionnées telles que la partition les écrit ; avec un enregistrement ouvert, à l'allure que vous y avez prise"
      ),
      Tr("▶ As written", "▶ Comme écrit"),
      onClick.compose(_.sample(beats.signal)) --> (on =>
        bus.send(PlayRange(asPlayed = false, on))
      )
    ),
    button(
      cls := "button is-small is-link",
      disabled <-- recordingOpen.map(!_),
      title <-- recordingOpen.map(open =>
        if (open) {
          Tr(
            "The same notes in the recording opened",
            "Les mêmes notes dans l'enregistrement ouvert"
          )
        } else {
          Tr(
            "Open a recording to hear how you played these notes",
            "Ouvrez un enregistrement pour entendre comment vous avez joué ces notes"
          )
        }
      ),
      Tr("▶ As you played it", "▶ Comme vous l'avez joué"),
      onClick.compose(_.sample(beats.signal)) --> (on =>
        bus.send(PlayRange(asPlayed = true, on))
      )
    ),
    label(
      cls := "checkbox",
      title := Tr(
        "The metronome's beats over the notes: the score's as written, steady ones at your own pace as you played them",
        "Les temps du métronome sur les notes : ceux de la partition comme écrit, réguliers à votre propre allure comme vous l'avez joué"
      ),
      input(
        typ := "checkbox",
        controlled(checked <-- beats.signal, onClick.mapToChecked --> beats)
      ),
      Tr(" Beats", " Temps")
    ),
    button(
      cls := "delete",
      title := Tr("Clear the selection", "Effacer la sélection"),
      onClick --> (_ => bus.send(ClearRange))
    )
  )

  override lazy val node: HtmlElement = div(
    cls := "range-bar",
    hidden <-- noScore,
    controls,
    span(
      cls := "range-hint",
      hidden <-- selected.map(_.isDefined),
      Tr(
        "Shift-click two notes to select what is between them, and hear it",
        "Maj-clic sur deux notes pour sélectionner ce qui est entre elles, et l'entendre"
      )
    )
  )
}
