package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.ScorePart

import com.raquo.laminar.api.L.*

/** The measures to check beside the sheet: each one shown on the sheet when
  * clicked, to be corrected with the pencil or said to be right.
  */
class ChecksPanel(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val shown: Signal[Boolean] = events
    .collect {
      case ChecksShown(on) => on
      case _: ScoreLoading => false
    }
    .startWith(false)

  private val suspects: Signal[List[SuspectMeasure]] = events
    .collect {
      case MeasuresSuspected(found) => found
      case _: ScoreLoading          => Nil
    }
    .startWith(Nil)

  private val parts: Signal[List[ScorePart]] =
    events.collect { case opened: ScoreOpened => opened.parts }.startWith(Nil)

  private def show(suspect: SuspectMeasure): Unit = {
    bus.send(PlayFromMeasure(suspect.measure))
    bus.send(ShowSuspect(Some(suspect)))
  }

  private def row(
      suspect: SuspectMeasure,
      names: List[ScorePart]
  ): HtmlElement =
    div(
      cls := "check-row",
      button(
        cls := "check-place",
        title := Tr("Show it on the sheet", "La montrer sur la partition"),
        span(
          cls := "check-where",
          (names.lift(suspect.part).map(_.name).toList :+ Tr(
            s"measure ${suspect.measure}",
            s"mesure ${suspect.measure}"
          )).mkString(" · ")
        ),
        span(cls := "check-detail", suspect.detail),
        onClick --> (_ => show(suspect))
      ),
      button(
        cls := "button is-small is-text check-right",
        title := Tr(
          "It is right as it is written: don't flag it again",
          "C'est juste tel quel : ne plus la signaler"
        ),
        Tr("Right", "C'est juste"),
        onClick --> (_ => bus.send(DismissSuspect(suspect)))
      )
    )

  override lazy val node: HtmlElement = div(
    cls := "checks-panel",
    // followed while the panel is closed, so as to be there when it opens
    suspects --> (_ => ()),
    parts --> (_ => ()),
    child.maybe <-- shown.map(on =>
      Option.when(on)(
        div(
          cls := "checks",
          div(
            cls := "review-header",
            span(cls := "review-heading", Tr("To check", "À vérifier")),
            button(
              cls := "delete",
              aria.label := Tr("Close", "Fermer"),
              onClick --> (_ => bus.send(ShowChecks(false)))
            )
          ),
          p(
            cls := "dialog-note",
            Tr(
              "Measures whose notes don't add up to the time signature, and ties between different notes. Correct them with the pencil, or say they are right.",
              "Des mesures dont les notes ne font pas le compte de la mesure indiquée, et des liaisons de tenue entre notes différentes. Corrigez-les avec le crayon, ou indiquez qu'elles sont justes."
            )
          ),
          children <-- suspects
            .combineWith(parts)
            .map((found, names) => found.map(row(_, names)))
        )
      )
    )
  )
}
