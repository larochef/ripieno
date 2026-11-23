package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*

import com.raquo.laminar.api.L.*

/** A pencil over the score's corner, to write on it: corrections, fingerings,
  * slurs, in editions over it.
  */
class AnnotateButton(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val editing: Signal[Boolean] =
    events.collect { case EditingSet(on) => on }.startWith(false)

  private val noScore: Signal[Boolean] =
    events.collect { case _: ScoreOpened => false }.startWith(true)

  override lazy val node: HtmlElement = button(
    cls := "annotate-button",
    idAttr := "annotate",
    cls("is-active") <-- editing,
    hidden <-- noScore,
    aria.label <-- editing.map(
      if (_) Tr("Done annotating", "Fin des annotations")
      else Tr("Annotate", "Annoter")
    ),
    title <-- editing.map(
      if (_) Tr("Done annotating", "Fin des annotations")
      else
        Tr(
          "Annotate: corrections, fingerings, slurs… in editions over the score",
          "Annoter : corrections, doigtés, liaisons… dans des versions superposées à la partition"
        )
    ),
    child <-- editing.map(on =>
      if (on) span(cls := "annotate-done", Tr("✓ Done", "✓ Terminé"))
      else
        svg.svg(
          svg.width := "20",
          svg.height := "20",
          svg.viewBox := "0 0 24 24",
          svg.fill := "none",
          svg.stroke := "currentColor",
          svg.strokeWidth := "1.8",
          svg.strokeLineCap := "round",
          svg.strokeLineJoin := "round",
          aria.hidden := true,
          svg.path(svg.d := "M12 20h9"),
          svg.path(
            svg.d := "M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4 12.5-12.5z"
          )
        )
    ),
    onClick.compose(_.sample(editing)) --> (on => bus.send(SetEditing(!on)))
  )
}
