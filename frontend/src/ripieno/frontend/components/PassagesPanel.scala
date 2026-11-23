package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.ScoreRepeat
import ripieno.shared.ScoreCut

import com.raquo.laminar.api.L.*

/** What is played of the score beside the sheet: its repeats, each played or
  * left out, and the cuts it allows, each made or not.
  */
class PassagesPanel(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val shown: Signal[Boolean] = events
    .collect {
      case PassagesShown(on) => on
      case _: ScoreLoading   => false
    }
    .startWith(false)

  private val repeats: Signal[List[(ScoreRepeat, Boolean)]] = events
    .collect {
      case RepeatsShown(now) => now
      case _: ScoreOpened    => Nil
    }
    .startWith(Nil)

  private val cuts: Signal[List[(ScoreCut, Boolean)]] = events
    .collect {
      case CutsShown(now) => now
      case _: ScoreOpened => Nil
    }
    .startWith(Nil)

  private def measures(from: String, to: String): String =
    if (from == to) Tr(s"m. $from", s"mes. $from")
    else Tr(s"m. $from–$to", s"mes. $from–$to")

  private def repeatRow(repeat: ScoreRepeat, played: Boolean): HtmlElement =
    label(
      cls := "checkbox passage-row",
      input(
        typ := "checkbox",
        checked := played,
        onChange.mapToChecked --> (on => bus.send(SetRepeat(repeat.index, on)))
      ),
      span(
        Tr("Repeat, ", "Reprise, ") + measures(repeat.from, repeat.to),
        repeat.firstEnding.fold(emptyNode)((from, to) =>
          span(
            cls := "passage-note",
            Tr(" · first ending ", " · 1re fois ") + measures(from, to)
          )
        )
      )
    )

  private def cutRow(cut: ScoreCut, made: Boolean): HtmlElement = label(
    cls := "checkbox passage-row",
    input(
      typ := "checkbox",
      checked := made,
      onChange.mapToChecked --> (on => bus.send(SetCut(cut.index, on)))
    ),
    span(
      Tr("Cut, ", "Coupure, ") + measures(cut.from, cut.to),
      cut.label.fold(emptyNode)(words =>
        span(cls := "passage-note", s" · $words")
      )
    )
  )

  private val repeatsSection: HtmlElement = div(
    cls := "sounds-section",
    hidden <-- repeats.map(_.isEmpty),
    span(cls := "sounds-title", Tr("Repeats played", "Reprises jouées")),
    div(
      cls := "passage-rows",
      children <-- repeats.map(_.map(repeatRow.tupled))
    ),
    span(
      cls := "sounds-note",
      Tr(
        "A repeat left out goes on at once to what follows it, past its first ending.",
        "Une reprise non jouée enchaîne directement sur la suite, sans sa 1re fois."
      )
    )
  )

  private val cutsSection: HtmlElement = div(
    cls := "sounds-section",
    hidden <-- cuts.map(_.isEmpty),
    span(cls := "sounds-title", Tr("Cuts made", "Coupures faites")),
    div(
      cls := "passage-rows",
      children <-- cuts.map(_.map(cutRow.tupled))
    ),
    span(
      cls := "sounds-note",
      Tr(
        "The passages the edition allows to cut, between two ⊕ signs.",
        "Les passages que l'édition permet de couper, entre deux signes ⊕."
      )
    )
  )

  override lazy val node: HtmlElement = div(
    cls := "passages-panel",
    // followed while the drawer is closed, so as to be there when it opens
    repeats --> (_ => ()),
    cuts --> (_ => ()),
    child.maybe <-- shown.map(on =>
      Option.when(on)(
        div(
          cls := "passages",
          div(
            cls := "review-header",
            span(
              cls := "review-heading",
              Tr("Cuts and repeats", "Coupures et reprises")
            ),
            button(
              cls := "delete",
              aria.label := Tr("Close", "Fermer"),
              onClick --> (_ => bus.send(ShowPassages(false)))
            )
          ),
          repeatsSection,
          cutsSection,
          span(
            cls := "sounds-note",
            hidden <-- repeats
              .combineWith(cuts)
              .map((some, others) => some.nonEmpty || others.nonEmpty),
            Tr(
              "This score has no repeat and no cut.",
              "Cette partition n'a ni reprise ni coupure."
            )
          )
        )
      )
    )
  )
}
