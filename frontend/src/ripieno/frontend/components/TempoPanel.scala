package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*

import com.raquo.laminar.api.L.*
import org.scalajs.dom.html

/** The tempi beside the sheet, all in view: the score's, or each of its
  * sections' with the one playing marked, each against the tempo written.
  */
class TempoPanel(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val shown: Signal[Boolean] = events
    .collect {
      case TempiShown(on)  => on
      case _: ScoreLoading => false
    }
    .startWith(false)

  private val written: Signal[Option[Tempo]] = events
    .collect { case opened: ScoreOpened => Some(opened.tempo) }
    .startWith(None)

  private val chosen: Signal[Int] =
    events
      .collect { case TempoSet(beatsPerMinute) => beatsPerMinute }
      .startWith(0)

  private val sections: Signal[List[SectionTempo]] = events
    .collect {
      case SectionsShown(now) => now
      case _: ScoreOpened     => Nil
    }
    .startWith(Nil)

  private val playing: Signal[Option[Int]] =
    events
      .collect { case SectionShown(now) => now.map(_.index) }
      .startWith(None)

  // Tempi typed or stepped to but not yet set: the score is timed again (and
  // its parts rendered again) once they stop changing, not at each step.
  private val pending: Var[Map[Int, Int]] = Var(Map.empty)
  private val edited: EventBus[Unit] = EventBus()
  private val settlesMillis = 800

  private val sectionCountIn: Signal[Int] =
    events.collect { case SectionCountInSet(measures) => measures }.startWith(0)

  private val step = 4

  // Its field is given the tempo back even when it is the one already shown,
  // so as to undo what was typed beyond the limits.
  private def tempoRow(
      name: String,
      beat: Beat,
      reference: Option[Int],
      current: Signal[Int],
      isPlaying: Signal[Boolean],
      set: Int => Int
  ): HtmlElement = {
    def stepButton(sign: String, label: String, change: Int) = button(
      cls := "button is-small tempo-step",
      aria.label := label,
      sign,
      onClick.compose(_.sample(current)) --> (now => set(now + change))
    )
    div(
      cls := "tempo-row",
      cls("is-playing") <-- isPlaying,
      span(cls := "tempo-name", title := name, name),
      span(cls := "tempo-beat", s"${beat.symbol} ="),
      stepButton("−", Tr("Slower", "Plus lent"), -step),
      input(
        cls := "input is-small tempo-value",
        typ := "number",
        aria.label := Tr(s"Tempo of $name", s"Tempo de $name"),
        value <-- current.map(_.toString),
        inContext(field =>
          onChange.mapToValue.compose(_.withCurrentValueOf(current)) --> {
            (typed, now) =>
              val kept = set(typed.trim.toIntOption.getOrElse(now))
              field.ref.asInstanceOf[html.Input].value = kept.toString
          }
        )
      ),
      stepButton("+", Tr("Faster", "Plus vite"), step),
      reference.fold(
        span(cls := "tempo-written", Tr("not written", "non écrit"))
      )(tempo =>
        button(
          cls := "tempo-written tempo-back",
          title := Tr("Back to the written tempo", "Revenir au tempo écrit"),
          disabled <-- current.map(_ == tempo),
          Tr(s"written $tempo", s"écrit $tempo"),
          onClick --> (_ => set(tempo))
        )
      )
    )
  }

  private def setSection(section: SectionTempo)(beatsPerMinute: Int): Int = {
    val kept = section.within(beatsPerMinute)
    pending.update(_.updated(section.index, kept))
    edited.emit(())
    kept
  }

  // built again only when the sections change, not as playback moves on:
  // what is being typed in keeps the focus
  private def sectionRows(all: List[SectionTempo]): List[HtmlElement] =
    all.map { section =>
      val name = section.name.getOrElse(
        Tr(s"Part ${section.index + 1}", s"Partie ${section.index + 1}")
      )
      tempoRow(
        if (all.sizeIs == 1) Tr("Whole score", "Toute la partition")
        else
          Tr(
            s"$name, m. ${section.measure}",
            s"$name, mes. ${section.measure}"
          ),
        section.beat,
        section.written,
        pending.signal.map(_.getOrElse(section.index, section.chosen)),
        playing.map(_.contains(section.index)),
        setSection(section)
      )
    }

  private val everyWritten: HtmlElement = button(
    cls := "button is-small",
    disabled <-- sections
      .combineWith(pending.signal)
      .map((all, typed) =>
        all.forall(section =>
          section.written.forall(
            typed.getOrElse(section.index, section.chosen) == _
          )
        )
      ),
    Tr("All at the written tempi", "Tout aux tempos écrits"),
    onClick.compose(_.sample(sections)) --> (all =>
      all.foreach(section => section.written.foreach(setSection(section)))
    )
  )

  private val sectionCountInSelect: HtmlElement = div(
    cls := "select is-small",
    select(
      aria.label := Tr(
        "Count-in at each new tempo",
        "Décompte à chaque changement de tempo"
      ),
      option(value := "0", Tr("No count", "Sans décompte")),
      option(value := "1", Tr("Count 1 bar", "Décompte d'une mesure")),
      option(value := "2", Tr("Count 2 bars", "Décompte de 2 mesures")),
      controlled(
        value <-- sectionCountIn.map(_.toString),
        onChange.mapToValue.map(_.toInt) --> (measures =>
          bus.send(SetSectionCountIn(measures))
        )
      )
    )
  )

  private val sectionsView: HtmlElement = div(
    div(
      cls := "tempo-rows",
      children <-- sections.map(sectionRows)
    ),
    div(
      cls := "tempo-actions",
      hidden <-- sections.map(all =>
        all.sizeIs < 2 || all.forall(_.written.isEmpty)
      ),
      everyWritten
    ),
    div(
      cls := "sounds-section",
      hidden <-- sections.map(_.sizeIs < 2),
      span(
        cls := "sounds-title",
        Tr("Where a new tempo starts", "À chaque changement de tempo")
      ),
      sectionCountInSelect,
      span(
        cls := "sounds-note",
        Tr(
          "Playback stops there and the metronome counts in the new tempo.",
          "La lecture s'y arrête et le métronome compte le nouveau tempo."
        )
      )
    ),
    span(
      cls := "sounds-note",
      Tr(
        "The tempi chosen are kept with the score.",
        "Les tempos choisis sont gardés avec la partition."
      )
    )
  )

  private val wholeView: HtmlElement = div(
    child.maybe <-- written.map(
      _.map(tempo =>
        tempoRow(
          Tr("Whole score", "Toute la partition"),
          tempo.beat,
          Some(tempo.beatsPerMinute),
          chosen,
          Val(false),
          typed => {
            val kept = Tempo.within(tempo.beatsPerMinute, typed)
            bus.send(SetTempo(kept))
            kept
          }
        )
      )
    )
  )

  override lazy val node: HtmlElement = div(
    cls := "tempo-panel",
    // followed while the drawer is closed, so as to be there when it opens
    written --> (_ => ()),
    chosen --> (_ => ()),
    sectionCountIn --> (_ => ()),
    playing --> (_ => ()),
    sections.changes --> (all =>
      // what is now set needs setting no more
      pending.update(
        _.filter((index, typed) =>
          !all.exists(section =>
            section.index == index && section.chosen == typed
          )
        )
      )
    ),
    edited.events.debounce(settlesMillis) --> (_ =>
      bus.send(SetSectionTempi(pending.now()))
    ),
    child.maybe <-- shown.map(on =>
      Option.when(on)(
        div(
          cls := "tempi",
          div(
            cls := "review-header",
            span(cls := "review-heading", Tr("Tempo", "Tempo")),
            button(
              cls := "delete",
              aria.label := Tr("Close", "Fermer"),
              onClick --> (_ => bus.send(ShowTempi(false)))
            )
          ),
          // a score with sections is timed by them, even by one alone
          child <-- sections
            .map(_.nonEmpty)
            .distinct
            .map(
              if (_) sectionsView else wholeView
            )
        )
      )
    )
  )
}
