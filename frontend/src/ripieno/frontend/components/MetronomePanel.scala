package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.utils.RememberedSettings

import com.raquo.laminar.api.L.*

/** The metronome's settings beside the sheet, while it beats: its sound, its
  * volume, the stressed first beat, and its tempo when no score is open.
  */
class MetronomePanel(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val on: Signal[Boolean] =
    events.collect { case MetronomeSet(now) => now }.startWith(false)

  private val settings: Signal[MetronomeSettings] = events
    .collect { case MetronomeConfigured(now) => now }
    .startWith(RememberedSettings.metronome)

  // what it follows: the score's tempo, or its own with no score
  private val written: Signal[Option[Tempo]] =
    events
      .collect { case opened: ScoreOpened => Some(opened.tempo) }
      .startWith(None)

  private val chosen: Signal[Int] =
    events
      .collect { case TempoSet(beatsPerMinute) => beatsPerMinute }
      .startWith(0)

  private val followed: Signal[Option[(Tempo, Int)]] =
    written
      .combineWith(chosen)
      .map((tempo, beatsPerMinute) => tempo.map((_, beatsPerMinute)))

  override lazy val node: HtmlElement = div(
    cls := "metronome-panel",
    // the score's tempo is followed even while the panel is closed
    child.maybe <-- on
      .combineWith(followed)
      .map((beating, tempo) =>
        Option.when(beating)(
          div(
            cls := "metronome",
            div(
              cls := "review-header",
              span(cls := "review-heading", Tr("Metronome", "Métronome")),
              button(
                cls := "delete",
                onClick --> (_ => bus.send(SetMetronome(false)))
              )
            ),
            div(
              cls := "metronome-tempo",
              child <-- settings.map(current =>
                tempo match {
                  case Some((written, beatsPerMinute)) =>
                    span(
                      Tr(
                        s"At the score's tempo, ${written.beat.symbol} = $beatsPerMinute, in the time of the measure where it stands",
                        s"Au tempo de la partition, ${written.beat.symbol} = $beatsPerMinute, dans la mesure où se trouve la lecture"
                      )
                    )
                  case None =>
                    span(
                      "♩ = ",
                      input(
                        cls := "input is-small metronome-bpm",
                        typ := "number",
                        minAttr := "20",
                        maxAttr := "300",
                        value := current.beatsPerMinute.toString,
                        onChange.mapToValue
                          .map(_.toIntOption)
                          .collect {
                            case Some(bpm) if bpm >= 20 && bpm <= 300 =>
                              current.copy(beatsPerMinute = bpm)
                          } --> (now => bus.send(ConfigureMetronome(now)))
                      ),
                      Tr(
                        ", in 4/4: open a score to follow its own",
                        ", à 4/4 : ouvrez une partition pour suivre le sien"
                      )
                    )
                }
              )
            ),
            div(
              cls := "metronome-row",
              span(cls := "metronome-label", Tr("Sound", "Son")),
              div(
                cls := "select is-small",
                select(
                  idAttr := "metronome-sound",
                  MetronomeSound.values.toSeq
                    .map(sound => option(value := sound.toString, sound.label)),
                  value <-- settings.map(_.sound.toString),
                  onChange.mapToValue
                    .map(MetronomeSound.valueOf)
                    .compose(
                      _.withCurrentValueOf(settings)
                        .map((sound, current) => current.copy(sound = sound))
                    ) -->
                    (now => bus.send(ConfigureMetronome(now)))
                )
              )
            ),
            div(
              cls := "metronome-row",
              span(cls := "metronome-label", Tr("Volume", "Volume")),
              input(
                idAttr := "metronome-volume",
                typ := "range",
                minAttr := "0",
                maxAttr := "100",
                value <-- settings.map(_.volume.toString),
                onChange.mapToValue
                  .map(_.toIntOption.getOrElse(70))
                  .compose(
                    _.withCurrentValueOf(settings)
                      .map((volume, current) => current.copy(volume = volume))
                  ) -->
                  (now => bus.send(ConfigureMetronome(now)))
              )
            ),
            label(
              cls := "checkbox metronome-row",
              input(
                idAttr := "metronome-accent",
                typ := "checkbox",
                checked <-- settings.map(_.accent),
                onChange.mapToChecked
                  .compose(
                    _.withCurrentValueOf(settings)
                      .map((accent, current) => current.copy(accent = accent))
                  ) -->
                  (now => bus.send(ConfigureMetronome(now)))
              ),
              Tr(
                " First beat of each measure stressed",
                " Premier temps de chaque mesure accentué"
              )
            )
          )
        )
      )
  )
}
