package ripieno.frontend.components

import ripieno.frontend.i18n.{Instruments, Tr}
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.utils.RememberedSettings
import ripieno.shared.ScorePart

import com.raquo.laminar.api.L.*

/** The settings of the sound beside the sheet: how the player hears the parts,
  * and the microphone's level.
  */
class SoundsPanel(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val shown: Signal[Boolean] =
    events.collect { case SoundsShown(on) => on }.startWith(false)

  private val parts: Signal[List[ScorePart]] =
    events.collect { case opened: ScoreOpened => opened.parts }.startWith(Nil)

  private val mixed: Signal[SoundMixed] = events
    .collect { case now: SoundMixed => now }
    .startWith(SoundMixed(SoundMix(Vector.empty), Map.empty))

  private val heard: Signal[Set[Int]] =
    events.collect { case PartsPlayed(parts) => parts }.startWith(Set.empty)

  // One line an instrument: the engine all its parts sound with, in every
  // score.
  private def instrumentRow(
      instrument: Option[String],
      engine: SoundEngine
  ): HtmlElement = {
    val name =
      instrument.fold(Tr("Other parts", "Autres parties"))(Instruments.name)
    div(
      cls := "sound-row",
      span(cls := "sound-part", name),
      div(
        cls := "select is-small",
        select(
          aria.label := Tr(s"$name's sound", s"Son de $name"),
          SoundEngine.values.toSeq.map(each =>
            option(
              value := each.toString,
              selected := each == engine,
              each.label
            )
          ),
          onChange.mapToValue.map(SoundEngine.valueOf) --> (each =>
            bus.send(UseInstrumentSound(instrument, each))
          )
        )
      )
    )
  }

  // One line a part: its level, and how its rendering goes.
  private def partRow(
      part: ScorePart,
      index: Int,
      sound: PartSound,
      waiting: Option[Option[Int]],
      isHeard: Boolean
  ): HtmlElement = div(
    cls := "sound-row",
    span(
      cls := "sound-part",
      cls("is-silent") := !isHeard,
      title := (if (isHeard) part.name
                else
                  Tr(
                    s"${part.name}: not heard",
                    s"${part.name} : non entendue"
                  )),
      part.name
    ),
    input(
      typ := "range",
      cls := "sound-level",
      aria.label := Tr(s"${part.name}'s level", s"Volume de ${part.name}"),
      minAttr := "0",
      maxAttr := "100",
      value := sound.level.toString,
      onChange.mapToValue.map(_.toIntOption.getOrElse(100)) --> (level =>
        bus.send(SetPartLevel(index, level))
      )
    ),
    span(
      cls := "sound-state",
      waiting.fold(if (isHeard) "" else Tr("not heard", "non entendue"))(
        _.fold(Tr("waiting", "en attente"))(percent => s"$percent %")
      )
    )
  )

  private val listening: Signal[Listening] = events
    .collect { case ListeningSet(now) => now }
    .startWith(RememberedSettings.listening)

  private val checking: Signal[Boolean] = events
    .collect {
      case _: LevelChecking => true
      case _: LevelChecked  => false
    }
    .startWith(false)

  private val outcome: Signal[String] = events
    .collect {
      case LevelChecking(_) =>
        Tr(
          "Play your loudest passage now…",
          "Jouez maintenant votre passage le plus fort…"
        )
      case LevelChecked(outcome) => outcome
    }
    .startWith("")

  override lazy val node: HtmlElement = div(
    cls := "sounds-panel",
    // followed while the drawer is closed, so as to be there when it opens
    parts --> (_ => ()),
    mixed --> (_ => ()),
    heard --> (_ => ()),
    child.maybe <-- shown.map(on =>
      Option.when(on)(
        div(
          cls := "sounds",
          div(
            cls := "review-header",
            span(cls := "review-heading", Tr("Sounds", "Sons")),
            button(
              cls := "delete",
              aria.label := Tr("Close", "Fermer"),
              onClick --> (_ => bus.send(ShowSounds(false)))
            )
          ),
          div(
            cls := "sounds-section",
            span(
              cls := "sounds-title",
              Tr("Each instrument", "Chaque instrument")
            ),
            div(
              cls := "sound-rows",
              children <-- mixed.map(now =>
                now.mix.instruments.toList.map(instrument =>
                  instrumentRow(
                    instrument,
                    now.mix.parts
                      .find(_.instrument == instrument)
                      .fold(SoundEngine.Sampled)(_.engine)
                  )
                )
              )
            ),
            span(
              cls := "sounds-note",
              Tr(
                "An instrument sounds the same in every score.",
                "Un instrument sonne de même dans toutes les partitions."
              )
            )
          ),
          div(
            cls := "sounds-section",
            span(cls := "sounds-title", Tr("Each part", "Chaque partie")),
            div(
              cls := "sound-rows",
              children <-- parts
                .combineWith(mixed, heard)
                .map((all, now, played) =>
                  all.zipWithIndex.flatMap((part, index) =>
                    now.mix.parts
                      .lift(index)
                      .map(sound =>
                        partRow(
                          part,
                          index,
                          sound,
                          now.rendering.get(index),
                          played.contains(index)
                        )
                      )
                  )
                )
            ),
            span(
              cls := "sounds-note",
              Tr(
                "Sampled is there at once; the others are rendered on this machine first, the sound in use playing meanwhile. Levels are heard at once.",
                "L'échantillonné est là tout de suite ; les autres sont d'abord rendus sur cette machine, le son actuel jouant en attendant. Les volumes s'entendent tout de suite."
              )
            )
          ),
          div(
            cls := "sounds-section",
            span(
              cls := "sounds-title",
              Tr("You listen on", "Vous écoutez sur")
            ),
            Listening.values.toSeq.map(each =>
              label(
                cls := "radio sounds-choice",
                input(
                  typ := "radio",
                  nameAttr := "listening",
                  idAttr := s"listening-${each.toString.toLowerCase}",
                  checked <-- listening.map(_ == each),
                  onChange --> (_ => bus.send(SetListening(each)))
                ),
                s" ${each.label}"
              )
            ),
            span(
              cls := "sounds-note",
              Tr(
                "On speakers, the microphone hears the parts too: the review takes them out, but headphones are surer.",
                "Sur haut-parleurs, le micro entend aussi les parties : l'analyse les retire, mais le casque est plus sûr."
              )
            )
          ),
          div(
            cls := "sounds-section",
            span(cls := "sounds-title", Tr("Microphone", "Micro")),
            button(
              cls := "button is-small",
              disabled <-- checking,
              title := Tr(
                "Play your loudest passage for six seconds: the microphone's input volume is set from it",
                "Jouez votre passage le plus fort pendant six secondes : le volume d'entrée du micro est réglé d'après lui"
              ),
              Tr("Set its level", "Régler son niveau"),
              onClick --> (_ => bus.send(CheckLevel))
            ),
            span(cls := "sounds-note", child.text <-- outcome)
          )
        )
      )
    )
  )
}
