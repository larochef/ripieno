package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.ScorePart

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Below the score, as on a player: where it is, play, record, the tempo and
  * the count-in, and a line saying what happens.
  */
class PlayerBar(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val noScore: Signal[Boolean] =
    events.collect { case _: ScoreOpened => false }.startWith(true)

  // the score's own sound being made ready: play starts once it is
  private val soundPreparing: Signal[Boolean] = events
    .collect {
      case SoundsLoading                        => true
      case SoundsReady                          => false
      case RequestFailed(TogglePlayback, _)     => false
      case _: ScoreLoading | _: ScoreUnreadable => false
    }
    .startWith(false)

  private val playing: Signal[Boolean] = events
    .collect {
      case PlaybackStarted                  => true
      case PlaybackPaused | PlaybackStopped => false
    }
    .startWith(false)

  private val elapsedSeconds: Signal[Double] = events
    .collect {
      case PlaybackMoved(seconds, _, _) => seconds
      case PlaybackStopped              => 0.0
    }
    .startWith(0.0)

  // how long the score shown lasts, at its written tempo
  private val lengthSeconds: Signal[Double] = events
    .collect {
      case shown: ScoreShown     => shown.engraving.durationMillis / 1000
      case retimed: ScoreRetimed => retimed.engraving.durationMillis / 1000
    }
    .startWith(0.0)

  // in a score whose sections each have their own tempo, the one where
  // playback stands: the tempo shown is its own
  private val section: Signal[Option[SectionTempo]] =
    events.collect { case SectionShown(now) => now }.startWith(None)

  private val wholeTempo: Signal[Option[Tempo]] = events
    .collect { case opened: ScoreOpened => Some(opened.tempo) }
    .startWith(None)

  private val tempiShown: Signal[Boolean] = events
    .collect {
      case TempiShown(on)  => on
      case _: ScoreLoading => false
    }
    .startWith(false)

  private val wholeChosen: Signal[Int] =
    events
      .collect { case TempoSet(beatsPerMinute) => beatsPerMinute }
      .startWith(0)

  // "Allegro ♩ = 72 (written 86)"
  // every section at a tempo of its own, in order; none for a score at one
  // tempo throughout
  private val sections: Signal[List[SectionTempo]] = events
    .collect {
      case SectionsShown(now) => now
      case _: ScoreOpened     => Nil
    }
    .startWith(Nil)

  // "♩ = 72 (written 86)"
  private def tempoText(
      beat: Option[Beat],
      playing: Int,
      written: Option[Int]
  ): String = {
    val against = written
      .filter(_ != playing)
      .fold("")(at => Tr(s" (written $at)", s" (écrit $at)"))
    beat.fold("")(_.symbol + " = ") + s"$playing$against"
  }

  // Each section's tempo one after the other, the one playing marked; the
  // score's own for a score at one tempo.
  private val tempiShownInBar: Signal[List[HtmlElement]] =
    sections
      .combineWith(section, wholeTempo, wholeChosen)
      .map { (all, now, whole, chosen) =>
        if (all.isEmpty) {
          List(
            span(
              tempoText(
                whole.map(_.beat),
                chosen,
                whole.map(_.beatsPerMinute)
              )
            )
          )
        } else {
          all.zipWithIndex.flatMap { (each, index) =>
            val shown = now.filter(_.index == each.index).getOrElse(each)
            Option.when(index > 0)(span(cls := "player-tempo-gap", "·")) ++
              List(
                span(
                  cls := "player-tempo-section",
                  cls("is-playing") := now.exists(_.index == each.index),
                  shown.name.fold("")(_ + " ") +
                    tempoText(Some(shown.beat), shown.chosen, shown.written)
                )
              )
          }
        }
      }

  private val measure: EventStream[String] =
    events.collect { case PlaybackMoved(_, _, measure) => measure }.distinct

  private val countIn: Signal[Int] =
    events.collect { case CountInSet(measures) => measures }.startWith(0)

  private val parts: Signal[List[ScorePart]] =
    events.collect { case opened: ScoreOpened => opened.parts }.startWith(Nil)

  private val heard: Signal[Set[Int]] =
    events.collect { case PartsPlayed(parts) => parts }.startWith(Set.empty)

  private val chosen: Signal[Option[Int]] =
    events.collect { case PartChosen(part, _) => part }.startWith(None)

  private val status: Signal[String] = events
    .collect {
      case ScoreLoading(path) =>
        Tr(s"Loading $path…", s"Chargement de $path…")
      case ScoreUnreadable(_, reason) => reason
      case _: ScoreShown              => ""
      case SoundsLoading              =>
        Tr("Preparing the sound…", "Son en préparation…")
      case SoundsReady              => ""
      case PlaybackStarted          => ""
      case RequestFailed(_, reason) => reason
      case _: RecordingStarted      =>
        Tr("Recording: play now", "Enregistrement : jouez")
      case LevelChecking(attempt) =>
        Tr(
          s"Play your loudest passage now, for six seconds${
              if (attempt > 1) s" (try $attempt)" else ""
            }…",
          s"Jouez maintenant votre passage le plus fort, pendant six secondes${
              if (attempt > 1) s" (essai $attempt)" else ""
            }…"
        )
      case LevelChecked(outcome)                                 => outcome
      case RecordingLevel(peak) if peak >= PlayerBar.saturatedAt =>
        Tr(
          "The microphone saturates: lower its input volume in the system's sound settings, or play further from it",
          "Le micro sature : baissez son volume d'entrée dans les réglages du son du système, ou jouez plus loin de lui"
        )
      case RecordingReviewing =>
        Tr("Reviewing your recording…", "Analyse de votre enregistrement…")
      case _: PerformanceReviewed => ""
      case _: PartChosen          => ""
      case _: EditingSet          => ""
    }
    .startWith("")

  private val editing: Signal[Boolean] =
    events.collect { case EditingSet(on) => on }.startWith(false)

  // What happens when Play is pressed, said when nothing else is.
  private val situation: Signal[String] =
    parts.combineWith(chosen, heard, editing).map { (all, part, played, on) =>
      def names(indices: Set[Int]) =
        all.zipWithIndex.collect {
          case (each, index) if indices.contains(index) => each.name
        }
      val others = names(part.fold(played)(played - _))
      val guided = part.exists(played.contains)
      val listed = others.mkString(", ")
      val accompanied =
        if (others.isEmpty) Tr("alone", "sans accompagnement")
        else Tr(s"with $listed", s"avec $listed")
      val guide =
        if (guided) {
          Tr(
            ", your part heard as a guide",
            ", votre partie entendue comme guide"
          )
        } else ""
      val with_ = accompanied + guide
      part.flatMap(all.lift).map(_.name) match {
        case _ if all.isEmpty => ""
        case _ if on          =>
          Tr(
            "Annotating: click a note, then a change",
            "Annotation : cliquez sur une note, puis sur une modification"
          )
        case Some(name) =>
          Tr(s"You play $name, $with_", s"Vous jouez $name, $with_")
        case None =>
          val heardNames = names(played).mkString(", ")
          Tr(s"Listening to $heardNames", s"Vous écoutez $heardNames")
      }
    }

  private val playButton: HtmlElement = button(
    cls := "player-play",
    idAttr := "play",
    disabled <-- noScore,
    title <-- soundPreparing.map(
      if (_)
        Tr(
          "The sound is being prepared: playback starts once it is ready",
          "Le son est en préparation : la lecture démarre dès qu'il est prêt"
        )
      else ""
    ),
    aria.label <-- playing.map(
      if (_) Tr("Pause", "Pause") else Tr("Play", "Lecture")
    ),
    child.text <-- playing.map(if (_) "❚❚" else "▶"),
    onClick --> (_ => bus.send(TogglePlayback))
  )

  private val stopButton: HtmlElement = button(
    cls := "player-round",
    idAttr := "stop",
    aria.label := Tr("Stop, back to the start", "Arrêt, retour au début"),
    disabled <-- noScore,
    "⏮",
    onClick --> (_ => bus.send(StopPlayback))
  )

  private enum Recording {
    case Idle, CountingIn, Recording, Reviewing
  }

  private val recordingState: Signal[Recording] = events
    .collect {
      case RecordingCountingIn    => Recording.CountingIn
      case _: RecordingStarted    => Recording.Recording
      case RecordingReviewing     => Recording.Reviewing
      case _: PerformanceReviewed => Recording.Idle
      case RequestFailed(StartRecording | StopRecording, _) => Recording.Idle
    }
    .startWith(Recording.Idle)

  // Records the part played, from where playback stands, with the parts heard.
  private val recordButton: HtmlElement = button(
    cls := "player-record",
    idAttr := "record",
    cls("is-recording") <-- recordingState.map(_ != Recording.Idle),
    disabled <-- noScore
      .combineWith(recordingState)
      .map((none, state) => none || state == Recording.Reviewing),
    title := Tr(
      "Record yourself playing your part, from where playback stands, with the parts heard, and have it reviewed",
      "Enregistrez-vous jouant votre partie, depuis la position de lecture, avec les parties entendues, et faites-la analyser"
    ),
    child.text <-- recordingState.map {
      case Recording.Idle       => Tr("● Record", "● Enregistrer")
      case Recording.CountingIn => Tr("Counting in…", "Décompte…")
      case Recording.Recording  => Tr("■ Stop", "■ Arrêter")
      case Recording.Reviewing  => Tr("Reviewing…", "Analyse…")
    },
    onClick.compose(_.sample(recordingState)) --> {
      case Recording.Idle => bus.send(RecordMyPart)
      case _              => bus.send(StopRecording)
    }
  )

  // the microphone's level while recording, kept red a moment after it saturated
  private val level: Signal[Option[Double]] = events
    .collect {
      case RecordingLevel(peak)                             => Some(peak)
      case _: LevelChecked                                  => None
      case RecordingReviewing | _: PerformanceReviewed      => None
      case RequestFailed(StartRecording | StopRecording, _) => None
    }
    .startWith(None)

  private val saturated: Signal[Boolean] = events
    .collect { case RecordingLevel(peak) => peak >= PlayerBar.saturatedAt }
    .filter(identity)
    .flatMapSwitch(_ =>
      EventStream.merge(
        EventStream.fromValue(true),
        EventStream.delay(PlayerBar.saturatedHoldMillis).mapTo(false)
      )
    )
    .startWith(false)

  // from -48 dB to full scale
  private def levelWidth(peak: Double): String = {
    val decibels = 20 * math.log10(math.max(peak, 1e-6))
    s"${math.max(0.0, math.min(100.0, 100 + decibels / 48 * 100))}%"
  }

  private val levelMeter: HtmlElement = div(
    cls := "level-meter",
    hidden <-- level.map(_.isEmpty),
    cls("is-saturated") <-- saturated,
    title := Tr("The microphone's level", "Le niveau du micro"),
    div(cls := "level-meter-bar", width <-- level.map(_.fold("0%")(levelWidth)))
  )

  // The space bar plays and pauses, but not while a field is typed in or a
  // button has the focus (it would press it too).
  private val spaceBar = documentEvents(_.onKeyDown)
    .filter(event =>
      event.key == " " && !(event.target match {
        case element: dom.Element =>
          element.matches("input, select, textarea, button")
        case _ => false
      })
    ) --> { event =>
    event.preventDefault()
    bus.send(TogglePlayback)
  }

  private def clockTime(seconds: Double): String = {
    val whole = seconds.toInt
    f"${whole / 60}%d:${whole % 60}%02d"
  }

  // Shows the measure playing, but not over what is being typed in it.
  private val measureField: HtmlElement = input(
    cls := "player-field measure-field",
    idAttr := "measure",
    typ := "text",
    aria.label := Tr(
      "Measure: type one in to go there",
      "Mesure : saisissez-en une pour y aller"
    ),
    title := Tr(
      "The measure playing; type one in to go there",
      "La mesure jouée ; saisissez-en une pour y aller"
    ),
    disabled <-- noScore,
    inContext(field =>
      value <-- measure.filter(_ => dom.document.activeElement != field.ref)
    ),
    onChange.mapToValue.map(_.trim).filter(_.nonEmpty) --> (number =>
      bus.send(PlayFromMeasure(number))
    )
  )

  private val progress: HtmlElement = div(
    cls := "player-progress",
    span(cls := "player-label", Tr("Bar", "Mesure")),
    measureField,
    div(
      cls := "player-track",
      div(
        cls := "player-track-done",
        width <-- elapsedSeconds
          .combineWith(lengthSeconds)
          .map((at, length) =>
            s"${if (length > 0) math.min(100.0, at / length * 100) else 0.0}%"
          )
      )
    ),
    span(
      cls := "player-time",
      idAttr := "clock",
      child.text <-- elapsedSeconds
        .combineWith(lengthSeconds)
        .map((at, length) => s"${clockTime(at)} / ${clockTime(length)}")
    )
  )

  // The tempi, against the ones written; set in the drawer.
  private val tempoControl: HtmlElement = div(
    cls := "player-group",
    span(cls := "player-label", Tr("Tempo", "Tempo")),
    button(
      cls := "player-tempo",
      idAttr := "tempo",
      cls("is-open") <-- tempiShown,
      disabled <-- noScore,
      title := Tr(
        "The score's tempi, all in view, to set them",
        "Les tempos de la partition, tous en vue, pour les régler"
      ),
      children <-- tempiShownInBar,
      onClick.compose(_.sample(tempiShown)) --> (on => bus.send(ShowTempi(!on)))
    )
  )

  private val countInSelect: HtmlElement = select(
    cls := "player-select",
    idAttr := "count-in",
    aria.label := Tr("Count-in", "Décompte"),
    disabled <-- noScore,
    option(value := "0", Tr("No count-in", "Sans décompte")),
    option(value := "1", Tr("Count in 1 bar", "Décompte d'une mesure")),
    option(value := "2", Tr("Count in 2 bars", "Décompte de 2 mesures")),
    controlled(
      value <-- countIn.map(_.toString),
      onChange.mapToValue.map(_.toInt) --> (measures =>
        bus.send(SetCountIn(measures))
      )
    )
  )

  private val mixed: Signal[SoundMixed] = events
    .collect { case now: SoundMixed => now }
    .startWith(SoundMixed(SoundMix(Vector.empty), Map.empty))

  // Asking for rendered sounds changes nothing heard until they are ready.
  private val rendering: Signal[String] = events
    .collect {
      case SoundMixed(_, waiting) if waiting.nonEmpty =>
        val known = waiting.values.flatten
        val done =
          if (known.isEmpty) ""
          else s" ${math.round(known.sum.toDouble / waiting.size)} %"
        val plural = if (waiting.sizeIs > 1) "s" else ""
        Tr(
          s"rendering ${waiting.size} part$plural$done…",
          s"rendu de ${waiting.size} partie$plural$done…"
        )
      case _: SoundMixed          => ""
      case _: RecordingPlayedBack =>
        Tr("your recording", "votre enregistrement")
      case ScoreSoundBack => ""
    }
    .startWith("")

  private val mixedChoice = "Mixed"

  // What the score sounds with: what changes most what is heard. One engine
  // for every part here; one for each instrument in the Sounds drawer.
  private val soundControl: HtmlElement = div(
    cls := "player-group",
    span(cls := "player-label", Tr("Sound", "Son")),
    select(
      cls := "player-select",
      idAttr := "sound",
      aria.label := Tr("Sound", "Son"),
      disabled <-- noScore,
      SoundEngine.values.toSeq.map(each =>
        option(value := each.toString, each.label)
      ),
      option(
        value := mixedChoice,
        disabled := true,
        Tr(
          "One for each instrument: see Sounds",
          "Un par instrument : voir Sons"
        )
      ),
      controlled(
        value <-- mixed.map(_.mix.engine.fold(mixedChoice)(_.toString)),
        onChange.mapToValue
          .filter(_ != mixedChoice)
          .map(SoundEngine.valueOf) --> (each => bus.send(UseSound(each)))
      )
    ),
    span(
      cls := "player-label",
      idAttr := "sound-progress",
      title := Tr(
        "The sound in use plays meanwhile",
        "Le son actuel joue en attendant"
      ),
      child.text <-- rendering
    )
  )

  override lazy val node: HtmlElement = footerTag(
    cls := "player-bar",
    spaceBar,
    progress,
    div(
      cls := "player-controls",
      stopButton,
      playButton,
      recordButton,
      levelMeter,
      div(cls := "player-separator"),
      tempoControl,
      div(cls := "player-separator"),
      countInSelect,
      div(cls := "player-separator"),
      soundControl,
      span(
        cls := "player-status",
        idAttr := "status",
        child.text <-- status
          .combineWith(situation)
          .map((said, now) => if (said.nonEmpty) said else now),
        child.maybe <-- soundPreparing.map(
          Option.when(_)(
            button(
              cls := "button is-small is-text player-status-link",
              Tr("See the sounds", "Voir les sons"),
              onClick --> (_ => bus.send(ShowSounds(true)))
            )
          )
        )
      )
    )
  )
}

object PlayerBar {
  val saturatedAt = 0.98
  val saturatedHoldMillis = 1500
}
