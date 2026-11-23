package ripieno.frontend.appServices

import ripieno.frontend.api.ApiClient
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.*
import ripieno.frontend.sound.*
import ripieno.frontend.utils.*
import ripieno.shared.*
import ripieno.shared.routes.{ReviewRoutes, SoundRoutes}

import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Plays the opened score, the parts chosen and at the tempo chosen, and tells
  * where the sound is in terms of what the sheet shows.
  *
  * When a recording is made with the other parts, they are what is played, and
  * they start and stop with the recording.
  */
class PlaybackService(bus: AppBus, apiClient: ApiClient) {

  private var scoreMidi: Option[Uint8Array] = None
  private var partCount = 0
  // the staves of each part, which Verovio gives a MIDI track each
  private var partStaves: List[Int] = Nil
  // the score's MIDI less the parts not played
  private var midi: Option[Uint8Array] = None
  private var playedParts: Set[Int] = Set.empty
  private var shown: Option[Engraving] = None
  private var scorePath = ""
  private var writtenTempo: Option[Tempo] = None
  // of the written tempo
  private var rate = 1.0
  // the whole score's engraving; in a score whose sections each have their
  // own tempo, the one where playback stands, with the tempo typed for it
  // until it is set
  private var whole: Option[Engraving] = None
  private var section: Option[SectionTempo] = None
  private var sectionTyped: Option[(Int, Int)] = None
  private var sectionSettled: Option[Int] = None
  // the notes selected, to select again once the score is timed again
  private var selectedNotes: Option[(String, String)] = None
  // where the score timed again carries on, in seconds, when no player was
  // there to carry on with: the next player starts there
  private var resumeAt: Option[Double] = None
  // how each part sounds; the parts rendered one by one, played together
  // whenever it is not all sampled at full level
  private var mix = SoundMix(Vector.empty)
  private var stems: Option[StemsPlayer] = None
  private var countInMeasures = 0
  // measures counted where a new tempo starts, and where the sound stood the
  // frame before, to see it get there
  private var sectionCountIn = 0
  private var lastPosition = 0.0
  // the section last counted in, not counted again as the sound starts back a
  // little before it (the output's delay), until the sound goes back before it
  private var countedIn: Option[Int] = None
  private val metronome = Metronome()
  // the metronome's clicks with the music, when it is on: scheduled a few
  // seconds at a time from where the sound is, again whenever it moves off
  // where they expect it (a seek, a new tempo)
  private val along = Metronome()
  along.settings = RememberedSettings.metronome
  private var metronomeOn = false
  private var alongFrom: Option[(Double, Double, Double)] = None

  // the one playing; the sound bank's stays loaded when a rendering takes over
  private var player: Option[ScorePlayer] = None
  private var soundFontPlayer: Option[SoundFontPlayer] = None
  // fetched as soon as the page opens, so that the first Play has little to wait for
  private var soundBank: Option[AppResult[ArrayBuffer]] = None
  private var startingPlayer = false
  // whether the player started for the sound bank plays at once, or only waits
  private var playOnceStarted = true

  private var playingAlong = false

  // The request for the parts' renderings whose answer is still wanted.
  // Opening a score or choosing a sound again starts another one: an answer
  // to an earlier request must not bring its player in on top of the current
  // one.
  private var renderingRequest = 0
  private val renderingPollMillis = 2000

  private var published = PlaybackMoved(0, Sounding.silence, "")

  // the notes selected, where playback of them stops (score seconds), and the
  // recording last reviewed, to play them as they were played
  private var range: Option[ScoreRange] = None
  private var rangeEnd: Option[Double] = None
  private var reviewed: Option[(String, List[AlignmentPoint])] = None
  // the score's own player, to go back to from a recording
  private var scorePlayer: Option[ScorePlayer] = None
  // a selection to play once the sound bank, asked for, is there
  private var pendingRange: Option[Boolean] = None

  AppStreams.runOnce(apiClient.soundBank()) { result =>
    soundBank = Some(result)
    if (startingPlayer) {
      startSoundFontPlayer(result)
    }
  }

  bus.events.events.foreach {
    case ScoreOpened(path, parts, midi, tempo, engraving) =>
      stop()
      // the same score engraved again, an edit made: the parts heard stay
      val kept = Option
        .when(path == scorePath && parts.size == partCount)(playedParts)
        .filter(_.nonEmpty)
      scorePath = path
      scoreMidi = Some(midi)
      partCount = parts.size
      partStaves = parts.map(_.staves)
      playedParts = kept.getOrElse(parts.indices.toSet)
      this.midi = Some(
        kept
          .flatMap(heard => Midi.onlyParts(midi, heard, partStaves).toOption)
          .getOrElse(midi)
      )
      writtenTempo = Some(tempo)
      whole = Some(engraving)
      section = None
      sectionTyped = None
      bus.publish(SectionShown(None))
      rate = 1.0
      soundFontPlayer.foreach { soundFont =>
        this.midi.foreach(soundFont.load(_))
        soundFont.speed = rate
      }
      // a rendering belongs to the score it was made from
      renderingRequest += 1
      closeStems()
      player = soundFontPlayer
      readySound()
      // where it was last played at, the written tempo staying a click away;
      // sections at their own tempo have theirs, the whole at the written one
      setRate(
        if (engraving.sections.nonEmpty) tempo.beatsPerMinute
        else RememberedTempos.get(path).getOrElse(tempo.beatsPerMinute)
      )
      publishSection()
      publishSections()
      bus.publish(PartsPlayed(playedParts))
      // each instrument's sound, at the levels set last for this score
      val levels = RememberedSettings.levelsOf(path, parts.size)
      useMix(
        SoundMix(parts.toVector.zipWithIndex.map { (part, index) =>
          val instrument = Library.instrumentOf(part.name)
          PartSound(
            instrument,
            RememberedSettings.soundOf(instrument),
            levels.fold(100)(_(index))
          )
        }),
        remember = false
      )
    case now: ScoreShown =>
      shown = Some(now.engraving)
      if (!now.revised) {
        selectRange(None)
      }
      publishPosition()
    // the sheet left for the library: its sound stops
    case PageShown(page) if !page.isInstanceOf[Page.Score] => stop()
    case MetronomeSet(on)                                  => metronomeOn = on
    case MetronomeConfigured(now) => along.settings = now; alongFrom = None
    case RecordingReviewing | RequestFailed(StartRecording, _) =>
      if (playingAlong) {
        playingAlong = false
        player.foreach(_.pause())
        bus.publish(PlaybackPaused)
      }
    case PerformanceReviewed(id, review) =>
      reviewed = Some((id, review.alignment))
    case ScoreRetimed(retimedMidi, _) => retimed(retimedMidi)
    case _                            => ()
  }(using unsafeWindowOwner)

  bus.commands.events.foreach {
    case TogglePlayback              => toggle()
    case StopPlayback                => stop()
    case SetTempo(beatsPerMinute)    => setTempo(beatsPerMinute)
    case command: PlayParts          => playParts(command)
    case command: PlayFromMeasure    => playFromMeasure(command)
    case SetCountIn(measures)        => setCountIn(measures)
    case SetSectionCountIn(measures) =>
      sectionCountIn = math.max(0, measures)
      bus.publish(SectionCountInSet(sectionCountIn))
    case PlayFromNote(note) => playFrom(note)
    case UseSound(engine)   =>
      mix.instruments.foreach(RememberedSettings.setSound(_, engine))
      useMix(mix.withEngine(engine))
    case UseInstrumentSound(instrument, engine) =>
      RememberedSettings.setSound(instrument, engine)
      useMix(mix.withInstrument(instrument, engine))
    case SetPartLevel(part, level) =>
      if (mix.parts.isDefinedAt(part)) {
        mix = mix.withLevel(part, math.max(0, math.min(100, level)))
        RememberedSettings.setLevels(scorePath, mix)
        // a level is heard at once when the parts are rendered already
        if (stems.isDefined || mix.plain) useMix(mix, remember = false)
        else publishMix()
      }
    case BackToScoreSound                 => backToScoreSound()
    case ListenToRecording(id, alignment) => listenTo(id, alignment)
    case SelectRange(from, to)            => selectRange(Some((from, to)))
    case ClearRange                       => selectRange(None)
    case command: PlayRange               => playRange(command)
    case PlayAlongNow                     => startAlong()
    case _                                => ()
  }(using unsafeWindowOwner)

  followPlayback()

  private def toggle(): Unit = {
    endRange()
    player match {
      case Some(_) if metronome.counting =>
        metronome.cancel()
        bus.publish(PlaybackPaused)
      case Some(player) if player.playing =>
        player.pause()
        bus.publish(PlaybackPaused)
      case Some(player) =>
        start(player)
      // the sound still being made ready: woken by this click, it plays as
      // soon as it is
      case None if startingPlayer =>
        SoundFontPlayer.wake()
        playOnceStarted = true
        bus.publish(SoundsLoading)
      case None if midi.isDefined =>
        startingPlayer = true
        bus.publish(SoundsLoading)
        soundBank.foreach(startSoundFontPlayer)
      case None => ()
    }
  }

  // The score's own sound made ready as soon as it opens, not at the first
  // click on play: the browser lets audio be prepared asleep and woken by
  // that click, and the sound bank takes a while to be set up. Until then,
  // play is not offered; a part recorded with the others has them ready too.
  private def readySound(): Unit =
    if (soundFontPlayer.isDefined) bus.publish(SoundsReady)
    else if (!startingPlayer) {
      startingPlayer = true
      playOnceStarted = false
      bus.publish(SoundsLoading)
      soundBank.foreach(startSoundFontPlayer)
    }

  // The recording has started: the other parts from where it starts, no
  // count-in of their own.
  private def startAlong(): Unit = {
    endRange()
    player match {
      case Some(along) if !along.playing =>
        playingAlong = true
        along.speed = rate
        along.play()
        bus.publish(PlaybackStarted)
      case Some(_) => ()
      case None    =>
        bus.publish(
          RequestFailed(
            StartRecording,
            Tr(
              "The other parts were not ready: you are recording alone",
              "Les autres parties n'étaient pas prêtes : vous enregistrez sans accompagnement"
            )
          )
        )
    }
  }

  private def startSoundFontPlayer(soundBank: AppResult[ArrayBuffer]): Unit = {
    val opening = soundBank.fold(
      reason => EventStream.fromValue(Left(reason)),
      SoundFontPlayer.open
    )
    AppStreams.runOnce(opening) {
      case Left(reason) =>
        startingPlayer = false
        playOnceStarted = true
        bus.publish(
          RequestFailed(
            TogglePlayback,
            Tr(s"No sound: $reason", s"Pas de son : $reason")
          )
        )
      case Right(opened) =>
        val playing = playOnceStarted
        playOnceStarted = true
        startingPlayer = false
        soundFontPlayer = Some(opened)
        bus.publish(SoundsReady)
        opened.onEnded(() => stop())
        midi.foreach(opened.load(_))
        opened.speed = rate
        // a rendering may have taken over while the sound bank was loading
        if (player.isEmpty) {
          player = Some(opened)
          scorePlayer = Some(opened)
          resumeAt.foreach(opened.seek)
          resumeAt = None
          pendingRange match {
            case Some(beats) =>
              pendingRange = None
              playRangeOn(opened, asPlayed = false, beats)
            case None if playing => start(opened)
            case None            => ()
          }
        }
    }
  }

  // After the count-in, if one is wanted: whichever player is current by then
  // starts.
  private def start(starting: ScorePlayer): Unit = {
    bus.publish(PlaybackStarted)
    val counted = for {
      engraving <- shown
      if countInMeasures > 0
      bar <- engraving.barAt(starting.position * 1000)
    } yield bar
    counted match {
      case Some(bar) =>
        metronome.countIn(
          countInMeasures,
          bar.beats,
          bar.beatMillis / 1000 / rate,
          // a pickup enters before the count's end, where it is written
          shown.flatMap(_.pickup(bar)).fold(0.0)(_ / 1000 / rate)
        ) { () =>
          player.foreach(_.play())
        }
      case None => starting.play()
    }
  }

  private def setCountIn(measures: Int): Unit = {
    countInMeasures = math.max(0, measures)
    bus.publish(CountInSet(countInMeasures))
  }

  // The recording as it was played, not at the tempo chosen for the score,
  // from the start of what it played.
  private def listenTo(id: String, alignment: List[AlignmentPoint]): Unit = {
    renderingRequest += 1
    metronome.cancel()
    val recorded =
      RecordingPlayer(ReviewRoutes.recordingAudioPath(id), alignment)
    recorded.onEnded(() => stop())
    endRange()
    player.foreach(_.pause())
    scorePlayer =
      player.filterNot(_.isInstanceOf[RecordingPlayer]).orElse(scorePlayer)
    player = Some(recorded)
    recorded.speed = 1
    alignment.headOption.foreach(start => recorded.seek(start.score / 1000))
    recorded.play()
    bus.publish(RecordingPlayedBack(id))
    bus.publish(PlaybackStarted)
  }

  // The parts still being rendered, and how far each is.
  private var rendering: Map[Int, Option[Int]] = Map.empty

  private def publishMix(): Unit = bus.publish(SoundMixed(mix, rendering))

  // How each part sounds from now on: sampled at full level, the page's own
  // synthesizer at once; otherwise each part rendered with its engine, the
  // sound in use playing meanwhile.
  private def useMix(next: SoundMix, remember: Boolean = true): Unit = {
    mix = next
    if (remember) RememberedSettings.setLevels(scorePath, next)
    if (next.plain) {
      renderingRequest += 1
      rendering = Map.empty
      closeStems()
      soundFontPlayer.filterNot(player.contains).foreach(switchTo)
      publishMix()
    } else {
      stems.filter(_.renderedRate == rate) match {
        case Some(current) if rendering.isEmpty && sameEngines(next) =>
          current.setLevels(levels)
          publishMix()
        case _ => renderParts()
      }
    }
  }

  // the engines the parts were last rendered with
  private var renderedEngines: Vector[SoundEngine] = Vector.empty

  private def sameEngines(next: SoundMix): Boolean =
    next.parts.map(_.engine) == renderedEngines

  // each part's level, silent when it is not heard. The rendered parts are
  // brought to one loudness, which would undo the balance the conductor gave
  // them: a part accompanying is set a dynamic below, for the share of the
  // score it accompanies
  private def levels: Vector[Double] =
    mix.parts.zipWithIndex.map((part, index) =>
      if (playedParts.contains(index))
        part.level / 100.0 * math.pow(
          10,
          -PlaybackService.accompanyingDecibels * whole.fold(0.0)(
            _.accompanying(index)
          ) / 20
        )
      else 0.0
    )

  private def renderParts(): Unit = {
    for (whole <- scoreMidi) {
      renderingRequest += 1
      val request = renderingRequest
      // at the tempo chosen: rendered parts play at the tempo they were made at
      val renderedRate = rate
      val engines = mix.parts.map(_.engine)
      val ready = Array.fill[Option[String]](engines.size)(None)
      rendering = engines.indices.map(_ -> None).toMap
      publishMix()
      engines.zipWithIndex.foreach { (engine, part) =>
        Midi.onlyParts(whole, Set(part), partStaves) match {
          case Left(reason) =>
            failed(
              request,
              Tr(s"part ${part + 1}: $reason", s"partie ${part + 1} : $reason")
            )
          case Right(alone) =>
            AppStreams.runOnce(
              apiClient.renderSound(
                engine.renderedBy.getOrElse(SoundEngine.sampledOnServer),
                Midi.atRate(alone, renderedRate)
              )
            )(answer =>
              follow(request, part, answer) { id =>
                ready(part) = Some(id)
                if (ready.forall(_.isDefined)) {
                  install(
                    request,
                    ready.toVector.flatten,
                    renderedRate,
                    engines
                  )
                }
              }
            )
        }
      }
    }
  }

  // Asks again until a part's rendering is ready, unless it stopped being
  // wanted.
  private def follow(
      request: Int,
      part: Int,
      answer: AppResult[RenderedSound]
  )(done: String => Unit): Unit = {
    if (request == renderingRequest) {
      answer match {
        case Right(RenderedSound(id, true, _, _)) =>
          rendering = rendering - part
          publishMix()
          done(id)
        case Right(RenderedSound(id, false, None, percent)) =>
          if (rendering.get(part).flatten != percent) {
            rendering = rendering.updated(part, percent)
            publishMix()
          }
          AppStreams.runOnce(
            EventStream
              .delay(renderingPollMillis)
              .flatMapSwitch(_ => apiClient.renderedSound(id))
          )(again => follow(request, part, again)(done))
        case failure =>
          failed(
            request,
            failure.fold(
              identity,
              _.failure.getOrElse(
                Tr(
                  "the server gave no reason",
                  "le serveur n'a donné aucune raison"
                )
              )
            )
          )
      }
    }
  }

  private def failed(request: Int, reason: String): Unit = {
    if (request == renderingRequest) {
      renderingRequest += 1
      rendering = Map.empty
      publishMix()
      bus.publish(
        RequestFailed(
          UseSound(SoundEngine.Sampled),
          Tr(s"No rendering: $reason", s"Pas de rendu : $reason")
        )
      )
    }
  }

  // The parts rendered: fetched, then played together where the sound was.
  private def install(
      request: Int,
      ids: Vector[String],
      renderedRate: Double,
      engines: Vector[SoundEngine]
  ): Unit = {
    AppStreams.runOnce(
      StemsPlayer.open(ids.map(SoundRoutes.renderedAudioPath), renderedRate)
    ) {
      case Right(opened) if request == renderingRequest =>
        val previous = stems
        opened.onEnded(() => stop())
        opened.setLevels(levels)
        renderedEngines = engines
        stems = Some(opened)
        if (!player.exists(_.isInstanceOf[RecordingPlayer])) {
          switchTo(opened)
        }
        scorePlayer = Some(opened)
        previous.foreach(_.close())
        publishMix()
      case Right(opened) => opened.close()
      case Left(reason)  => failed(request, reason)
    }
  }

  private def closeStems(): Unit = {
    stems.foreach { closing =>
      if (player.contains(closing)) {
        player = soundFontPlayer
      }
      if (scorePlayer.contains(closing)) {
        scorePlayer = soundFontPlayer
      }
      closing.close()
    }
    stems = None
    renderedEngines = Vector.empty
  }

  // From a recording played back to the score's own sound, where it was.
  private def backToScoreSound(): Unit = {
    endRange()
    stems.orElse(soundFontPlayer).foreach { scoreSound =>
      player.filter(_.isInstanceOf[RecordingPlayer]).foreach(_.stop())
      player = Some(scoreSound)
      scoreSound.speed = scoreSpeed(scoreSound)
    }
    bus.publish(PlaybackStopped)
    bus.publish(ScoreSoundBack)
  }

  // The speed a score's player plays the tempo chosen at: rendered parts only
  // play at the tempo they were made at.
  private def scoreSpeed(of: ScorePlayer): Double = of match {
    case parts: StemsPlayer => parts.renderedRate
    case _                  => rate
  }

  // The new player carries on where the other one was.
  private def switchTo(next: ScorePlayer): Unit = {
    val wasPlaying = player.exists(_.playing)
    val position = player.map(_.position).orElse(resumeAt).getOrElse(0.0)
    resumeAt = None
    player.foreach(_.pause())
    next.speed = scoreSpeed(next)
    next.seek(position)
    player = Some(next)
    if (wasPlaying) {
      next.play()
    }
  }

  // The notes from one clicked to another: from the start of the first to the
  // end of the last.
  private def selectRange(notes: Option[(String, String)]): Unit = {
    endRange()
    selectedNotes = notes
    range = for {
      (a, b) <- notes
      engraving <- shown
      first <- engraving.onset(a)
      second <- engraving.onset(b)
      firstEnd <- engraving.end(a)
      secondEnd <- engraving.end(b)
    } yield {
      val from = math.min(first, second)
      val to = math.max(firstEnd, secondEnd)
      ScoreRange(
        from,
        to,
        engraving.barAt(from).fold("")(_.number),
        engraving.barAt(to - 1).fold("")(_.number)
      )
    }
    bus.publish(RangeSelected(range))
  }

  private def playRange(command: PlayRange): Unit = {
    range.foreach { _ =>
      if (command.asPlayed) {
        reviewed match {
          case Some((id, alignment)) =>
            val recorded =
              RecordingPlayer(ReviewRoutes.recordingAudioPath(id), alignment)
            recorded.onEnded(() => stop())
            endRange()
            player.foreach(_.pause())
            scorePlayer = player
              .filterNot(_.isInstanceOf[RecordingPlayer])
              .orElse(scorePlayer)
            player = Some(recorded)
            bus.publish(RecordingPlayedBack(id))
            playRangeOn(recorded, asPlayed = true, command.beats)
          case None =>
            bus.publish(
              RequestFailed(
                command,
                Tr(
                  "Open a recording to hear how you played these notes",
                  "Ouvrez un enregistrement pour entendre comment vous avez joué ces notes"
                )
              )
            )
        }
      } else {
        val target = player
          .filterNot(_.isInstanceOf[RecordingPlayer])
          .orElse(scorePlayer)
          .orElse(soundFontPlayer)
        target match {
          case Some(scoreSound) =>
            endRange()
            player.filterNot(_ eq scoreSound).foreach(_.pause())
            player = Some(scoreSound)
            playRangeOn(scoreSound, asPlayed = false, command.beats)
          case None if !startingPlayer && midi.isDefined =>
            // the sound bank first, then the notes
            pendingRange = Some(command.beats)
            startingPlayer = true
            bus.publish(SoundsLoading)
            soundBank.foreach(startSoundFontPlayer)
          case None => ()
        }
      }
    }
  }

  // How long the selection lasted in the recording reviewed, in seconds.
  private def recordedSeconds(selected: ScoreRange): Option[Double] =
    reviewed.map((_, alignment) => alignment).filter(_.sizeIs > 1).map {
      alignment =>
        def at(score: Double): Double = {
          val after = alignment.indexWhere(_.score > score)
          if (after < 0) alignment.last.recording
          else if (after == 0) alignment.head.recording
          else {
            val (a, b) = (alignment(after - 1), alignment(after))
            a.recording + (score - a.score) / (b.score - a.score) * (b.recording - a.recording)
          }
        }
        (at(selected.to) - at(selected.from)) / 1000
    }

  // The selection on that player, stopping after it; the written notes at the
  // pace the recording reviewed took over them, if there is one.
  private def playRangeOn(
      on: ScorePlayer,
      asPlayed: Boolean,
      beats: Boolean
  ): Unit = {
    range.foreach { selected =>
      val written = (selected.to - selected.from) / 1000
      val recorded = recordedSeconds(selected).filter(_ > 0)
      val speed =
        if (asPlayed) 1.0
        else
          recorded.fold(rate)(seconds =>
            math.max(
              Tempo.slowest,
              math.min(Tempo.fastest, written / seconds)
            )
          )
      // rendered parts play at their own tempo only: at another pace, the
      // sampled sound plays the selection
      val fitting = on match {
        case parts: StemsPlayer
            if math.abs(speed - parts.renderedRate) > 1e-6 =>
          soundFontPlayer.getOrElse(on)
        case _ => on
      }
      if (fitting ne on) {
        on.pause()
        player = Some(fitting)
      }
      playOn(fitting, selected, speed, asPlayed, beats, recorded, written)
    }
  }

  private def playOn(
      on: ScorePlayer,
      selected: ScoreRange,
      speed: Double,
      asPlayed: Boolean,
      beats: Boolean,
      recorded: Option[Double],
      written: Double
  ): Unit = {
    {
      on.speed = speed
      on.seek(selected.from / 1000)
      rangeEnd = Some(selected.to / 1000)
      bus.publish(PlaybackStarted)
      // as written: the score's beats; as played: steady beats at the
      // player's own pace over the selection, which shows them rushing or
      // dragging inside it
      val lasting =
        if (asPlayed) recorded.getOrElse(written) else written / speed
      def real(scoreMillis: Double) = scoreMillis / 1000 / written * lasting
      // music entering between two beats is led in from the beat before it
      val leadIn = shown
        .filter(_ => beats)
        .flatMap(_.beatBefore(selected.from))
        .fold(0.0)(beat => selected.from - beat)
      if (beats) {
        shown.foreach(engraving =>
          metronome.beat(
            engraving
              .beatsBetween(selected.from - leadIn, selected.to)
              .map((at, stressed) =>
                (real(at - selected.from + leadIn), stressed)
              )
          )
        )
      }
      if (leadIn > 0) {
        dom.window.setTimeout(
          () => if (rangeEnd.isDefined) on.play(),
          real(leadIn) * 1000
        )
      } else {
        on.play()
      }
    }
  }

  // Playback of the selection is over: the next playback is the score's again.
  private def endRange(): Unit = {
    if (rangeEnd.isDefined) {
      rangeEnd = None
      metronome.cancel()
      player.foreach(current =>
        current.speed =
          if (current.isInstanceOf[RecordingPlayer]) 1 else scoreSpeed(current)
      )
    }
  }

  private def stop(): Unit = {
    endRange()
    metronome.cancel()
    player.foreach(_.stop())
    published = PlaybackMoved(0, Sounding.silence, "")
    bus.publish(PlaybackStopped)
  }

  // The tempo of the section where playback stands, in a score whose
  // sections each have their own: set once it has stopped changing, the score
  // timed again then.
  private def setTempo(beatsPerMinute: Int): Unit = {
    section match {
      case Some(current) =>
        val chosen = current.within(beatsPerMinute)
        sectionTyped = Some(current.index -> chosen)
        section = Some(current.copy(chosen = chosen))
        bus.publish(SectionShown(section))
        sectionSettled.foreach(dom.window.clearTimeout)
        sectionSettled = Some(
          dom.window.setTimeout(
            () => {
              sectionSettled = None
              sectionTyped = None
              bus.send(SetSectionTempo(current.index, chosen))
            },
            tempoSettlesMillis
          )
        )
      case None => setRate(beatsPerMinute)
    }
  }

  // The section where playback stands, in a score whose sections each have
  // their own tempo, published when it changes.
  private def publishSection(): Unit = {
    val now = for {
      engraving <- whole
      played <- engraving.sectionAt(
        player.map(_.position).orElse(resumeAt).getOrElse(0.0) * 1000
      )
    } yield {
      val shown = sectionTempo(engraving, played)
      sectionTyped
        .filter(_._1 == played.index)
        .fold(shown)((_, typed) => shown.copy(chosen = typed))
    }
    if (now != section) {
      section = now
      bus.publish(SectionShown(now))
    }
  }

  // A section as the player bar shows it, in beats of the score's beat.
  private def sectionTempo(
      engraving: Engraving,
      played: PlayedSection
  ): SectionTempo = {
    val beat = writtenTempo.fold(Beat.Quarter)(_.beat)
    def inBeats(quarters: Double) = math.round(quarters / beat.quarters).toInt
    SectionTempo(
      played.index,
      played.name,
      engraving.barAt(played.start).fold("")(_.number),
      beat,
      played.written.map(inBeats),
      inBeats(played.chosen)
    )
  }

  // Every section of the score, for setting their tempi all at once.
  private def publishSections(): Unit =
    bus.publish(
      SectionsShown(
        whole.toList.flatMap(engraving =>
          engraving.sections.map(sectionTempo(engraving, _))
        )
      )
    )

  // The score timed again, a section's tempo set: the sound carries on from
  // the start of the measure it was in, the parts rendered again.
  private def retimed(retimedMidi: Uint8Array): Unit = {
    val measure = published.measure
    val wasPlaying = player.exists(_.playing)
    player.foreach(_.pause())
    scoreMidi = Some(retimedMidi)
    midi = Some(
      Midi
        .onlyParts(retimedMidi, playedParts, partStaves)
        .getOrElse(retimedMidi)
    )
    selectRange(selectedNotes)
    val position = whole
      .flatMap(_.measureStart(measure))
      .fold(0.0)(_ / 1000)
    renderingRequest += 1
    rendering = Map.empty
    closeStems()
    for {
      soundFont <- soundFontPlayer
      played <- midi
    } {
      soundFont.load(played, position, wasPlaying)
      soundFont.speed = rate
      player = Some(soundFont)
      scorePlayer = Some(soundFont)
    }
    // the sampled sound not there yet: whichever player comes first starts
    // where the sound was
    if (soundFontPlayer.isEmpty) {
      resumeAt = Some(position)
      if (!startingPlayer) {
        startingPlayer = true
        playOnceStarted = false
        soundBank.foreach(startSoundFontPlayer)
      }
    }
    if (!mix.plain) renderParts()
    publishPosition()
    publishSection()
    publishSections()
  }

  // Within what the players keep a natural sound at.
  private def setRate(beatsPerMinute: Int): Unit = {
    writtenTempo.foreach { written =>
      val chosen = Tempo.within(written.beatsPerMinute, beatsPerMinute)
      rate = chosen.toDouble / written.beatsPerMinute
      player.foreach(current => current.speed = scoreSpeed(current))
      renderAtTempo()
      RememberedTempos.set(scorePath, chosen, written.beatsPerMinute)
      bus.publish(TempoSet(chosen))
    }
  }

  // Rendered parts are made at one tempo: at another, the sampled sound takes
  // over at once, and they are made again once the tempo has stopped changing.
  private var tempoSettled: Option[Int] = None
  private val tempoSettlesMillis = 1000

  private def renderAtTempo(): Unit = {
    if (!mix.plain && !stems.exists(_.renderedRate == rate)) {
      for {
        current <- stems
        if player.contains(current)
        soundFont <- soundFontPlayer
        played <- midi
      } carryOnWith(soundFont, played)
      tempoSettled.foreach(dom.window.clearTimeout)
      tempoSettled = Some(
        dom.window.setTimeout(
          () => {
            tempoSettled = None
            renderParts()
          },
          tempoSettlesMillis
        )
      )
    }
  }

  // The parts heard: the sampled sound plays them alone, the rendered parts
  // are silenced but those.
  private def playParts(command: PlayParts): Unit = {
    for {
      scoreMidi <- scoreMidi
      if command.parts.nonEmpty && command.parts != playedParts
    } {
      Midi.onlyParts(scoreMidi, command.parts, partStaves) match {
        case Left(reason) =>
          bus.publish(
            RequestFailed(
              command,
              Tr(
                s"Cannot play these parts alone: $reason",
                s"Impossible de jouer ces parties seules : $reason"
              )
            )
          )
        case Right(played) =>
          midi = Some(played)
          playedParts = command.parts
          stems match {
            case Some(current) => current.setLevels(levels)
            case None => soundFontPlayer.foreach(carryOnWith(_, played))
          }
          // the sampled sound keeps the parts heard, should it take over
          soundFontPlayer
            .filterNot(player.contains)
            .foreach(
              _.load(played, player.map(_.position).getOrElse(0.0), false)
            )
          bus.publish(PartsPlayed(playedParts))
      }
    }
  }

  // The sound bank's player takes over with other MIDI, where the sound was.
  private def carryOnWith(
      soundFont: SoundFontPlayer,
      played: Uint8Array
  ): Unit = {
    val wasPlaying = player.exists(_.playing)
    val position = player.map(_.position).getOrElse(0.0)
    player.filterNot(_ == soundFont).foreach(_.pause())
    soundFont.load(played, position, wasPlaying)
    soundFont.speed = rate
    player = Some(soundFont)
  }

  private def playFromMeasure(command: PlayFromMeasure): Unit = {
    for {
      player <- player
      engraving <- shown
    } {
      engraving.measureStart(command.number) match {
        case Some(start) =>
          player.seek(start / 1000)
          publishPosition()
        case None =>
          bus.publish(
            RequestFailed(
              command,
              Tr(
                s"There is no measure ${command.number}",
                s"Il n'y a pas de mesure ${command.number}"
              )
            )
          )
      }
    }
  }

  private def playFrom(note: String): Unit = {
    for {
      player <- player
      engraving <- shown
      onset <- engraving.onset(note)
    } {
      player.seek(onset / 1000)
      publishPosition()
    }
  }

  // Published when what a listener could tell apart has changed: the notes
  // sounding, or the tenth of a second.
  private def publishPosition(): Unit = {
    for {
      player <- player
      engraving <- shown
    } {
      val seconds = math.floor(player.position * 10) / 10
      val sounding =
        if (player.playing || seconds > 0)
          engraving.soundingAt(player.position * 1000)
        else Sounding.silence
      val measure =
        engraving.barAt(player.position * 1000).fold("")(_.number)
      val moved = PlaybackMoved(seconds, sounding, measure)
      if (moved != published) {
        published = moved
        bus.publish(moved)
      }
    }
    publishSection()
  }

  private def beatAlong(): Unit = {
    val clicking = for {
      current <- player
      engraving <- shown
      if metronomeOn && current.playing && !metronome.counting
      if rangeEnd.isEmpty && !current.isInstanceOf[RecordingPlayer]
    } yield (current, engraving)
    clicking match {
      case Some((current, engraving)) =>
        val now = dom.window.performance.now()
        val position = current.position
        val due = alongFrom.forall { (from, atRate, at) =>
          val expected = from + (now - at) / 1000 * atRate
          atRate != rate || math.abs(
            expected - position
          ) > PlaybackService.alongSlack ||
          now - at > PlaybackService.alongSchedule * 1000
        }
        if (due) {
          val until = position + 2 * PlaybackService.alongSchedule * rate
          along.beat(
            engraving
              .beatsBetween(position * 1000, until * 1000)
              .map((at, stressed) => ((at / 1000 - position) / rate, stressed))
          )
          alongFrom = Some((position, rate, now))
        }
      case None if alongFrom.isDefined =>
        along.cancel()
        alongFrom = None
      case None => ()
    }
  }

  // Where the sound gets to a new tempo, when it is to be counted in: the
  // sound stops at the section's first note (its upbeat, if it has one) and
  // the metronome beats measures of its first bar before it carries on, the
  // upbeat entering where it is written.
  private def countInNewTempo(): Unit = {
    for {
      current <- player
      if sectionCountIn > 0 && rangeEnd.isEmpty && !playingAlong
      if !current.isInstanceOf[RecordingPlayer] && !metronome.counting
      engraving <- whole
    } {
      val now = current.position
      countedIn = countedIn.filter(index =>
        engraving.sections
          .find(_.index == index)
          .exists(section => now >= section.start / 1000 - 1)
      )
      val reached = engraving.sections.drop(1).find { section =>
        val at = section.start / 1000
        !countedIn.contains(section.index) && lastPosition < at - 1e-3 &&
        now >= at - 1e-3 && now - lastPosition < 1
      }
      lastPosition = now
      for {
        section <- reached
        bar <- engraving.barStartingFrom(section.start)
      } {
        current.pause()
        current.seek(section.start / 1000)
        lastPosition = section.start / 1000
        countedIn = Some(section.index)
        metronome.countIn(
          sectionCountIn,
          bar.beats,
          bar.beatMillis / 1000 / rate,
          (bar.start - section.start) / 1000 / rate
        ) { () =>
          player.foreach(_.play())
        }
      }
    }
  }

  private def followPlayback(): Unit = {
    beatAlong()
    if (player.exists(_.playing)) {
      countInNewTempo()
      publishPosition()
      // at the end of the selection, back at its start
      for {
        end <- rangeEnd
        current <- player
        selected <- range
        if current.position >= end
      } {
        current.pause()
        endRange()
        current.seek(selected.from / 1000)
        bus.publish(PlaybackPaused)
        publishPosition()
      }
    }
    dom.window.requestAnimationFrame(_ => followPlayback())
  }
}

object PlaybackService {
  // seconds of clicks scheduled at a time (twice as many ahead), and how far
  // the sound may move from where they expect it
  val alongSchedule = 4.0
  val alongSlack = 0.08
  // how much softer than the parts it accompanies a rendered part sounds: a
  // dynamic, as from mf to mp
  val accompanyingDecibels = 6.0
}
