package ripieno.frontend.appServices

import ripieno.frontend.api.ApiClient
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.Engraving
import ripieno.frontend.sound.{Metronome, Recorder}
import ripieno.frontend.utils.{AppStreams, RememberedSettings}
import ripieno.shared.*

import scala.scalajs.js.typedarray.ArrayBuffer

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Records the player playing the part shown alone, and has the server review
  * the recording against it.
  *
  * The recording starts where playback stands, after the count-in, with the
  * parts heard, which `PlaybackService` starts with it: on speakers the
  * microphone hears them too, and the review is given their notes to take them
  * out.
  */
class ReviewService(bus: AppBus, apiClient: ApiClient) {

  private var shown: Option[Engraving] = None
  // the engraving with every part, for the notes played along
  private var whole: Option[Engraving] = None
  private var listening = RememberedSettings.listening
  // the recordings closed by the player, for the score opened
  private var recordingsClosed = false
  // the parts heard, which play along with the recording
  private var heard: Set[Int] = Set.empty
  private var solo: Option[Int] = None
  private var partCount = 0
  private var path = ""
  private var partNames: List[String] = Nil
  private var positionSeconds = 0.0
  private var countInMeasures = 0
  private var writtenTempo: Option[Tempo] = None
  private var beatsPerMinute = 0
  private val metronome = Metronome()

  // what is being recorded: the part from where it starts
  private var recording: Option[(Recorder, PerformanceReference)] = None
  private val reviewPollMillis = 1000
  private val levelMillis = 100
  private var checkingLevel = false
  private val levelCheckMillis = 6000
  private val levelChecks = 3
  // where the loudest notes should peak, in dB of full scale
  private val wantedPeak = -6.0
  private val lowestGoodPeak = -12.0
  private val highestGoodPeak = -2.0
  private val saturatedAt = 0.98

  bus.events.events.foreach {
    case ScoreOpened(opened, parts, _, tempo, engraving) =>
      whole = Some(engraving)
      if (opened != path) {
        path = opened
        recordingsClosed = false
        list()
      }
      partCount = parts.size
      partNames = parts.map(_.name)
      writtenTempo = Some(tempo)
      beatsPerMinute = tempo.beatsPerMinute
      positionSeconds = 0
    case shownNow: ScoreShown =>
      shown = Some(shownNow.engraving)
      solo = shownNow.solo
    case PlaybackMoved(seconds, _, _) => positionSeconds = seconds
    case PlaybackStopped              => positionSeconds = 0
    case CountInSet(measures)         => countInMeasures = measures
    case PartsPlayed(parts)           => heard = parts
    case ListeningSet(now)            => listening = now
    case TempoSet(chosen)             => beatsPerMinute = chosen
    case _                            => ()
  }(using unsafeWindowOwner)

  bus.commands.events.foreach {
    case StartRecording => start()
    case CheckLevel    => if (recording.isEmpty && !checkingLevel) checkLevel(1)
    case StopRecording => stop()
    case ShowRecordings(on) =>
      recordingsClosed = !on
      bus.publish(RecordingsShown(on))
    case OpenRecording(id)   => AppStreams.runOnce(apiClient.review(id))(follow)
    case DeleteRecording(id) =>
      AppStreams.runOnce(apiClient.deleteReview(id)) {
        case Left(reason) =>
          bus.publish(
            RequestFailed(
              DeleteRecording(id),
              Tr(s"Not deleted: $reason", s"Non supprimé : $reason")
            )
          )
        case Right(_) => list()
      }
    case _ => ()
  }(using unsafeWindowOwner)

  private def rate: Double =
    writtenTempo.fold(1.0)(written =>
      beatsPerMinute.toDouble / written.beatsPerMinute
    )

  private def start(): Unit = {
    // the part to review: the one shown alone, or the score's only one
    val part = solo.orElse(Option.when(partCount == 1)(0))
    (shown, part) match {
      case _ if recording.isDefined || metronome.counting => ()
      case (Some(engraving), Some(index))                 =>
        // with what finds the recordings of the same part again
        val reference = engraving
          .reference(positionSeconds * 1000)
          .copy(
            score = Some(path),
            part = partNames.lift(index),
            beat = writtenTempo.map(_.beat.symbol),
            writtenBeatsPerMinute = writtenTempo.map(_.beatsPerMinute.toDouble),
            accompaniment = accompaniment(index)
          )
        val bar = engraving.barAt(positionSeconds * 1000)
        AppStreams.runOnce(Recorder.open()) {
          case Left(reason) =>
            bus.publish(
              RequestFailed(
                StartRecording,
                Tr(s"No microphone: $reason", s"Pas de micro : $reason")
              )
            )
          case Right(recorder) =>
            def go(): Unit = {
              if (playsAlong(index)) {
                bus.send(PlayAlongNow)
              }
              recorder.start()
              recording = Some((recorder, reference))
              meter(recorder)
              bus.publish(RecordingStarted(bar.fold("")(_.number)))
            }
            bar.filter(_ => countInMeasures > 0) match {
              case Some(counted) =>
                bus.publish(RecordingCountingIn)
                metronome.countIn(
                  countInMeasures,
                  counted.beats,
                  counted.beatMillis / 1000 / rate,
                  // a pickup enters before the count's end, where it is written
                  engraving.pickup(counted).fold(0.0)(_ / 1000 / rate)
                )(() => go())
              case None => go()
            }
        }
      case _ =>
        bus.publish(
          RequestFailed(
            StartRecording,
            Tr(
              "Choose the part you play to record it",
              "Choisissez la partie que vous jouez pour l'enregistrer"
            )
          )
        )
    }
  }

  // Whether something plays with the part recorded: any part heard but it.
  private def playsAlong(part: Int): Boolean = (heard - part).nonEmpty

  // The parts heard as they play along from where the recording starts.
  private def accompaniment(part: Int): Option[Accompaniment] =
    whole.filter(_ => playsAlong(part)).map { engraving =>
      Accompaniment(
        onSpeakers = listening == Listening.Speakers,
        startMillis = positionSeconds * 1000,
        rate = rate,
        notes = engraving.accompaniment(heard, positionSeconds * 1000)
      )
    }

  // The level, ten times a second, for as long as this recorder records.
  private def meter(recorder: Recorder): Unit = {
    if (recording.exists(_._1 eq recorder)) {
      bus.publish(RecordingLevel(recorder.peak))
      dom.window.setTimeout(() => meter(recorder), levelMillis)
    }
  }

  // The recordings of the score opened, the latest first.
  private def list(): Unit = {
    val listed = path
    AppStreams.runOnce(apiClient.listReviews(listed)) {
      case Right(found) if listed == path =>
        bus.publish(RecordingsListed(listed, found.reviews))
        // shown as soon as there are some, unless closed for this score
        if (found.reviews.nonEmpty && !recordingsClosed) {
          bus.publish(RecordingsShown(true))
        }
      case Right(_)     => ()
      case Left(reason) =>
        bus.publish(
          RequestFailed(
            OpenScore(listed),
            Tr(s"No recordings: $reason", s"Pas d'enregistrements : $reason")
          )
        )
    }
  }

  // Listens to the loudest passage, then sets the microphone from it and
  // listens again, up to `levelChecks` times.
  private def checkLevel(attempt: Int): Unit = {
    checkingLevel = true
    AppStreams.runOnce(Recorder.open()) {
      case Left(reason) =>
        checkingLevel = false
        bus.publish(
          LevelChecked(Tr(s"No microphone: $reason", s"Pas de micro : $reason"))
        )
      case Right(listening) =>
        bus.publish(LevelChecking(attempt))
        val started = dom.window.performance.now()
        var loudest = 0.0
        def listen(): Unit = {
          val peak = listening.peak
          loudest = math.max(loudest, peak)
          bus.publish(RecordingLevel(peak))
          if (dom.window.performance.now() - started < levelCheckMillis) {
            dom.window.setTimeout(() => listen(), 50)
          } else {
            listening.close()
            levelHeard(attempt, loudest)
          }
        }
        listen()
    }
  }

  private def levelHeard(attempt: Int, loudest: Double): Unit = {
    val saturated = loudest >= saturatedAt
    val peak = 20 * math.log10(math.max(loudest, 1e-6))
    val good = !saturated && peak >= lowestGoodPeak && peak <= highestGoodPeak
    def heard =
      if (saturated) {
        Tr("your loudest notes saturate", "vos notes les plus fortes saturent")
      } else {
        Tr(
          f"your loudest notes peak at $peak%.0f dB",
          f"vos notes les plus fortes culminent à $peak%.0f dB"
        )
      }
    AppStreams.runOnce(apiClient.microphoneLevel()) { level =>
      level match {
        case Right(microphone)
            if !good && microphone.adjustable && attempt < levelChecks =>
          // halved when saturating, since how far beyond is not known
          val factor =
            if (saturated) 0.5
            else
              math.max(
                0.25,
                math.min(4.0, math.pow(10, (wantedPeak - peak) / 20))
              )
          val volume = math.max(0.05, math.min(1.0, microphone.volume * factor))
          if (math.abs(volume - microphone.volume) < 0.02) {
            finish(
              if (factor > 1) {
                Tr(
                  s"${heard.capitalize}, and the microphone is already as high as it goes.",
                  s"${heard.capitalize}, et le micro est déjà au plus haut."
                )
              } else {
                Tr(
                  s"${heard.capitalize}, and the microphone is already as low as it goes.",
                  s"${heard.capitalize}, et le micro est déjà au plus bas."
                )
              }
            )
          } else {
            AppStreams.runOnce(apiClient.setMicrophoneLevel(volume)) {
              case Right(set) =>
                bus.publish(
                  LevelChecked(
                    Tr(
                      s"${heard.capitalize}: microphone set from ${percent(microphone.volume)} to ${percent(set.volume)}; once more…",
                      s"${heard.capitalize} : micro réglé de ${percent(microphone.volume)} à ${percent(set.volume)} ; encore une fois…"
                    )
                  )
                )
                dom.window.setTimeout(() => checkLevel(attempt + 1), 1500)
              case Left(reason) =>
                finish(
                  Tr(
                    s"${heard.capitalize}; the microphone could not be set: $reason",
                    s"${heard.capitalize} ; le micro n'a pas pu être réglé : $reason"
                  )
                )
            }
          }
        case Right(microphone) if good =>
          finish(
            Tr(
              s"Good level: ${heard} (microphone at ${percent(microphone.volume)}).",
              s"Bon niveau : ${heard} (micro à ${percent(microphone.volume)})."
            )
          )
        case Right(microphone) if microphone.adjustable =>
          finish(
            Tr(
              s"${heard.capitalize} after $attempt tries (microphone at ${percent(microphone.volume)}).",
              s"${heard.capitalize} après $attempt essais (micro à ${percent(microphone.volume)})."
            )
          )
        case _ =>
          finish(
            if (good) Tr(s"Good level: $heard.", s"Bon niveau : $heard.")
            else if (saturated || peak > highestGoodPeak)
              Tr(
                s"${heard.capitalize}: lower the microphone's input volume in the system's sound settings, or play further from it.",
                s"${heard.capitalize} : baissez le volume d'entrée du micro dans les réglages son du système, ou jouez plus loin de lui."
              )
            else
              Tr(
                s"${heard.capitalize}: raise the microphone's input volume in the system's sound settings, or play nearer to it.",
                s"${heard.capitalize} : montez le volume d'entrée du micro dans les réglages son du système, ou jouez plus près de lui."
              )
          )
      }
    }
  }

  private def finish(outcome: String): Unit = {
    checkingLevel = false
    bus.publish(LevelChecked(outcome))
  }

  private def percent(volume: Double): String = s"${math.round(volume * 100)} %"

  private def stop(): Unit = {
    if (metronome.counting) {
      metronome.cancel()
      bus.publish(RecordingReviewing)
      bus.publish(
        RequestFailed(
          StopRecording,
          Tr(
            "Recording stopped before it started",
            "Enregistrement arrêté avant d'avoir commencé"
          )
        )
      )
    }
    recording.foreach { (recorder, reference) =>
      recording = None
      bus.publish(RecordingReviewing)
      val reviewed = recorder.stop().flatMapSwitch {
        case Left(reason) =>
          EventStream.fromValue(
            Left(
              Tr(
                s"the recording was lost: $reason",
                s"l'enregistrement a été perdu : $reason"
              )
            )
          )
        case Right(sound) => send(reference, sound)
      }
      AppStreams.runOnce(reviewed)(follow)
    }
  }

  private def send(
      reference: PerformanceReference,
      sound: ArrayBuffer
  ): EventStream[AppResult[ReviewStatus]] =
    apiClient.startReview(reference).flatMapSwitch {
      case Left(reason)  => EventStream.fromValue(Left(reason))
      case Right(review) => apiClient.sendRecording(review.id, sound)
    }

  // Asks again until the review is ready or has failed.
  private def follow(status: AppResult[ReviewStatus]): Unit = {
    status match {
      case Right(ReviewStatus(id, true, _, Some(review))) =>
        bus.publish(PerformanceReviewed(id, review))
        list()
      case Right(ReviewStatus(id, false, None, _)) =>
        AppStreams.runOnce(
          EventStream
            .delay(reviewPollMillis)
            .flatMapSwitch(_ => apiClient.review(id))
        )(follow)
      case failed =>
        val reason = failed.fold(
          identity,
          _.failure.getOrElse(
            Tr(
              "the server gave no reason",
              "le serveur n'a pas donné de raison"
            )
          )
        )
        bus.publish(
          RequestFailed(
            StopRecording,
            Tr(s"No review: $reason", s"Pas d'analyse : $reason")
          )
        )
    }
  }
}
