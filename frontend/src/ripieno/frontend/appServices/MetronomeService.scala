package ripieno.frontend.appServices

import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.Engraving
import ripieno.frontend.sound.Metronome
import ripieno.frontend.utils.RememberedSettings

import com.raquo.laminar.api.L.*

/** The metronome on its own, while nothing plays: at the tempo chosen for the
  * score, in the time of the measure where it stands; with no score, at its own
  * tempo in 4/4. While the score plays, `PlaybackService` beats with it.
  */
class MetronomeService(bus: AppBus) {
  private val metronome = Metronome()
  private var settings = RememberedSettings.metronome
  metronome.settings = settings
  private var on = false
  private var playing = false
  private var countingIn = false

  private var shown: Option[Engraving] = None
  private var positionMillis = 0.0
  private var written: Option[Tempo] = None
  private var chosen = 0

  bus.commands.events.foreach {
    case SetMetronome(now) =>
      on = now
      bus.publish(MetronomeSet(now))
      follow(leadSeconds = Metronome.leadSeconds)
    case ConfigureMetronome(now) =>
      settings = now
      metronome.settings = now
      RememberedSettings.metronome = now
      bus.publish(MetronomeConfigured(now))
    case _ => ()
  }(using unsafeWindowOwner)

  bus.events.events.foreach {
    case opened: ScoreOpened =>
      written = Some(opened.tempo)
      chosen = opened.tempo.beatsPerMinute
    case shownNow: ScoreShown         => shown = Some(shownNow.engraving)
    case TempoSet(beatsPerMinute)     => chosen = beatsPerMinute
    case PlaybackMoved(seconds, _, _) => positionMillis = seconds * 1000
    case PlaybackStopped              =>
      positionMillis = 0
      playing = false
      follow(Metronome.leadSeconds)
    case PlaybackStarted =>
      playing = true
      follow(Metronome.leadSeconds)
    case PlaybackPaused =>
      playing = false
      follow(Metronome.leadSeconds)
    case RecordingCountingIn =>
      countingIn = true
      follow(Metronome.leadSeconds)
    // the count-in's last beat is over: the measure starts now
    case _: RecordingStarted =>
      countingIn = false
      follow(Metronome.startNow)
    case RecordingReviewing | _: RequestFailed =>
      if (countingIn) {
        countingIn = false
        follow(Metronome.leadSeconds)
      }
    case _ => ()
  }(using unsafeWindowOwner)

  // Beats on its own when it should, and only then.
  private def follow(leadSeconds: Double): Unit = {
    if (on && !playing && !countingIn) {
      metronome.steady(() => beats, () => beatSeconds, leadSeconds)
    } else if (metronome.steadying) {
      metronome.cancel()
    }
  }

  private def bar = shown.flatMap(_.barAt(positionMillis))

  private def beats: Int = bar.fold(4)(_.beats)

  // a beat of the measure where the score stands, at the tempo chosen
  private def beatSeconds: Double =
    (bar, written) match {
      case (Some(measure), Some(tempo)) if chosen > 0 =>
        measure.beatMillis / 1000 * tempo.beatsPerMinute / chosen
      case _ => 60.0 / math.max(1, settings.beatsPerMinute)
    }
}
