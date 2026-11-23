package ripieno.frontend.sound

import ripieno.frontend.model.{MetronomeSettings, MetronomeSound}

import org.scalajs.dom
import org.scalajs.dom.{AudioContext, OscillatorNode}

/** Beats measures before playback starts, so that whoever plays along starts
  * with it, beats with the music, or beats on its own. Its clicks are scheduled
  * on the audio clock; what starts after them is started by a timer, a few
  * milliseconds off at most.
  */
final class Metronome {
  // made on the first count, which follows a click: the browser allows it then
  private lazy val context = new AudioContext()
  private var clicks: Seq[OscillatorNode] = Seq.empty
  private var timer: Option[Int] = None
  private var steadyTimer: Option[Int] = None

  /** How the clicks sound from now on. */
  var settings: MetronomeSettings = MetronomeSettings.default

  def counting: Boolean = timer.isDefined

  /** Beats `measures` measures of `beats` beats of `beatSeconds`, the first of
    * each stressed, then calls `andThen`: `earlier` seconds before the end of
    * the count, for music that enters before the downbeat (a pickup).
    */
  def countIn(
      measures: Int,
      beats: Int,
      beatSeconds: Double,
      earlier: Double = 0
  )(
      andThen: () => Unit
  ): Unit = {
    cancel()
    context.resume()
    val first = context.currentTime + Metronome.leadSeconds
    clicks = (0 until measures * beats).map(beat =>
      click(first + beat * beatSeconds, stressed = beat % beats == 0)
    )
    val untilFirstNote =
      math.max(
        0,
        Metronome.leadSeconds + measures * beats * beatSeconds - earlier
      )
    timer = Some(
      dom.window.setTimeout(
        () => {
          timer = None
          clicks = Seq.empty
          andThen()
        },
        untilFirstNote * 1000
      )
    )
  }

  /** Beats with the music: a click at each of `beats`, seconds from now, the
    * stressed ones higher.
    */
  def beat(beats: Seq[(Double, Boolean)]): Unit = {
    cancel()
    context.resume()
    val now = context.currentTime
    clicks = beats.map((after, stressed) => click(now + after, stressed))
  }

  /** Beats on its own until cancelled, from `leadSeconds` from now:
    * `beatsPerMeasure()` beats a measure of `beatSeconds()` each, asked again
    * at every beat, so that a new tempo or time signature is followed from the
    * next beat on.
    */
  def steady(
      beatsPerMeasure: () => Int,
      beatSeconds: () => Double,
      leadSeconds: Double = Metronome.leadSeconds
  ): Unit = {
    cancel()
    context.resume()
    var next = context.currentTime + leadSeconds
    var beat = 0
    // scheduled a little ahead, by a timer that may run late
    def schedule(): Unit = {
      while (next < context.currentTime + Metronome.aheadSeconds) {
        val beats = math.max(1, beatsPerMeasure())
        clicks = clicks :+ click(next, beat % beats == 0)
        beat = (beat + 1) % beats
        next += math.max(0.05, beatSeconds())
      }
      // the clicks heard already need no stopping
      if (clicks.sizeIs > Metronome.keptClicks) {
        clicks = clicks.takeRight(Metronome.keptClicks)
      }
    }
    schedule()
    steadyTimer = Some(
      dom.window.setInterval(() => schedule(), Metronome.scheduleMillis)
    )
  }

  def steadying: Boolean = steadyTimer.isDefined

  def cancel(): Unit = {
    timer.foreach(dom.window.clearTimeout)
    timer = None
    steadyTimer.foreach(dom.window.clearInterval)
    steadyTimer = None
    clicks.foreach(_.stop())
    clicks = Seq.empty
  }

  /** A note of `hertz` for `seconds`, to tune to. */
  def tone(hertz: Double, seconds: Double): Unit = {
    context.resume()
    val at = context.currentTime + Metronome.leadSeconds
    val tone = context.createOscillator()
    tone.frequency.value = hertz
    val level = context.createGain()
    level.gain.setValueAtTime(0.0001, at)
    level.gain.exponentialRampToValueAtTime(0.3, at + 0.05)
    level.gain.setValueAtTime(0.3, at + seconds - 0.2)
    level.gain.exponentialRampToValueAtTime(0.0001, at + seconds)
    tone.connect(level)
    level.connect(context.destination)
    tone.start(at)
    tone.stop(at + seconds)
  }

  private def click(at: Double, stressed: Boolean): OscillatorNode = {
    val accented = stressed && settings.accent
    val (shape, low, high, seconds, loudness) = settings.sound match {
      case MetronomeSound.Click     => ("sine", 1320.0, 1760.0, 0.04, 0.4)
      case MetronomeSound.Woodblock => ("triangle", 880.0, 1175.0, 0.06, 0.6)
      case MetronomeSound.Clave     => ("sine", 2500.0, 3150.0, 0.025, 0.5)
      case MetronomeSound.Beep      => ("square", 660.0, 880.0, 0.08, 0.15)
      case MetronomeSound.Soft      => ("sine", 440.0, 587.0, 0.09, 0.5)
    }
    val tone = context.createOscillator()
    tone.`type` = shape
    tone.frequency.value = if (accented) high else low
    val level = context.createGain()
    val volume = settings.volume / 100.0 * loudness * (if (accented) 1.4 else 1)
    level.gain.setValueAtTime(math.max(0.0001, volume), at)
    level.gain.exponentialRampToValueAtTime(0.0001, at + seconds)
    tone.connect(level)
    level.connect(context.destination)
    tone.start(at)
    tone.stop(at + seconds)
    tone
  }
}

object Metronome {
  // room for the first click to be scheduled ahead of the audio thread
  val leadSeconds = 0.1
  // as soon as can be: on a beat that is due now
  val startNow = 0.005
  // how far ahead the steady beat is scheduled, and how often
  val aheadSeconds = 0.3
  val scheduleMillis = 50
  val keptClicks = 16
}
