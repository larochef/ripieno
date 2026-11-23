package ripieno.frontend.sound

import ripieno.frontend.model.AppResult
import ripieno.frontend.utils.AppStreams

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.*

/** Plays the parts of a score rendered one by one, together: their sounds are
  * started at the same instant of one audio clock, so that they stay together
  * to the sample, each through its own level.
  *
  * The parts were rendered at `renderedRate` times the written tempo, and play
  * at that tempo only: faster or slower, their pitch would change with it. Each
  * part is brought to the same loudness first; its level is a share of that.
  */
final class StemsPlayer private (
    context: AudioContext,
    buffers: Vector[AudioBuffer],
    val renderedRate: Double
) extends ScorePlayer {
  private val output = context.createDynamicsCompressor()
  // a limiter more than a compressor: only the loudest moments of the parts
  // together are held back
  output.threshold.value = -3
  output.ratio.value = 12
  output.connect(context.destination)

  // each part to the same loudness, then its level
  private val loudness: Vector[Double] = buffers.map(StemsPlayer.matched)
  private val gains: Vector[GainNode] = buffers.indices.toVector.map { _ =>
    val gain = context.createGain()
    gain.connect(output)
    gain
  }
  private var levels: Vector[Double] = Vector.fill(buffers.size)(1.0)

  private val lengthSeconds = buffers.map(_.duration).maxOption.getOrElse(0.0)
  private var sources: Vector[AudioBufferSourceNode] = Vector.empty
  private var ended: () => Unit = () => ()
  // where in the sound playback stands while paused, in its own seconds
  private var paused = 0.0
  // the audio clock's time at which the sound's start would have played
  private var startedAt = 0.0
  private var playbackRate = 1.0

  setLevels(levels)

  /** Each part's level, from 0 (silent) to 1. */
  def setLevels(each: Vector[Double]): Unit = {
    levels = each
    gains.indices.foreach(index =>
      gains(index).gain.value =
        levels.lift(index).getOrElse(1.0) * loudness(index)
    )
  }

  override def playing: Boolean = sources.nonEmpty

  // in the sound's own seconds
  private def soundTime: Double =
    if (playing) (context.currentTime - startedAt) * playbackRate else paused

  override def play(): Unit = {
    if (!playing && paused < lengthSeconds) {
      context.resume()
      val at = context.currentTime + StemsPlayer.leadSeconds
      startedAt = at - paused / playbackRate
      sources = buffers.zip(gains).map { (buffer, gain) =>
        val source = context.createBufferSource()
        source.buffer = buffer
        source.playbackRate.value = playbackRate
        source.connect(gain)
        source.start(at, math.min(paused, buffer.duration))
        source
      }
      // the longest part's end is the score's
      sources
        .zip(buffers)
        .maxByOption(_._2.duration)
        .foreach((longest, _) =>
          longest.onended = _ =>
            if (sources.contains(longest)) {
              halt()
              paused = 0
              ended()
            }
        )
    }
  }

  private def halt(): Unit = {
    val stopping = sources
    sources = Vector.empty
    stopping.foreach { source =>
      source.onended = null
      source.stop()
      source.disconnect()
    }
  }

  override def pause(): Unit = {
    if (playing) {
      paused = soundTime
      halt()
    }
  }

  override def stop(): Unit = {
    halt()
    paused = 0
  }

  override def seek(seconds: Double): Unit = {
    val wasPlaying = playing
    halt()
    paused = math.max(0, seconds / renderedRate)
    if (wasPlaying) play()
  }

  override def speed: Double = playbackRate * renderedRate

  override def speed_=(rate: Double): Unit = {
    val now = soundTime
    playbackRate = rate / renderedRate
    sources.foreach(_.playbackRate.value = playbackRate)
    if (playing) {
      startedAt = context.currentTime - now / playbackRate
    }
  }

  override def position: Double =
    math.max(
      0,
      (if (playing) soundTime - outputDelay * playbackRate else paused) *
        renderedRate
    )

  override def onEnded(action: () => Unit): Unit = {
    ended = action
  }

  /** Lets its sound go: it is not played again. */
  def close(): Unit = {
    stop()
    gains.foreach(_.disconnect())
    output.disconnect()
  }

  private def outputDelay: Double = {
    val audio = context.asInstanceOf[js.Dynamic]
    Seq(audio.baseLatency, audio.outputLatency)
      .map(_.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0))
      .filter(_ > 0)
      .sum
  }
}

object StemsPlayer {
  // room for the parts to be scheduled together ahead of the audio thread
  private val leadSeconds = 0.05
  // the loudness each part is brought to, in full scale (about -18 dB)
  private val loudnessTarget = 0.125

  // one context for all of them: a browser allows only a few
  private lazy val context = new AudioContext()

  /** Fetches and decodes the parts' sounds, in score order. */
  def open(
      urls: Vector[String],
      renderedRate: Double
  ): EventStream[AppResult[StemsPlayer]] = {
    val decoded = js.Promise.all(
      js.Array(urls.map { url =>
        dom
          .fetch(url)
          .`then`[ArrayBuffer](response => response.arrayBuffer())
          .`then`[AudioBuffer](bytes => context.decodeAudioData(bytes))
      }*)
    )
    AppStreams
      .fromPromise(decoded)
      .map(
        _.map(buffers => StemsPlayer(context, buffers.toVector, renderedRate))
      )
  }

  // The gain bringing a part's sound to the target loudness: its root mean
  // square over the moments it sounds, a sample in sixteen.
  private def matched(buffer: AudioBuffer): Double = {
    val samples = buffer.getChannelData(0)
    var sum = 0.0
    var counted = 0
    var index = 0
    while (index < samples.length) {
      val value = samples(index).toDouble
      if (math.abs(value) > 1e-3) {
        sum += value * value
        counted += 1
      }
      index += 16
    }
    if (counted == 0) 1.0
    else math.min(8.0, loudnessTarget / math.sqrt(sum / counted))
  }
}
