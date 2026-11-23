package ripieno.frontend.sound

import ripieno.shared.AlignmentPoint

import org.scalajs.dom.document
import org.scalajs.dom.html.Audio

/** Plays back a recording of the player, its time turned into the score's by
  * the review's alignment, so that the sheet follows what they played.
  */
final class RecordingPlayer(url: String, alignment: List[AlignmentPoint])
    extends ScorePlayer {
  private val audio = document.createElement("audio").asInstanceOf[Audio]
  audio.preload = "auto"
  audio.src = url

  // in seconds, both increasing
  private val recording = alignment.map(_.recording / 1000).toVector
  private val score = alignment.map(_.score / 1000).toVector

  private def interpolated(
      x: Double,
      from: Vector[Double],
      to: Vector[Double]
  ): Double = {
    if (from.isEmpty) x
    else if (x <= from.head) to.head
    else if (x >= from.last) to.last
    else {
      val after = from.indexWhere(_ > x)
      val share = (x - from(after - 1)) / (from(after) - from(after - 1))
      to(after - 1) + share * (to(after) - to(after - 1))
    }
  }

  override def playing: Boolean = !audio.paused

  override def play(): Unit = {
    audio.play()
  }

  override def pause(): Unit = {
    audio.pause()
  }

  override def stop(): Unit = {
    audio.pause()
    audio.currentTime = recording.headOption.getOrElse(0.0)
  }

  override def seek(seconds: Double): Unit = {
    audio.currentTime = interpolated(seconds, score, recording)
  }

  // The browser keeps the pitch when the speed changes.
  override def speed: Double = audio.playbackRate

  override def speed_=(rate: Double): Unit = {
    audio.playbackRate = rate
  }

  override def position: Double =
    interpolated(audio.currentTime, recording, score)

  override def onEnded(action: () => Unit): Unit = {
    audio.onended = _ => action()
  }
}
