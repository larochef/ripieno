package ripieno.frontend.sound

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.AppResult
import ripieno.frontend.utils.AppStreams

import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Listens to the microphone for the pitch of one note at a time, for tuning:
  * YIN (de Cheveigné and Kawahara, 2002) over the last 85 milliseconds.
  */
final class PitchListener(microphone: js.Dynamic) {
  private val context = new dom.AudioContext()
  private val analyser = context.createAnalyser()
  analyser.fftSize = PitchListener.window
  context
    .asInstanceOf[js.Dynamic]
    .createMediaStreamSource(microphone)
    .connect(analyser)
  private val samples = new Float32Array(PitchListener.window)
  private val rate = context.sampleRate

  /** The pitch heard now, in hertz: none in silence, or where no one pitch
    * stands out.
    */
  def pitch: Option[Double] = {
    analyser.asInstanceOf[js.Dynamic].getFloatTimeDomainData(samples)
    val size = samples.length / 2
    var energy = 0.0
    for (k <- 0 until samples.length) {
      energy += samples(k) * samples(k)
    }
    if (math.sqrt(energy / samples.length) < PitchListener.silence) None
    else {
      val shortest = math.max(2, (rate / PitchListener.highest).toInt)
      val longest = math.min(size - 1, (rate / PitchListener.lowest).toInt)
      // the difference of the sound with itself, lag by lag, each against the
      // mean of those before it
      val normalised = new Array[Double](longest + 2)
      normalised(0) = 1
      var running = 0.0
      for (lag <- 1 to longest + 1) {
        var difference = 0.0
        for (k <- 0 until size) {
          val d = samples(k) - samples(k + lag)
          difference += d * d
        }
        running += difference
        normalised(lag) = if (running > 0) difference * lag / running else 1
      }
      // the first dip under the threshold, followed to its bottom
      var lag = shortest
      while (lag <= longest && normalised(lag) >= PitchListener.threshold) {
        lag += 1
      }
      if (lag > longest) None
      else {
        while (lag + 1 <= longest && normalised(lag + 1) < normalised(lag)) {
          lag += 1
        }
        // between samples: the parabola through the bottom and its neighbours
        val (a, b, c) =
          (normalised(lag - 1), normalised(lag), normalised(lag + 1))
        val shift =
          if (a + c - 2 * b != 0) (a - c) / (2 * (a + c - 2 * b)) else 0.0
        Some(rate / (lag + shift))
      }
    }
  }

  /** Lets the microphone go. */
  def close(): Unit = {
    microphone
      .getTracks()
      .asInstanceOf[js.Array[js.Dynamic]]
      .foreach(_.stop())
    context.close()
  }
}

object PitchListener {
  private val window = 4096
  // a violin's open G down to a cello's open C, and up to the violin's top
  private val lowest = 60.0
  private val highest = 2500.0
  private val threshold = 0.15
  // under this level, in full scale: the room, not an instrument
  private val silence = 0.01

  /** Asks for the microphone, as `Recorder.open` does. */
  def open(): EventStream[AppResult[PitchListener]] = {
    val constraints = js.Dynamic.literal(
      audio = js.Dynamic.literal(
        echoCancellation = false,
        noiseSuppression = false,
        autoGainControl = false
      )
    )
    val devices = dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices
    if (js.isUndefined(devices)) {
      EventStream.fromValue(
        Left(
          Tr(
            "this page cannot use a microphone: it must be served over HTTPS or from this machine",
            "cette page ne peut pas utiliser le micro : elle doit être servie en HTTPS ou depuis cette machine"
          )
        )
      )
    } else {
      AppStreams
        .fromPromise(
          devices.getUserMedia(constraints).asInstanceOf[js.Promise[js.Dynamic]]
        )
        .map(_.map(PitchListener(_)))
    }
  }
}
