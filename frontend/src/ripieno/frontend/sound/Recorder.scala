package ripieno.frontend.sound

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.AppResult
import ripieno.frontend.utils.AppStreams

import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Float32Array}

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Records the microphone, compressed by the browser (Opus). The sound is taken
  * as it comes: no echo cancelling, noise suppression or gain control, which
  * would bend the dynamics and the attacks to be reviewed.
  */
final class Recorder(microphone: js.Dynamic) {
  private val recorder =
    js.Dynamic.newInstance(js.Dynamic.global.MediaRecorder)(microphone)
  private val chunks = js.Array[js.Any]()
  recorder.ondataavailable = (event: js.Dynamic) => chunks.push(event.data)

  // what the microphone hears now, for a level meter
  private val context = new dom.AudioContext()
  private val analyser = context.createAnalyser()
  analyser.fftSize = 2048
  context
    .asInstanceOf[js.Dynamic]
    .createMediaStreamSource(microphone)
    .connect(analyser)
  private val samples = new Float32Array(analyser.fftSize)

  /** The loudest sample of the last few tens of milliseconds, from 0 to 1. */
  def peak: Double = {
    analyser.asInstanceOf[js.Dynamic].getFloatTimeDomainData(samples)
    var loudest = 0.0
    for (index <- 0 until samples.length) {
      loudest = math.max(loudest, math.abs(samples(index).toDouble))
    }
    loudest
  }

  def start(): Unit = {
    recorder.start()
  }

  /** Lets the microphone go without recording: after a level check. */
  def close(): Unit = {
    microphone
      .getTracks()
      .asInstanceOf[js.Array[js.Dynamic]]
      .foreach(_.stop())
    context.close()
  }

  /** Stops, and gives the whole recording; the microphone is let go. */
  def stop(): EventStream[AppResult[ArrayBuffer]] = {
    val stopped = js.Promise[Unit]((resolve, _) => {
      recorder.onstop = (_: js.Any) => resolve(())
    })
    recorder.stop()
    AppStreams.fromPromise(stopped).flatMapSwitch {
      case Left(reason) => EventStream.fromValue(Left(reason))
      case Right(_)     =>
        microphone
          .getTracks()
          .asInstanceOf[js.Array[js.Dynamic]]
          .foreach(_.stop())
        context.close()
        val blob = new dom.Blob(chunks.asInstanceOf[js.Array[dom.BlobPart]])
        AppStreams.fromPromise(blob.arrayBuffer())
    }
  }
}

object Recorder {

  /** Asks for the microphone, which the browser only gives a page served over
    * HTTPS or from this machine, and after a click.
    */
  def open(): EventStream[AppResult[Recorder]] = {
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
        .map(_.map(Recorder(_)))
    }
  }
}
