package ripieno.frontend.appServices

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.sound.{Metronome, PitchListener}
import ripieno.frontend.utils.{AppStreams, RememberedSettings}

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Listens to the microphone for the pitch played, a few times a second, and
  * sounds notes to tune to, at the A chosen.
  */
class TunerService(bus: AppBus) {
  private var listener: Option[PitchListener] = None
  private var opening = false
  private var reference = RememberedSettings.referencePitch
  // what sounds the notes to tune to
  private val sound = Metronome()
  private val listenMillis = 100
  private val toneSeconds = 2.5

  bus.commands.events.foreach {
    case command @ SetTuner(true) if listener.isEmpty && !opening =>
      opening = true
      AppStreams.runOnce(PitchListener.open()) { opened =>
        opening = false
        opened match {
          case Left(reason) =>
            bus.publish(
              RequestFailed(
                command,
                Tr(s"No microphone: $reason", s"Pas de micro : $reason")
              )
            )
          case Right(listening) =>
            listener = Some(listening)
            bus.publish(TunerSet(true))
            listen(listening)
        }
      }
    case SetTuner(false) =>
      listener.foreach(_.close())
      listener = None
      bus.publish(TunerSet(false))
      bus.publish(PitchHeard(None))
    case SetReferencePitch(hertz) =>
      reference = hertz
      RememberedSettings.referencePitch = hertz
      bus.publish(ReferencePitchSet(hertz))
    case SetTunerInstrument(instrument) =>
      RememberedSettings.tunerInstrument = instrument
      bus.publish(TunerInstrumentSet(instrument))
    case PlayReferenceNote(pitch) =>
      sound.tone(reference * math.pow(2, (pitch - 69) / 12.0), toneSeconds)
    case _ => ()
  }(using unsafeWindowOwner)

  // For as long as this listener is the one listening.
  private def listen(listening: PitchListener): Unit = {
    if (listener.exists(_ eq listening)) {
      bus.publish(PitchHeard(listening.pitch))
      dom.window.setTimeout(() => listen(listening), listenMillis)
    }
  }
}
