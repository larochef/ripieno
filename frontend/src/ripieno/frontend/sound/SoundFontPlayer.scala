package ripieno.frontend.sound

import ripieno.frontend.Libraries
import ripieno.frontend.model.AppResult
import ripieno.frontend.utils.AppStreams

import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

import com.raquo.laminar.api.L.*
import org.scalajs.dom.AudioContext

/** Plays MIDI through a SoundFont synthesizer running on the browser's audio
  * thread, which is also the clock everything shown follows.
  */
final class SoundFontPlayer(context: AudioContext, sequencer: js.Dynamic)
    extends ScorePlayer {
  sequencer.loopCount = 0

  // Where a song being loaded is to start from, and whether it plays: the
  // sequencer reads it on the audio thread, and whatever it is told before then
  // is lost. Until then, the player answers for it.
  final private case class Pending(seconds: Double, play: Boolean)
  private var pending: Option[Pending] = None
  // Where a paused sequencer was moved to: it only says so once the audio
  // thread has moved.
  private var movedTo: Option[Double] = None

  sequencer.eventHandler.addEvent(
    "songChange",
    "ripieno",
    (_: js.Any) => {
      pending.foreach { loaded =>
        pending = None
        sequencer.currentTime = loaded.seconds
        movedTo = Some(loaded.seconds)
        if (loaded.play) {
          play()
        } else {
          sequencer.pause()
        }
      }
    }
  )

  /** Plays that MIDI from now on, from `seconds` into it, playing or not. */
  def load(
      midi: Uint8Array,
      seconds: Double = 0,
      play: Boolean = false
  ): Unit = {
    // a copy: the sequencer hands the buffer it is given over to the audio thread
    val ownedByTheSequencer = new Uint8Array(midi).buffer
    pending = Some(Pending(seconds, play))
    // before the loading: its answer would otherwise come after the song is
    // there, and mark it paused while it plays
    sequencer.pause()
    sequencer.loadNewSongList(
      js.Array(
        js.Dynamic.literal(binary = ownedByTheSequencer, fileName = "score.mid")
      )
    )
  }

  def playing: Boolean =
    pending.fold(!sequencer.paused.asInstanceOf[Boolean])(_.play)

  def play(): Unit = {
    context.resume()
    movedTo = None
    pending match {
      case Some(loading) => pending = Some(loading.copy(play = true))
      case None          => sequencer.play()
    }
  }

  def pause(): Unit = {
    pending match {
      case Some(loading) => pending = Some(loading.copy(play = false))
      case None          => sequencer.pause()
    }
  }

  def stop(): Unit = {
    pending match {
      case Some(_) => pending = Some(Pending(0, play = false))
      case None    =>
        sequencer.pause()
        sequencer.currentTime = 0
        movedTo = Some(0)
    }
  }

  /** Moves to that many seconds into the score. */
  def seek(seconds: Double): Unit = {
    pending match {
      case Some(loading) => pending = Some(loading.copy(seconds = seconds))
      case None          =>
        sequencer.currentTime = seconds
        if (!playing) {
          movedTo = Some(seconds)
        }
    }
  }

  def speed: Double = sequencer.playbackRate.asInstanceOf[Double]

  def speed_=(rate: Double): Unit = {
    sequencer.playbackRate = rate
  }

  /** How far into the score the sound coming out of the speakers is, in seconds
    * at the score's own tempo: behind the sequencer by what the browser reports
    * its output takes.
    */
  def position: Double = pending.fold {
    if (playing) {
      math.max(
        0,
        sequencer.currentHighResolutionTime
          .asInstanceOf[Double] - outputDelay * speed
      )
    } else {
      movedTo.getOrElse(sequencer.currentTime.asInstanceOf[Double])
    }
  }(_.seconds)

  def onEnded(action: () => Unit): Unit = {
    sequencer.eventHandler.addEvent(
      "songEnded",
      "ripieno",
      (_: js.Any) => action()
    )
  }

  private def outputDelay: Double = {
    val audio = context.asInstanceOf[js.Dynamic]
    Seq(audio.baseLatency, audio.outputLatency)
      .map(_.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0))
      .filter(_ > 0)
      .sum
  }
}

object SoundFontPlayer {

  // the page's audio for the sound bank's synthesizer: made before any click,
  // asleep until one wakes it
  private lazy val context = new AudioContext()

  /** Wakes the page's audio: a click is the one thing the browser lets do it,
    * and the synthesizer being prepared may wait for it.
    */
  def wake(): Unit = context.resume()

  /** Prepares the synthesizer with its sound bank, asleep until a click wakes
    * it (`wake`, `play`).
    */
  def open(soundBank: ArrayBuffer): EventStream[AppResult[SoundFontPlayer]] = {
    val synthesizer = EventStream.fromValue(()).flatMapSwitch { _ =>
      AppStreams.fromPromise(
        promise(
          context
            .asInstanceOf[js.Dynamic]
            .audioWorklet
            .addModule(Libraries.processorUrl)
        )
      )
    }
    synthesizer.flatMapSwitch {
      case Left(reason) => EventStream.fromValue(Left(reason))
      case Right(_)     =>
        val synthesizer =
          new Libraries.WorkletSynthesizer(context).asInstanceOf[js.Dynamic]
        synthesizer.connect(context.destination)
        AppStreams
          .fromPromise(
            promise(
              synthesizer.soundBankManager.addSoundBank(soundBank, "main")
            )
          )
          .flatMapSwitch {
            case Left(reason) => EventStream.fromValue(Left(reason))
            case Right(_)     =>
              AppStreams
                .fromPromise(promise(synthesizer.isReady))
                .map(_.map(_ => sequenced(context, synthesizer)))
          }
    }
  }

  private def promise(value: js.Dynamic): js.Promise[Any] =
    value.asInstanceOf[js.Promise[Any]]

  private def sequenced(
      context: AudioContext,
      synthesizer: js.Dynamic
  ): SoundFontPlayer = {
    // the score's time starts at zero, not at its first note
    val options = js.Dynamic.literal(skipToFirstNoteOn = false)
    SoundFontPlayer(
      context,
      new Libraries.Sequencer(synthesizer, options).asInstanceOf[js.Dynamic]
    )
  }
}
