package ripieno.backend.sound

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** A score's parts played by MIDI-DDSP (the runner's `MidiDdspRendering`), on
  * the processor: each part a file of notes, each played as its instrument;
  * then all of them mixed, from the model's 16 kHz to the page's 48.
  */
object MidiDdspRendering {

  val Rate = 16000
  private val pageRate = 48000

  // the programs MIDI-DDSP models: bowed strings, woodwinds, brass
  private val modelled = Set(40, 41, 42, 43, 73, 68, 71, 66, 70, 56, 60, 57, 58)
  // the bowed strings and their lowest notes, the highest string first
  private val strings = Vector(55 -> 40, 48 -> 41, 36 -> 42)
  private val contrabass = 43

  /** The program a part is played with: its own when the model knows it, else
    * the highest bowed string that reaches its lowest note, which keeps every
    * note where the score puts it.
    */
  def programOf(instrument: MidiInstrument): Int =
    if (!instrument.isDrum && modelled.contains(instrument.program))
      instrument.program
    else {
      val lowest = instrument.notes.map(_.pitch).min
      strings
        .collectFirst { case (bottom, program) if lowest >= bottom => program }
        .getOrElse(contrabass)
    }

  /** Writes each part with notes as `<k>.notes` in `folder`: how many there
    * are.
    */
  def prepared(instruments: Vector[MidiInstrument], folder: Path): Int = {
    Files.createDirectories(folder)
    val played = instruments.filter(_.notes.nonEmpty)
    for ((instrument, index) <- played.zipWithIndex) {
      val notes = instrument.notes
        .map(note =>
          s"${note.pitch} ${note.velocity} ${note.start} ${note.end}\n"
        )
        .mkString
      Files.writeString(
        folder.resolve(f"$index%03d.notes"),
        s"program ${programOf(instrument)}\n$notes",
        StandardCharsets.UTF_8
      )
    }
    played.size
  }

  /** The parts' sounds in `folder` added up, at the page's rate. */
  def mixed(folder: Path, parts: Int): Array[Float] = {
    val sounds = (0 until parts).map(index =>
      Wav.read(Files.readAllBytes(folder.resolve(f"$index%03d.wav")))
    )
    sounds.foreach(sound =>
      require(sound.rate == Rate, s"MIDI-DDSP's sound at ${sound.rate} Hz")
    )
    val length = sounds.map(_.channels.head.length).maxOption.getOrElse(0)
    val sum = new Array[Double](length)
    for {
      sound <- sounds
      (value, index) <- sound.mono.zipWithIndex
    } sum(index) += value
    Resampling.resampled(sum.map(_.toFloat), pageRate, Rate)
  }

  /** The command that renders the parts of `parts` into `sounds`. */
  /** The runner's command playing the parts of `parts` into `sounds`, from
    * MIDI-DDSP's `weights` folder.
    */
  def command(
      runner: Runner,
      weights: Path,
      parts: Path,
      sounds: Path
  ): Seq[String] =
    runner.command(
      "MidiDdspRendering",
      Seq(weights, parts, sounds).map(_.toString),
      rocm = None
    )
}
