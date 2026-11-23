package ripieno.backend.sound.salamander

import ripieno.backend.sound.*

import java.nio.charset.StandardCharsets
import java.nio.file.*

/** A score's MIDI rendered with its piano parts on the Salamander Grand Piano
  * (`SalamanderPiano`), its other parts by FluidSynth with the page's own sound
  * bank, then mixed. Processor only, a few seconds for a piece.
  */
object SalamanderRendering {

  // the samples' level: a full score of chords at ff would clip without it
  private val gain = math.pow(10, -7 / 20.0).toFloat
  // a mix that would clip peaks at -1 dB of full scale
  private val peak = math.pow(10, -1 / 20.0).toFloat

  /** Renders `midi` into `sound`, working in `work`; `run` runs commands at
    * once, telling whether they all ended well.
    */
  def render(
      samples: Path,
      bank: Option[Path],
      midi: Path,
      sound: Path,
      work: Path,
      log: Path,
      run: Seq[Seq[String]] => Boolean
  ): Boolean = {
    val score = MidiFile.read(Files.readAllBytes(midi))
    val piano = SalamanderPiano.played(score, samples.resolve("Samples"))
    Files.writeString(
      log,
      s"${piano.notes} piano notes from the samples, ${piano.others.size} other parts for FluidSynth\n",
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.APPEND
    )
    Files.createDirectories(work)
    val othersMidi = work.resolve("others.mid")
    val othersSound = work.resolve("others.wav")
    val othersRendered = piano.others.isEmpty || {
      val soundBank = bank.getOrElse(
        throw IllegalStateException("the other parts need a sound bank")
      )
      // the times kept as they are: the other parts line up with the piano
      val resolution = score.timing.resolution
      Files.write(
        othersMidi,
        MidiFile(
          piano.others,
          Vector.empty,
          MidiTiming(resolution, Vector(0 -> 60.0 / (120.0 * resolution)), 0)
        ).bytes
      )
      // rendered to the file only: the audio driver it would open otherwise is
      // the machine's sound card
      run(
        Seq(
          Seq(
            "fluidsynth",
            "-ni",
            "-q",
            "-a",
            "file",
            "-r",
            SalamanderPiano.rate.toString,
            "-F",
            othersSound.toString,
            soundBank.toString,
            othersMidi.toString
          )
        )
      )
    }
    othersRendered && {
      val parts = piano.sound +: Option
        .when(piano.others.nonEmpty)(
          stereo(Wav.read(Files.readAllBytes(othersSound)))
        )
        .toVector
      val partial = sound.resolveSibling(s"${sound.getFileName}.part")
      Files.write(partial, Wav.pcm16(mixed(parts), SalamanderPiano.rate))
      Files.move(
        partial,
        sound,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      true
    }
  }

  /** The parts summed at a fixed gain, so that the velocities the page gives
    * (its Conductor's dynamics and balance) stay heard as such; only a mix that
    * would clip is brought down.
    */
  def mixed(parts: Vector[Vector[Array[Float]]]): Vector[Array[Float]] = {
    val length = parts.flatMap(_.map(_.length)).maxOption.getOrElse(0)
    val mix = Vector.fill(2)(new Array[Float](length))
    for {
      part <- parts
      (channel, rank) <- part.zipWithIndex
      index <- channel.indices
    } {
      mix(rank)(index) += channel(index)
    }
    for {
      channel <- mix
      index <- channel.indices
    } channel(index) *= gain
    val loudest = mix
      .map(channel =>
        channel.foldLeft(0.0f)((found, value) =>
          math.max(found, math.abs(value))
        )
      )
      .max
    if (loudest > peak) {
      val factor = peak / loudest
      for {
        channel <- mix
        index <- channel.indices
      } channel(index) *= factor
    }
    mix
  }

  /** A part's sound at the piano's rate, in stereo (a mono one in both). */
  def stereo(wav: Wav): Vector[Array[Float]] = {
    val channels =
      if (wav.rate == SalamanderPiano.rate) wav.channels
      else
        wav.channels.map(
          Resampling.resampled(_, SalamanderPiano.rate, wav.rate)
        )
    if (channels.size == 2) channels else Vector(channels.head, channels.head)
  }
}
