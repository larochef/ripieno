package ripieno.backend.sound

import ripieno.backend.models.{Download, Downloads}
import ripieno.backend.sound.salamander.SalamanderRendering
import ripieno.backend.sound.violet.VioletRendering

import java.io.File
import java.nio.file.*

/** A way the server renders MIDI to sound, by its name on the wire. */
sealed trait RenderingEngine {
  def name: String

  /** What its renderings were made by, beside the MIDI: a rendering made by
    * another version is made again.
    */
  def version: String = ""

  /** What it plays with that is downloaded: before it first renders.
    */
  def needs: Seq[Download] = Seq.empty

  /** Renders `midi` into `sound`, working in `work`, writing to `log`; `run`
    * runs commands at once, telling whether they all ended well.
    */
  def render(
      midi: Path,
      sound: Path,
      work: Path,
      log: Path,
      run: Seq[Seq[String]] => Boolean
  ): Boolean
}

object RenderingEngine {

  /** The page's own sound bank played by FluidSynth, as the page plays it: a
    * few seconds for a piece.
    */
  final case class Sampled(downloads: Downloads) extends RenderingEngine {
    def name: String = "sampled"

    override def needs: Seq[Download] = Seq(Download.SoundBank)

    def render(
        midi: Path,
        sound: Path,
        work: Path,
        log: Path,
        run: Seq[Seq[String]] => Boolean
    ): Boolean = {
      val partial = sound.resolveSibling(s"${sound.getFileName}.part.wav")
      // rendered to the file only: the audio driver it would open otherwise
      // is the machine's sound card
      run(
        Seq(
          Seq(
            "fluidsynth",
            "-ni",
            "-q",
            "-a",
            "file",
            "-r",
            "48000",
            "-F",
            partial.toString,
            downloads.soundBank.toString,
            midi.toString
          )
        )
      ) && {
        Files.move(
          partial,
          sound,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
        true
      }
    }
  }

  /** VIOLET, the runner sampling violin lines on the GPU, the rest done here;
    * its other parts rendered by MIDI-DDSP. `windows` keeps the windows it
    * sampled.
    */
  final case class Violet(
      runner: Runner,
      downloads: Downloads,
      windows: Path
  ) extends RenderingEngine {
    def name: String = "violet"

    // 4: each note leveled by its loudest moment, played at its velocity as
    // VIOLET's dynamics
    override def version: String = "4"

    override def needs: Seq[Download] =
      Seq(Download.Rocm, Download.Violet, Download.MidiDdsp)

    def render(
        midi: Path,
        sound: Path,
        work: Path,
        log: Path,
        run: Seq[Seq[String]] => Boolean
    ): Boolean =
      VioletRendering.render(
        runner,
        downloads,
        windows,
        midi,
        sound,
        work,
        log,
        run
      )
  }

  /** MIDI-DDSP, the runner playing a part at a time on the processor, the rest
    * done here (`MidiDdspRendering`).
    */
  final case class MidiDdsp(runner: Runner, downloads: Downloads)
      extends RenderingEngine {
    def name: String = "midi-ddsp"

    // 2: in the runner, a part on its own, its draws from its notes
    override def version: String = "2"

    override def needs: Seq[Download] = Seq(Download.MidiDdsp)

    def render(
        midi: Path,
        sound: Path,
        work: Path,
        log: Path,
        run: Seq[Seq[String]] => Boolean
    ): Boolean = {
      val (parts, sounds) = (work.resolve("parts"), work.resolve("sounds"))
      val count = MidiDdspRendering.prepared(
        MidiFile.read(Files.readAllBytes(midi)).instruments,
        parts
      )
      require(count > 0, "the score has no note to render")
      run(
        Seq(
          MidiDdspRendering.command(
            runner,
            downloads.folder(Download.MidiDdsp),
            parts,
            sounds
          )
        )
      ) && {
        val partial = sound.resolveSibling(s"${sound.getFileName}.part")
        Files.write(
          partial,
          Wav.pcm16(Vector(MidiDdspRendering.mixed(sounds, count)), 48000)
        )
        Files.move(
          partial,
          sound,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
        true
      }
    }
  }

  /** The Salamander Grand Piano's samples, the other parts played with the
    * page's sound bank.
    */
  final case class Salamander(downloads: Downloads) extends RenderingEngine {
    def name: String = "salamander"

    override def needs: Seq[Download] = Seq(Download.Piano, Download.SoundBank)

    def render(
        midi: Path,
        sound: Path,
        work: Path,
        log: Path,
        run: Seq[Seq[String]] => Boolean
    ): Boolean =
      SalamanderRendering.render(
        downloads.folder(Download.Piano),
        Some(downloads.soundBank),
        midi,
        sound,
        work,
        log,
        run
      )
  }

  /** The engines this server offers: those of the runner when the build gave it
    * one (VIOLET only on the GPU its kernels are for), and FluidSynth's (the
    * sampled sound, and the rest of a score with the piano) when it is
    * installed. What they play with downloads when first rendered with.
    */
  def installed(
      runner: Option[Runner],
      downloads: Downloads,
      windows: Path
  ): List[RenderingEngine] = {
    val played = runner.toList.flatMap(runner =>
      MidiDdsp(runner, downloads) +:
        Option.when(Runner.hasGpu())(Violet(runner, downloads, windows)).toList
    )
    val synthesised = Option
      .when(hasFluidSynth)(Seq(Sampled(downloads), Salamander(downloads)))
      .getOrElse(Nil)
    (played ++ synthesised).sortBy(_.name)
  }

  // FluidSynth among the commands of the PATH
  private def hasFluidSynth: Boolean =
    sys.env
      .getOrElse("PATH", "")
      .split(File.pathSeparator)
      .exists(folder => Files.isExecutable(Path.of(folder, "fluidsynth")))
}
