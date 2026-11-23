package ripieno.backend.sound.violet

import ripieno.backend.models.{Download, Downloads}
import ripieno.backend.sound.*

import java.nio.charset.StandardCharsets
import java.nio.file.*

/** A score's MIDI rendered with VIOLET: its violin parts cut into lines
  * (`VioletLines`), each told its strokes (`VioletBowing`) and rendered by the
  * runner (`ViolinSampling`), its other parts rendered by MIDI-DDSP meanwhile
  * (`MidiDdspRendering`), the processor's work while the violins are on the
  * GPU; then all of it mixed (`VioletMix`).
  */
object VioletRendering {

  // what VIOLET's sound is late by on the notes it is given (measured on its
  // reference implementation)
  private val late = 0.08
  // the stroke of a short note the page says nothing of
  private val shortStroke = "bow"

  /** Renders `midi` into `sound`, working in `work`, the windows sampled kept
    * in `windows`; `run` runs commands at once, telling whether they all ended
    * well.
    */
  def render(
      runner: Runner,
      downloads: Downloads,
      windows: Path,
      midi: Path,
      sound: Path,
      work: Path,
      log: Path,
      run: Seq[Seq[String]] => Boolean
  ): Boolean = {
    val split = VioletLines.split(MidiFile.read(Files.readAllBytes(midi)))
    val linesFolder = Files.createDirectories(work.resolve("lines"))
    val sounds = work.resolve("sounds")
    val lines = split.lines.map { line =>
      val bowed =
        VioletBowing.bowed(MidiFile.read(line.file.bytes), shortStroke).bytes
      val read = MidiFile.read(bowed)
      Files.writeString(
        linesFolder.resolve(s"${line.name}.notes"),
        notesOf(read)
      )
      line.name -> read
    }
    val (othersParts, othersSounds) =
      (work.resolve("others"), work.resolve("others-sounds"))
    val otherParts = split.others.fold(0)(others =>
      MidiDdspRendering.prepared(others.instruments, othersParts)
    )
    Files.writeString(
      log,
      s"${split.violins} violin parts for VIOLET, $otherParts other parts for MIDI-DDSP\n",
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.APPEND
    )
    val commands =
      Option
        .when(lines.nonEmpty)(
          runner.command(
            "ViolinSampling",
            Seq(
              downloads.folder(Download.Violet),
              linesFolder,
              sounds,
              windows
            ).map(_.toString),
            rocm = Some(downloads.folder(Download.Rocm))
          )
        )
        .toSeq ++
        Option
          .when(otherParts > 0)(
            MidiDdspRendering.command(
              runner,
              downloads.folder(Download.MidiDdsp),
              othersParts,
              othersSounds
            )
          )
          .toSeq
    require(commands.nonEmpty, "the score has no note to render")
    run(commands) && {
      val partial = sound.resolveSibling(s"${sound.getFileName}.part")
      val others = Option.when(otherParts > 0)(
        MidiDdspRendering.mixed(othersSounds, otherParts)
      )
      if (lines.isEmpty) {
        Files.write(partial, Wav.pcm16(others.toVector, VioletMix.rate))
      } else {
        val stems = lines.map((name, line) =>
          VioletMix.Stem(name, audioOf(sounds.resolve(s"$name.wav")), line)
        )
        Files.write(
          partial,
          Wav.pcm16(
            Vector(VioletMix.mixed(late, stems, others, otherParts)),
            VioletMix.rate
          )
        )
      }
      Files.move(
        partial,
        sound,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      true
    }
  }

  /** A line's notes as VIOLET reads them, the runner's `ViolinNote`s: one a
    * line, `pitch velocity start end`, in seconds, as the line's MIDI file
    * holds them.
    */
  def notesOf(line: MidiFile): String =
    line.instruments
      .flatMap(_.notes)
      .map(note =>
        s"${note.pitch} ${note.velocity} ${note.start} ${note.end}\n"
      )
      .mkString

  private def audioOf(path: Path): Array[Float] = {
    val wav = Wav.read(Files.readAllBytes(path))
    require(wav.rate == VioletMix.rate, s"$path is at ${wav.rate} Hz")
    wav.mono
  }
}
