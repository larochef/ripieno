package ripieno.runner

import ripieno.runner.formats.FloatWav
import ripieno.runner.models.*
import ripieno.runner.ops.CpuOps
import ripieno.runner.weights.WeightSource

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.SplittableRandom
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** MIDI-DDSP (Wu et al., ICLR 2022) renders parts of other instruments than the
  * violin, on the processor: `MidiDdspRendering <weights folder> <parts folder>
  * <sounds folder>`, the weights folder Magenta's
  * (`midi_ddsp_model_weights_urmp_9_10`, TensorFlow checkpoints as published).
  *
  * Each `<name>.notes` of the parts folder (`program N` on its first line, then
  * a note a line: `pitch velocity start end`, in seconds, in the order
  * pretty_midi reads them) becomes `<name>.wav`, 16 kHz, one channel, floats:
  * its notes' expression (`MidiDdspExpression`), the synthesiser's controls
  * (`MidiDdspSynthesis`), the sound (`MidiDdspSynth`). Its random draws (the
  * pitch played, the noise) come from its content: the same part sounds the
  * same.
  */
object MidiDdspRendering {

  /** The instruments MIDI-DDSP knows, by General MIDI program. */
  val Instruments: Map[Int, Int] = Map(
    40 -> 0,
    41 -> 1,
    42 -> 2,
    43 -> 3,
    73 -> 4,
    68 -> 5,
    71 -> 6,
    66 -> 7,
    70 -> 8,
    56 -> 9,
    60 -> 10,
    57 -> 11,
    58 -> 12
  )

  def main(arguments: Array[String]): Unit = {
    val Array(weightsFolder, partsFolder, soundsFolder) =
      arguments.map(Path.of(_))
    Files.createDirectories(soundsFolder)
    val ops = new CpuOps
    val expressionWeights = WeightSource.open(
      ops,
      weightsFolder.resolve("expression_generator/5000.index")
    )
    val synthesisWeights = WeightSource.open(
      ops,
      weightsFolder.resolve("synthesis_generator/50000.index")
    )
    try {
      val expression = new MidiDdspExpression(expressionWeights)
      val synthesis = new MidiDdspSynthesis(synthesisWeights)
      val synth = new MidiDdspSynth(synthesisWeights)
      val parts = Using
        .resource(Files.list(partsFolder))(_.iterator.asScala.toList)
        .filter(_.getFileName.toString.endsWith(".notes"))
        .sortBy(_.getFileName.toString)
      for ((file, index) <- parts.zipWithIndex) {
        val started = System.nanoTime()
        val text = Files.readString(file)
        val (program, notes) = read(text)
        val instrument = Instruments.getOrElse(
          program,
          throw IllegalArgumentException(
            s"$file: MIDI-DDSP plays no program $program"
          )
        )
        val random = new SplittableRandom(seed(text))
        val sequence = MidiDdspSequence.of(notes)
        val parameters = synthesis.parameters(
          sequence,
          expression.controlsOf(sequence, instrument),
          instrument,
          (logits, _) => MidiDdspSynthesis.drawn(logits, random.nextDouble())
        )
        val noise = Array.fill(
          parameters.f0Hz.length * MidiDdspSynth.FrameSize
        )((random.nextDouble() * 2 - 1).toFloat)
        val sound =
          synth.sound(parameters.f0Hz, parameters.controls, instrument, noise)
        val name = file.getFileName.toString.stripSuffix(".notes")
        Files.write(
          soundsFolder.resolve(s"$name.wav"),
          FloatWav.bytes(sound, MidiDdspSynth.SampleRate)
        )
        System.err.println(
          f"$name: ${notes.size} notes, ${sound.length.toDouble / MidiDdspSynth.SampleRate}%.1f s in ${(System.nanoTime() - started) / 1e9}%.1f s"
        )
        System.err.println(s"Rendering: ${100 * (index + 1) / parts.size}%|")
      }
    } finally {
      Seq(expressionWeights, synthesisWeights).foreach(_.close())
      ops.close()
    }
  }

  // a part's program and notes (pitch, start, end)
  private def read(text: String): (Int, Vector[(Int, Double, Double)]) = {
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    val program = lines.head.stripPrefix("program").trim.toInt
    (
      program,
      lines.tail.map { line =>
        val Array(pitch, _, start, end) = line.split(" ")
        (pitch.toInt, start.toDouble, end.toDouble)
      }
    )
  }

  private def seed(text: String): Long =
    ByteBuffer
      .wrap(MessageDigest.getInstance("SHA-256").digest(text.getBytes("UTF-8")))
      .getLong
}
