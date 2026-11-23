package ripieno.runner

import ripieno.runner.formats.FloatWav
import ripieno.runner.models.*
import ripieno.runner.native.HipRuntime
import ripieno.runner.ops.{HipOps, MatVecInputs}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.*
import java.nio.file.attribute.FileTime
import java.security.MessageDigest
import java.util.HexFormat
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** VIOLET renders violin lines on the GPU, the whole of its inference here:
  * `ViolinSampling <weights folder> <lines folder> <sounds folder> <window
  * cache>`, the weights folder VIOLET's published repository, its weights as
  * its authors publish them (`VioletWeights`).
  *
  * Each `<name>.notes` of the lines folder (`ViolinNote`) becomes `<name>.wav`,
  * 48 kHz, one channel, floats: cut into windows (`ViolinWindows`), each
  * conditioned (`ViolinConditioning`), sampled from a noise seeded by its
  * content (`ViolinNoise`, `ViolinSchedule`, `HeunSampler`), decoded
  * (`ViolinDecoder`), then assembled (`ViolinLine`). A window's audio is kept
  * in the cache under all it is made from: the same window asked again, in a
  * score changed elsewhere, is read back. How far the rendering is goes to the
  * standard error as `Sampling: N%|`. `VIOLET_STEPS` sets the sampler's steps
  * (30, the reference's).
  */
object ViolinSampling {

  // the cache's size, the windows least recently used going first beyond it
  private val cacheBytes = 3L << 30

  def main(arguments: Array[String]): Unit = {
    val Array(weightsFolder, linesFolder, soundsFolder, cacheFolder) =
      arguments.map(Path.of(_))
    val network = weightsFolder.resolve(VioletWeights.NetworkFile)
    val decoderFile = weightsFolder.resolve(VioletWeights.DecoderFile)
    val steps =
      sys.env.get("VIOLET_STEPS").map(_.toInt).getOrElse(ViolinSchedule.Steps)
    val sigmas = ViolinSchedule.sigmas(steps).toIndexedSeq
    Files.createDirectories(soundsFolder)
    Files.createDirectories(cacheFolder)
    pruned(cacheFolder)
    // what the windows are made with besides their content, for their keys
    val makers = s"violet v3 $steps " + Seq(network, decoderFile)
      .map(path =>
        s"${path.getFileName}:${Files.size(path)}:${Files.getLastModifiedTime(path).toMillis}"
      )
      .mkString(" ")

    val ops = new HipOps(HipRuntime.fromEnvironment(), MatVecInputs.Int8)
    val networkWeights = VioletWeights.network(ops, network)
    val decoderWeights = VioletWeights.decoder(ops, decoderFile)
    val renderer =
      new ViolinRenderer(ops, networkWeights, decoderWeights, sigmas)
    var sampled = 0
    var cached = 0
    var seconds = 0.0
    // how far the rendering is: the windows done of all the lines'
    var windows = 1
    var done = 0
    def windowDone(): Unit = {
      done += 1
      System.err.println(s"Sampling: ${math.min(99, 100 * done / windows)}%|")
    }

    val line = new ViolinLine(
      (window, attempt) => {
        val content =
          digest(window.notes, window.techniques, window.dynamics)
        val seed = Integer.parseInt(content.take(7), 16).toLong + attempt
        val file = cacheFolder.resolve(
          s"${digest(makers, seed.toString, content).take(40)}.f32"
        )
        if (Files.exists(file)) {
          cached += 1
          Files.setLastModifiedTime(
            file,
            FileTime.fromMillis(System.currentTimeMillis())
          )
          read(file)
        } else {
          val started = System.nanoTime()
          val audio = renderer.audio(window, seed)
          seconds += (System.nanoTime() - started) / 1e9
          sampled += 1
          val partial = file.resolveSibling(s"${file.getFileName}.part")
          write(partial, audio)
          Files.move(
            partial,
            file,
            StandardCopyOption.ATOMIC_MOVE,
            StandardCopyOption.REPLACE_EXISTING
          )
          audio
        }
      },
      () => windowDone()
    )

    try {
      val lines = Using
        .resource(Files.list(linesFolder))(_.iterator.asScala.toList)
        .filter(_.getFileName.toString.endsWith(".notes"))
        .sortBy(_.getFileName.toString)
      val notes = lines.map(ViolinNote.read)
      windows = math.max(1, notes.map(ViolinWindows.starts(_).size).sum)
      for ((file, lineNotes) <- lines.zip(notes)) {
        val audio = line.rendered(lineNotes)
        val name = file.getFileName.toString.stripSuffix(".notes")
        Files.write(
          soundsFolder.resolve(s"$name.wav"),
          FloatWav.bytes(audio, ViolinLine.SampleRate)
        )
      }
      System.err.println(
        f"runner: $sampled windows sampled in $seconds%.1f s, $cached read from the cache"
      )
    } finally {
      renderer.close()
      Seq(networkWeights, decoderWeights).foreach(_.close())
      ops.close()
    }
  }

  private def digest(parts: Any*): String = {
    val md = MessageDigest.getInstance("SHA-256")
    parts.foreach {
      case values: Array[Float] =>
        val bytes =
          ByteBuffer.allocate(4 * values.length).order(ByteOrder.LITTLE_ENDIAN)
        bytes.asFloatBuffer().put(values)
        md.update(bytes.array())
      case other => md.update(other.toString.getBytes("UTF-8"))
    }
    HexFormat.of().formatHex(md.digest())
  }

  // the cache kept under its size, the windows least recently used going first
  private def pruned(cache: Path): Unit = {
    val files = Using
      .resource(Files.list(cache))(_.iterator.asScala.toList)
      .filter(_.getFileName.toString.endsWith(".f32"))
    val recent = files.sortBy(file => -Files.getLastModifiedTime(file).toMillis)
    recent
      .scanLeft(0L)((total, file) => total + Files.size(file))
      .drop(1)
      .zip(recent)
      .collect { case (total, file) if total > cacheBytes => file }
      .foreach(Files.deleteIfExists)
  }

  private def read(file: Path): Array[Float] = {
    val bytes = ByteBuffer
      .wrap(Files.readAllBytes(file))
      .order(ByteOrder.LITTLE_ENDIAN)
      .asFloatBuffer()
    val values = new Array[Float](bytes.remaining())
    bytes.get(values)
    values
  }

  private def write(file: Path, values: Array[Float]): Unit = {
    val bytes =
      ByteBuffer.allocate(4 * values.length).order(ByteOrder.LITTLE_ENDIAN)
    bytes.asFloatBuffer().put(values)
    Files.write(file, bytes.array())
  }
}
