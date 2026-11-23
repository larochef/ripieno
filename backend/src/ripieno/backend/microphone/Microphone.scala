package ripieno.backend.microphone

import ripieno.shared.MicrophoneLevel

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.util.Try

/** The input volume of this machine's default microphone, through PipeWire's
  * `wpctl`: the browser cannot set it, and a sound that saturates before it
  * reaches the page cannot be brought back.
  */
final class Microphone {
  private val source = "@DEFAULT_AUDIO_SOURCE@"
  private val volumeLine = """Volume:\s*([0-9.]+)""".r.unanchored

  /** Only a page on this machine plays into its microphone. */
  private def isLocal(remote: Option[InetSocketAddress]): Boolean =
    remote.exists(address =>
      Option(address.getAddress).exists(_.isLoopbackAddress)
    )

  def level(
      remote: Option[InetSocketAddress]
  ): Either[String, MicrophoneLevel] =
    run("get-volume", source).flatMap {
      case volumeLine(volume) =>
        Right(MicrophoneLevel(volume.toDouble, isLocal(remote)))
      case other => Left(s"The microphone's volume could not be read: $other")
    }

  def setLevel(
      volume: Double,
      remote: Option[InetSocketAddress]
  ): Either[String, MicrophoneLevel] =
    if (!isLocal(remote)) {
      Left("Only a page on the server's own machine may set its microphone")
    } else {
      val bounded = math.max(Microphone.lowest, math.min(1.0, volume))
      run("set-volume", source, f"$bounded%.3f").flatMap(_ => level(remote))
    }

  private def run(arguments: String*): Either[String, String] =
    Try {
      val process = new ProcessBuilder(("wpctl" +: arguments)*)
        .redirectErrorStream(true)
        .start()
      val finished = process.waitFor(5, TimeUnit.SECONDS)
      val output = new String(
        process.getInputStream.readAllBytes(),
        StandardCharsets.UTF_8
      ).trim
      if (finished && process.exitValue() == 0) Right(output)
      else Left(s"wpctl failed: $output")
    }.toEither.left
      .map(error =>
        s"No microphone volume on this machine: ${error.getMessage}"
      )
      .flatten
}

object Microphone {
  // not quite silent: a volume of 0 would leave nothing to measure the next time
  val lowest = 0.05
}
