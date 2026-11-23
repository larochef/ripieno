package ripieno.backend.sound

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Try

/** The runner (`runner/`), where the models play: a process of its own, on this
  * server's Java, from the classpath the build hands the server
  * (`RIPIENO_RUNNER_CLASSPATH`). Its GPU work runs on the ROCm tree the server
  * downloads (`Download.Rocm`), which compiles its kernels the first time, kept
  * in `kernels`.
  */
final case class Runner(java: Path, classpath: String, kernels: Path) {

  /** The command running `main` of the runner with `arguments`, on the GPU with
    * the ROCm tree `rocm` when there is one.
    */
  def command(
      main: String,
      arguments: Seq[String],
      rocm: Option[Path]
  ): Seq[String] =
    rocm
      .map(root =>
        Seq(
          "env",
          s"RUNNER_ROCM_ROOT=$root",
          s"RUNNER_KERNEL_CACHE=$kernels"
        )
      )
      .getOrElse(Seq.empty) ++
      Seq(
        java.toString,
        "--enable-native-access=ALL-UNNAMED",
        "-cp",
        classpath,
        s"ripieno.runner.$main"
      ) ++ arguments
}

object Runner {

  /** The GPU the runner's kernels are compiled for. */
  val GpuTarget = "gfx1151"

  def fromEnvironment(
      environment: Map[String, String],
      kernels: Path
  ): Option[Runner] =
    for {
      classpath <- environment
        .get("RIPIENO_RUNNER_CLASSPATH")
        .filter(_.nonEmpty)
      java <- ProcessHandle.current().info().command().toScala
    } yield Runner(Path.of(java), classpath, kernels)

  /** Whether this machine's GPU is the one the kernels are for, as the ROCm
    * kernel driver lists its compute nodes (drift's `GpuDetection`).
    */
  def hasGpu(
      nodes: Path = Path.of("/sys/class/kfd/kfd/topology/nodes")
  ): Boolean =
    Try {
      val listing = Files.list(nodes)
      try
        listing.iterator.asScala.toList
          .flatMap(node => targetVersion(node.resolve("properties")))
          .exists(version => gfxName(version) == GpuTarget)
      finally listing.close()
    }.getOrElse(false)

  // the CPU's node reports version 0
  private def targetVersion(properties: Path): Option[Int] =
    Try(Files.readAllLines(properties, StandardCharsets.UTF_8).asScala)
      .getOrElse(Nil)
      .collectFirst {
        case line if line.startsWith("gfx_target_version ") =>
          line.stripPrefix("gfx_target_version ").trim.toIntOption
      }
      .flatten
      .filter(_ > 0)

  /** 110501 → gfx1151: the major in decimal, minor and stepping a hex digit
    * each.
    */
  def gfxName(version: Int): String =
    s"gfx${version / 10000}${Integer.toHexString(version / 100 % 100)}${Integer
        .toHexString(version % 100)}"
}
