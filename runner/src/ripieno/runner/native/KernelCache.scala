package ripieno.runner.native

import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.security.MessageDigest
import java.util.HexFormat
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The kernels compiled for this machine: the runner carries their sources
  * (`runner/kernels`, its resources' `kernels/`), compiled the first time with
  * the ROCm tree it runs on, all at once, and kept under `RUNNER_KERNEL_CACHE`
  * (else Ripieno's cache, `kernels`). A folder for each set of sources, tree
  * and flags: `hipcc` does not give the same bytes twice, so the code objects
  * are named by what they were made from, not by what they are.
  */
object KernelCache {

  private val flags =
    Seq("--genco", "--offload-arch=gfx1151", "-O3", "-std=c++20")

  // at most this long for all of them: about 50 s on a Strix Halo
  private val longestMinutes = 15L

  private val folders = scala.collection.mutable.Map.empty[Path, Path]

  /** The folder of the kernels compiled with `rocmRoot`'s `hipcc`, compiled now
    * when they are not there yet. How far the compilation is goes to the
    * standard error as `Compiling the GPU's kernels: N%|`.
    */
  def folder(rocmRoot: Path): Path = synchronized {
    folders.getOrElseUpdate(rocmRoot, compiled(rocmRoot))
  }

  private def resource(name: String): Array[Byte] =
    Using.resource(
      Option(getClass.getResourceAsStream(s"/kernels/$name")).getOrElse(
        throw new IllegalStateException(s"the runner carries no kernels/$name")
      )
    )(_.readAllBytes())

  private def compiled(rocmRoot: Path): Path = {
    val sources =
      new String(resource("sources.txt"), StandardCharsets.UTF_8).linesIterator
        .map(_.trim)
        .filter(_.nonEmpty)
        .toVector
        .map(name => name -> resource(name))
    val tree = rocmRoot.toRealPath()
    val digest = MessageDigest.getInstance("SHA-256")
    for ((name, bytes) <- sources) {
      digest.update(name.getBytes(StandardCharsets.UTF_8))
      digest.update(bytes)
    }
    digest.update(tree.toString.getBytes(StandardCharsets.UTF_8))
    digest.update(flags.mkString(" ").getBytes(StandardCharsets.UTF_8))
    val key = HexFormat.of().formatHex(digest.digest()).take(16)
    val folder = cacheRoot.resolve(key)
    if (!Files.isDirectory(folder)) {
      Files.createDirectories(cacheRoot)
      val work = Files.createTempDirectory(cacheRoot, s".$key-")
      try {
        val (source, out) = (work.resolve("source"), work.resolve("kernels"))
        Files.createDirectories(source)
        Files.createDirectories(out)
        for ((name, bytes) <- sources) Files.write(source.resolve(name), bytes)
        compileAll(
          tree,
          source,
          out,
          sources.map(_._1).filter(_.endsWith(".hip"))
        )
        try Files.move(out, folder, StandardCopyOption.ATOMIC_MOVE)
        catch {
          // another runner compiled them meanwhile: theirs are as good
          case _: FileAlreadyExistsException | _: DirectoryNotEmptyException =>
            ()
        }
      } finally forget(work)
    }
    folder
  }

  private def compileAll(
      tree: Path,
      source: Path,
      out: Path,
      kernels: Vector[String]
  ): Unit = {
    val logs = out.resolveSibling("logs")
    Files.createDirectories(logs)
    val processes = kernels.map { name =>
      val base = name.stripSuffix(".hip")
      val builder = new ProcessBuilder(
        (Seq(tree.resolve("bin/hipcc").toString) ++ flags ++
          Seq(
            source.resolve(name).toString,
            "-o",
            out.resolve(s"$base.hsaco").toString
          ))*
      ).redirectErrorStream(true)
        .redirectOutput(logs.resolve(s"$base.log").toFile)
      builder.environment().put("ROCM_PATH", tree.toString)
      builder.environment().put("HIP_PATH", tree.toString)
      base -> builder.start()
    }
    val deadline = System.nanoTime() + TimeUnit.MINUTES.toNanos(longestMinutes)
    var done = 0
    System.err.println("Compiling the GPU's kernels: 0%|")
    for ((base, process) <- processes) {
      val left = math.max(0L, deadline - System.nanoTime())
      if (!process.waitFor(left, TimeUnit.NANOSECONDS)) {
        processes.foreach(_._2.destroyForcibly())
        throw new IllegalStateException(
          s"compiling the GPU's kernels took more than $longestMinutes minutes"
        )
      }
      if (process.exitValue() != 0) {
        processes.foreach(_._2.destroyForcibly())
        val log = Files.readString(logs.resolve(s"$base.log")).take(2000)
        throw new IllegalStateException(s"hipcc failed on $base.hip:\n$log")
      }
      done += 1
      System.err.println(
        s"Compiling the GPU's kernels: ${100 * done / processes.size}%|"
      )
    }
  }

  private def cacheRoot: Path = {
    def variable(name: String): Option[Path] =
      sys.env.get(name).map(_.trim).filter(_.nonEmpty).map(Path.of(_))
    variable("RUNNER_KERNEL_CACHE").getOrElse(
      variable("XDG_CACHE_HOME")
        .getOrElse(Path.of(sys.props("user.home"), ".cache"))
        .resolve("ripieno/kernels")
    )
  }

  private def forget(folder: Path): Unit =
    if (Files.exists(folder))
      Using
        .resource(Files.walk(folder))(_.iterator.asScala.toList)
        .reverse
        .foreach(Files.delete)
}
