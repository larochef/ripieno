package ripieno.backend.sound

import ripieno.backend.models.*
import ripieno.shared.RenderedSound

import java.io.RandomAccessFile
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.security.MessageDigest
import java.util.HexFormat
import java.util.concurrent.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import com.typesafe.scalalogging.Logger
import ox.*

/** Renders MIDI files to sound with the engines installed (`RenderingEngine`),
  * and keeps what was rendered: an engine only ever renders the same MIDI once.
  * The models an engine plays with are downloaded before its first rendering,
  * as part of it (`Downloads`).
  *
  * @param runner
  *   where the models play, when the build gave one
  * @param windows
  *   where VIOLET keeps the windows it sampled
  * @param rendered
  *   where the renderings are kept, each named after the digest of its engine
  *   and MIDI
  */
class SoundRenderer(
    runner: Option[Runner],
    downloads: Downloads,
    windows: Path,
    rendered: Path
)(using Ox) {
  import SoundRenderer.*

  private val logger = Logger(getClass)

  // An engine takes every core it finds: a second one would only slow both.
  private val oneAtATime = Semaphore(1)
  private val longestRendering: FiniteDuration = 1.hour

  // A rendering nobody has asked about for this long is given up: its page
  // moved on to another score, or was closed.
  private val forgottenAfter: FiniteDuration = 15.seconds
  private val lastAsked = ConcurrentHashMap[String, Long]()

  private val rendering = ConcurrentHashMap.newKeySet[String]()
  private val failures = ConcurrentHashMap[String, String]()

  Files.createDirectories(rendered)

  private def installed: List[RenderingEngine] =
    RenderingEngine.installed(runner, downloads, windows)

  /** The engines installed, by name. */
  def engineNames: List[String] = installed.map(_.name)

  private val engineOf = ConcurrentHashMap[String, RenderingEngine]()

  def render(
      engine: String,
      midi: Array[Byte]
  ): Either[String, RenderedSound] = {
    installed
      .find(_.name == engine)
      .map(render(_, midi))
      .toRight(s"The server has no sound engine $engine")
  }

  private def render(
      engine: RenderingEngine,
      midi: Array[Byte]
  ): RenderedSound = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(
      (engine.name + engine.version).getBytes(StandardCharsets.UTF_8)
    )
    val id = HexFormat
      .of()
      .formatHex(digest.digest(midi))
      .take(32)
    if (!Files.exists(soundPath(id)) && rendering.add(id)) {
      failures.remove(id)
      engineOf.put(id, engine)
      Files.write(rendered.resolve(s"$id.mid"), midi)
      fork {
        try {
          run(id)
        } catch {
          case NonFatal(error) =>
            logger.error(s"Rendering $id failed", error)
            failures.put(id, error.getMessage)
        } finally {
          rendering.remove(id)
        }
      }.discard
    }
    lastAsked.put(id, System.nanoTime())
    described(id)
  }

  /** The file of a rendering that is ready. */
  def soundFile(id: String): Option[Path] = {
    Option.when(isId(id))(soundPath(id)).filter(Files.exists(_))
  }

  /** Where a rendering is, when it was ever asked for. Asking is also what
    * keeps a rendering going.
    */
  def status(id: String): Option[RenderedSound] = {
    Option.when(isId(id))(described(id)).filter { sound =>
      lastAsked.computeIfPresent(id, (_, _) => System.nanoTime())
      sound.ready || sound.failure.isDefined || rendering.contains(id)
    }
  }

  private def described(id: String): RenderedSound = {
    val ready = Files.exists(soundPath(id))
    RenderedSound(
      id,
      ready,
      failure = Option(failures.get(id)),
      progress =
        Option.when(!ready && rendering.contains(id))(progress(id)).flatten
    )
  }

  private def forgotten(id: String): Boolean = {
    val asked = Option(lastAsked.get(id)).getOrElse(0L)
    System.nanoTime() - asked > forgottenAfter.toNanos
  }

  private def logFile(id: String): Path = rendered.resolve(s"$id.log")

  // The engine reports how far it is as a percentage, in the last lines it wrote.
  private def progress(id: String): Option[Int] = {
    val log = logFile(id)
    Option
      .when(Files.exists(log)) {
        Using.resource(new RandomAccessFile(log.toFile, "r")) { file =>
          val tail =
            new Array[Byte](math.min(file.length, lastLinesBytes).toInt)
          file.seek(file.length - tail.length)
          file.readFully(tail)
          percentage
            .findAllMatchIn(new String(tail, StandardCharsets.ISO_8859_1))
            .map(_.group(1).toInt)
            .toSeq
            .lastOption
        }
      }
      .flatten
  }

  // An id is a file name: nothing else than a digest is looked for on disk.
  private def isId(id: String): Boolean = id.matches("[0-9a-f]{32}")

  private def soundPath(id: String): Path = rendered.resolve(s"$id.wav")

  private def run(id: String): Unit = {
    Files.deleteIfExists(logFile(id))
    if (fetched(id)) played(id)
  }

  // The engine's models downloaded, outside the one rendering at a time: a
  // download does not hold up the other engines. Whether they are all there.
  private def fetched(id: String): Boolean =
    engineOf.get(id).needs.forall { model =>
      var shown = -1
      downloads.fetched(
        model,
        () => forgotten(id),
        percent =>
          if (percent != shown) {
            shown = percent
            Files.writeString(
              logFile(id),
              s"Downloading ${model.title}: $percent%|\n",
              StandardCharsets.UTF_8,
              StandardOpenOption.CREATE,
              StandardOpenOption.APPEND
            )
          }
      ) match {
        case _: DownloadOutcome.Completed => true
        case DownloadOutcome.Cancelled    =>
          logger.info(s"Rendering $id is no longer wanted")
          false
        case DownloadOutcome.Failed(reason) =>
          failures.put(id, s"${model.title} could not be downloaded: $reason")
          false
      }
    }

  private def played(id: String): Unit = {
    oneAtATime.acquire()
    try {
      if (forgotten(id)) {
        logger.info(s"Rendering $id is no longer wanted")
      } else {
        logger.info(s"Rendering $id")
        val work = rendered.resolve(s"$id.work")
        forget(work)
        val done =
          try {
            engineOf
              .get(id)
              .render(
                rendered.resolve(s"$id.mid"),
                soundPath(id),
                work,
                logFile(id),
                ran(id, _)
              )
          } finally {
            forget(work)
          }
        if (done) logger.info(s"Rendered $id")
      }
    } finally {
      oneAtATime.release()
    }
  }

  // Runs the commands at once, below everything else in priority (the machine
  // stays usable meanwhile), their output in the rendering's log; whether they
  // all ended well. They are stopped when nobody waits for the rendering any
  // more, or when it takes too long.
  private def ran(id: String, commands: Seq[Seq[String]]): Boolean = {
    val processes = commands.map(command =>
      new ProcessBuilder((Seq("nice", "-n", "15") ++ command)*)
        .redirectErrorStream(true)
        .redirectOutput(Redirect.appendTo(logFile(id).toFile))
        .start()
    )
    val started = System.nanoTime()
    def tooLong = System.nanoTime() - started > longestRendering.toNanos
    while (
      processes.exists(!_.waitFor(1, TimeUnit.SECONDS)) && !forgotten(id) &&
      !tooLong
    ) {}
    if (processes.exists(_.isAlive)) {
      processes.foreach { process =>
        process.descendants().forEach(_.destroy().discard)
        process.destroy()
      }
      if (tooLong) {
        failures.put(id, s"the rendering took more than $longestRendering")
      } else {
        logger.info(s"Rendering $id given up: nobody waits for it")
      }
      false
    } else {
      processes.find(_.exitValue() != 0).fold(true) { failed =>
        val lastWords = Files
          .readAllLines(logFile(id), StandardCharsets.ISO_8859_1)
          .asScala
          .filter(_.trim.nonEmpty)
          .lastOption
        failures.put(
          id,
          lastWords.getOrElse(s"the engine ended with ${failed.exitValue()}")
        )
        false
      }
    }
  }
}

object SoundRenderer {

  // a rendering's work folder removed
  private def forget(work: Path): Unit =
    if (Files.exists(work)) {
      Using
        .resource(Files.walk(work))(_.iterator.asScala.toList)
        .reverse
        .foreach(Files.delete)
    }

  private val lastLinesBytes = 4096
  private val percentage = """(\d+)%\|""".r
}
