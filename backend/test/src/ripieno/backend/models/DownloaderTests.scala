package ripieno.backend.models

import java.io.ByteArrayOutputStream
import java.net.{InetSocketAddress, URI}
import java.net.http.HttpClient
import java.nio.file.{Files, LinkOption, Path}
import java.security.MessageDigest
import java.util.HexFormat
import java.util.concurrent.{CountDownLatch, Executors}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.{Random, Using}

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import ox.*
import utest.*

/** Downloads from a local server: a silent connection fails and a cancel lands
  * while it is silent (`StallWatch`, from drift); a large file comes in
  * parallel ranges, checked by its SHA-256, and resumes where a cancel left it;
  * a Hugging Face file lands in the hub's layout and is downloaded once; a zip
  * is extracted only inside its folder.
  */
object DownloaderTests extends TestSuite {

  private def serving(
      handler: HttpExchange => Unit
  ): HttpServer = {
    val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/", exchange => handler(exchange))
    server.setExecutor(Executors.newCachedThreadPool())
    server.start()
    server
  }

  private def address(server: HttpServer): String =
    s"http://127.0.0.1:${server.getAddress.getPort}"

  /** A few bytes of a 1 MB body, then nothing until released. */
  private def silent(release: CountDownLatch): HttpServer =
    serving { exchange =>
      exchange.sendResponseHeaders(200, 1024 * 1024)
      val out = exchange.getResponseBody
      out.write(Array.fill[Byte](1024)(1))
      out.flush()
      release.await()
      exchange.close()
    }

  /** `content` at any path, a range of it when asked; `served` counts the bytes
    * sent.
    */
  private def ranged(content: Array[Byte], served: AtomicLong): HttpServer =
    serving { exchange =>
      val range = Option(exchange.getRequestHeaders.getFirst("Range"))
        .map(_.stripPrefix("bytes=").split("-", -1))
      val (start, end) = range.fold((0L, content.length - 1L))(parts =>
        (
          parts(0).toLong,
          parts.lift(1).filter(_.nonEmpty).fold(content.length - 1L)(_.toLong)
        )
      )
      val length = end - start + 1
      if (range.isDefined)
        exchange.getResponseHeaders
          .add("Content-Range", s"bytes $start-$end/${content.length}")
      exchange.sendResponseHeaders(if (range.isDefined) 206 else 200, length)
      try {
        exchange.getResponseBody.write(content, start.toInt, length.toInt)
        served.addAndGet(length)
      } catch { case _: java.io.IOException => () }
      exchange.close()
    }

  private def forget(folder: Path): Unit =
    Using
      .resource(Files.walk(folder))(_.iterator.asScala.toList)
      .reverse
      .foreach(Files.delete)

  private def sha256(bytes: Array[Byte]): String =
    HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))

  private def downloader(stallMillis: Long = 2000)(using Ox): Downloader =
    Downloader(HttpClient.newHttpClient(), StallWatches(), stallMillis)

  val tests = Tests {
    test("a silent connection fails instead of hanging") {
      val release = CountDownLatch(1)
      val server = silent(release)
      try
        supervised {
          val folder = Files.createTempDirectory("stall")
          val started = System.currentTimeMillis()
          val outcome = downloader().fetch(
            url = s"${address(server)}/file",
            partFile = folder.resolve("file.part"),
            finalFile = folder.resolve("file"),
            expectedSha256 = None
          )
          val took = System.currentTimeMillis() - started
          assert(outcome.isInstanceOf[DownloadOutcome.Failed], took < 15000)
        }
      finally { release.countDown(); server.stop(0) }
    }

    test("a cancel lands while the connection is silent") {
      val release = CountDownLatch(1)
      val server = silent(release)
      val cancelled = AtomicBoolean(false)
      try
        supervised {
          forkDiscard { sleep(500.millis); cancelled.set(true) }
          val folder = Files.createTempDirectory("stall")
          val started = System.currentTimeMillis()
          val outcome = downloader().fetch(
            url = s"${address(server)}/file",
            partFile = folder.resolve("file.part"),
            finalFile = folder.resolve("file"),
            expectedSha256 = None,
            isCancelled = () => cancelled.get()
          )
          val took = System.currentTimeMillis() - started
          assert(outcome == DownloadOutcome.Cancelled, took < 5000)
        }
      finally { release.countDown(); server.stop(0) }
    }

    test("a large file comes in ranges, checked, and resumes after a cancel") {
      // six chunks of 4 MB
      val chunk = 4L << 20
      val content = new Array[Byte](24 << 20)
      Random(7).nextBytes(content)
      val served = AtomicLong()
      val server = ranged(content, served)
      val folder = Files.createTempDirectory("ranged")
      try
        supervised {
          val (part, file) =
            (folder.resolve("file.part"), folder.resolve("file"))
          val cancelled = AtomicBoolean(false)
          def fetch(onProgress: (Long, Option[Long]) => Unit) =
            Downloader(
              HttpClient.newHttpClient(),
              StallWatches(),
              chunkBytes = chunk,
              chunkedThreshold = chunk
            ).fetch(
              url = s"${address(server)}/file",
              partFile = part,
              finalFile = file,
              expectedSha256 = Some(sha256(content)),
              isCancelled = () => cancelled.get(),
              onProgress = onProgress
            )
          val first =
            fetch((bytes, _) => if (bytes > (20 << 20)) cancelled.set(true))
          assert(first == DownloadOutcome.Cancelled, !Files.exists(file))
          // none completed is written as no list at all
          val kept = ujson
            .read(Files.readString(folder.resolve("file.part.chunks")))
            .obj
            .get("completed")
            .fold(0)(_.arr.size)
          cancelled.set(false)
          val before = served.get()
          val outcome = fetch((_, _) => ())
          assert(
            kept > 0,
            outcome == DownloadOutcome.Completed(file, content.length.toLong),
            sha256(Files.readAllBytes(file)) == sha256(content),
            // only the chunks the cancel left came again, and the byte asking
            // whether ranges are served
            served.get() - before == content.length - kept * chunk + 1
          )
        }
      finally {
        server.stop(0)
        forget(folder)
      }
    }

    test("a Hugging Face file lands in the hub's layout, once") {
      val content = "a downloads weight".getBytes
      val served = AtomicLong()
      val server = ranged(content, served)
      try
        supervised {
          val root = Files.createTempDirectory("hub")
          val cache = HuggingFaceCache(
            root,
            downloader(),
            URI.create(address(server))
          )
          def fetch() = cache.fetched(
            "someone/model",
            "abc123",
            "folder/weights.pth",
            sha256(content),
            () => false,
            (_, _) => ()
          )
          val link = root.resolve(
            "models--someone--model/snapshots/abc123/folder/weights.pth"
          )
          assert(
            fetch() == DownloadOutcome.Completed(link, content.length.toLong)
          )
          val once = served.get()
          fetch()
          assert(
            served.get() == once,
            Files.isSymbolicLink(link),
            Files.readSymbolicLink(link) == Path.of(
              s"../../../blobs/${sha256(content)}"
            ),
            Files.readString(link) == "a downloads weight",
            Files.readString(
              root.resolve("models--someone--model/refs/main")
            ) == "abc123",
            Files.exists(
              root.resolve(
                s".locks/models--someone--model/${sha256(content)}.lock"
              ),
              LinkOption.NOFOLLOW_LINKS
            )
          )
        }
      finally server.stop(0)
    }

    test("a zip is extracted inside its folder only") {
      def zip(names: String*): Path = {
        val bytes = ByteArrayOutputStream()
        val out = ZipOutputStream(bytes)
        for (name <- names) {
          out.putNextEntry(ZipEntry(name))
          out.write(name.getBytes)
          out.closeEntry()
        }
        out.close()
        val file = Files.createTempFile("weights", ".zip")
        Files.write(file, bytes.toByteArray)
      }
      val folder = Files.createTempDirectory("extracted").resolve("model")
      Downloads.extracted(
        zip("weights/a.index", "weights/b.data"),
        folder
      )
      assert(
        Files.readString(folder.resolve("weights/a.index")) == "weights/a.index"
      )
      val other = Files.createTempDirectory("extracted").resolve("model")
      val refused = assertThrows[IllegalArgumentException](
        Downloads.extracted(zip("../escaped"), other)
      )
      assert(refused.getMessage.contains("outside"), !Files.exists(other))
    }
  }
}
