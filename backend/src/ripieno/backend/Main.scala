package ripieno.backend

import ripieno.backend.editions.{EditionApi, EditionStore}
import ripieno.backend.library.*
import ripieno.backend.microphone.{Microphone, MicrophoneApi}
import ripieno.backend.models.*
import ripieno.backend.review.{PerformanceReviewer, ReviewApi}
import ripieno.backend.scores.{ScoreApi, ScoreLibrary}
import ripieno.backend.sound.*
import ripieno.backend.storage.*
import ripieno.backend.tempi.{TempoApi, TempoStore}

import java.net.http.HttpClient
import java.nio.file.Path
import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

import com.typesafe.scalalogging.Logger
import ox.*
import sttp.tapir.server.netty.NettySocketConfig
import sttp.tapir.server.netty.sync.NettySyncServer

object Main extends OxApp {
  private val logger = Logger("ripieno.backend.main")

  private def setting(name: String, default: String): String =
    sys.env.getOrElse(name, default)

  override def run(args: Vector[String])(using Ox): ExitCode = {
    val places = Places.fromEnvironment(sys.env).created()
    val renderedSounds = Path.of(
      setting(
        "RIPIENO_RENDERED_SOUNDS",
        places.cache.resolve("rendered").toString
      )
    )
    val reviews =
      Path.of(
        setting("RIPIENO_REVIEWS", places.data.resolve("reviews").toString)
      )
    val editions = Path.of(
      setting("RIPIENO_EDITIONS", places.data.resolve("editions").toString)
    )
    val port = setting("RIPIENO_PORT", "4322").toInt
    logger.info(s"Data in ${places.data}, cache in ${places.cache}")
    val http = HttpClient
      .newBuilder()
      .followRedirects(HttpClient.Redirect.NORMAL)
      .build()
    val downloader = Downloader(http, StallWatches())
    val library = ScoreLibrary(places.catalog)
    val database = Database.open(places.database)
    val blobs = BlobStore(places, places.catalog)
    val works = LibraryStore(database, blobs)
    val catalog = CatalogIndex(places.catalog, database, blobs, works)
    catalog.refresh()
    // the catalog's scores downloaded the first time, meanwhile the server serves
    forkUnsupervised {
      val download = CatalogDownload(http, places.catalog)
      try
        if (!download.installed) {
          download.fetched()
          catalog.refresh()
        }
      catch {
        case NonFatal(error) =>
          logger.warn("The catalog's scores could not be downloaded", error)
      }
    }.discard
    val isScore = (path: String) => works.exists(path) || library.isScore(path)
    val editionStore = EditionStore(editions, isScore)
    val tempoStore = TempoStore(editions, isScore)
    val reviewer = PerformanceReviewer(reviews)
    val home = Path.of(sys.props("user.home"))
    val downloads = Downloads(
      HuggingFaceCache(
        HuggingFaceCache.rootOf(sys.env, home),
        downloader
      ),
      places.cache,
      downloader
    )
    val server = NettySyncServer()
      .modifyConfig(
        _.requestTimeout(5.minutes)
          .idleTimeout(5.minutes)
          // a restart does not have to wait for the previous run's port
          .socketConfig(NettySocketConfig.default.withReuseAddress)
      )
      .host("0.0.0.0")
      .port(port)
      .addEndpoints(ScoreApi(library, works).endpoints)
      .addEndpoints(
        LibraryApi(
          works,
          WorkRemoval(works, editionStore, tempoStore, reviewer),
          PdfExport(places.cache.resolve("export"))
        ).endpoints
      )
      .addEndpoints(EditionApi(editionStore).endpoints)
      .addEndpoints(TempoApi(tempoStore).endpoints)
      .addEndpoints(SoundBankApi(downloads).endpoints)
      .addEndpoints(
        SoundRenderingApi(
          SoundRenderer(
            Runner.fromEnvironment(sys.env, places.cache.resolve("kernels")),
            downloads,
            places.cache.resolve("violet-windows"),
            renderedSounds
          )
        ).endpoints
      )
      .addEndpoints(ReviewApi(reviewer).endpoints)
      .addEndpoints(MicrophoneApi(Microphone()).endpoints)
      .addEndpoints(FrontendApi.endpoints)

    logger.info(s"Serving on http://localhost:$port")
    fork(server.startAndWait()).join()

    ExitCode.Success
  }
}
