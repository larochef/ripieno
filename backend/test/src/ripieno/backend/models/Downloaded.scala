package ripieno.backend.models

import ripieno.backend.storage.Places

import java.net.http.HttpClient
import java.nio.file.Path
import scala.collection.mutable

import ox.*

/** What the application downloads, for the tests that need it: where the server
  * keeps it (`Downloads`, the caches of the environment), downloaded first when
  * it is not there yet, the test then waiting for it. The application needs all
  * of it to work, so a test is never skipped for want of it.
  */
object Downloaded {

  private val folders = mutable.Map.empty[Download, Path]

  /** A download's folder, once there. */
  def folder(download: Download): Path = synchronized {
    folders.getOrElseUpdate(download, fetched(download)._1)
  }

  /** The page's sound bank. */
  lazy val soundBank: Path = fetched(Download.SoundBank)._2.soundBank

  private def fetched(download: Download): (Path, Downloads) = supervised {
    val downloader = Downloader(
      HttpClient
        .newBuilder()
        .followRedirects(HttpClient.Redirect.NORMAL)
        .build(),
      StallWatches()
    )
    val downloads = Downloads(
      HuggingFaceCache(
        HuggingFaceCache.rootOf(sys.env, Path.of(sys.props("user.home"))),
        downloader
      ),
      Places.fromEnvironment(sys.env).cache,
      downloader
    )
    var shown = -1
    downloads.fetched(
      download,
      () => false,
      percent =>
        if (percent / 10 != shown / 10) {
          shown = percent
          println(s"  downloading ${download.title}: $percent %")
        }
    ) match {
      case DownloadOutcome.Completed(folder, _) => (folder, downloads)
      case other                                =>
        throw IllegalStateException(s"${download.title}: $other")
    }
  }
}
