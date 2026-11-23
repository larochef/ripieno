package ripieno.backend.sound

import ripieno.backend.models.*

import java.io.File

import com.typesafe.scalalogging.Logger
import sttp.model.StatusCode
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint

/** Serves the SoundFont every page plays with (`Download.SoundBank`),
  * downloaded first when it is not there: the first page asking waits for it, a
  * few seconds.
  */
final class SoundBankApi(downloads: Downloads) {
  private val logger = Logger(getClass)
  private val oneWeek = 604800

  private def soundBank: Either[Unit, File] =
    downloads.fetched(Download.SoundBank, () => false, _ => ()) match {
      case _: DownloadOutcome.Completed => Right(downloads.soundBank.toFile)
      case other                        =>
        logger.warn(s"The sound bank is not there: $other")
        Left(())
    }

  // `SoundRoutes.soundBankPath`
  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    endpoint.get
      .in("api" / "soundfont")
      .errorOut(statusCode(StatusCode.ServiceUnavailable))
      // tens of megabytes that do not change: fetched once per browser
      .out(header("Cache-Control", s"max-age=$oneWeek"))
      .out(fileBody)
      .serverLogic[Identity](_ => soundBank)
  )
}
