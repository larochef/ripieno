package ripieno.backend.review

import ripieno.backend.ByteRanges
import ripieno.shared.routes.ReviewRoutes

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

import sttp.model.{HeaderNames, StatusCode}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint

final class ReviewApi(reviewer: PerformanceReviewer) {

  // `ReviewRoutes.recordingAudioPath`, served by byte ranges as an audio element
  // asks for it (see `SoundRenderingApi`).
  private val recordingAudio: ServerEndpoint[Any, Identity] =
    endpoint.get
      .in("api" / "recordings" / path[String]("id"))
      .in(header[Option[String]](HeaderNames.Range))
      .errorOut(
        statusCode.and(header[Option[String]](HeaderNames.ContentRange))
      )
      .out(statusCode)
      .out(header[String](HeaderNames.ContentType))
      .out(header(HeaderNames.AcceptRanges, "bytes"))
      .out(header[Option[String]](HeaderNames.ContentRange))
      .out(header[Long](HeaderNames.ContentLength))
      .out(fileRangeBody)
      .serverLogic[Identity] { (id, range) =>
        reviewer.recording(id) match {
          case None       => Left((StatusCode.NotFound, None))
          case Some(file) =>
            val size = Files.size(file)
            ByteRanges.of(range, size) match {
              case ByteRanges.Request.Whole =>
                Right(
                  (
                    StatusCode.Ok,
                    ReviewApi.mediaType(file),
                    None,
                    size,
                    FileRange(file.toFile)
                  )
                )
              case ByteRanges.Request.Part(first, last) =>
                val part = FileRange(
                  file.toFile,
                  Some(RangeValue(Some(first), Some(last), size))
                )
                Right(
                  (
                    StatusCode.PartialContent,
                    ReviewApi.mediaType(file),
                    Some(s"bytes $first-$last/$size"),
                    last - first + 1,
                    part
                  )
                )
              case ByteRanges.Request.Unsatisfiable =>
                Left((StatusCode.RangeNotSatisfiable, Some(s"bytes */$size")))
            }
        }
      }

  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    ReviewRoutes.startReview.serverLogic[Identity](reviewer.start),
    ReviewRoutes.sendRecording.serverLogic[Identity](reviewer.record),
    ReviewRoutes.review.serverLogic[Identity](reviewer.status),
    ReviewRoutes.listReviews.serverLogic[Identity](reviewer.summaries),
    ReviewRoutes.deleteReview.serverLogic[Identity](reviewer.delete),
    recordingAudio
  )
}

object ReviewApi {

  /** What the browser recorded in, from the file's first bytes: Chrome writes
    * WebM, Firefox Ogg, Safari MP4. An audio element refuses a file announced
    * as another.
    */
  def mediaType(file: Path): String = {
    val head = Using.resource(Files.newInputStream(file))(_.readNBytes(12))
    def startsWith(bytes: Int*) =
      head.length >= bytes.length && bytes.indices.forall(i =>
        (head(i) & 0xff) == bytes(i)
      )
    if (startsWith(0x4f, 0x67, 0x67, 0x53)) "audio/ogg"
    else if (startsWith(0x1a, 0x45, 0xdf, 0xa3)) "audio/webm"
    else if (
      head.length >= 8 && new String(
        head.slice(4, 8),
        StandardCharsets.ISO_8859_1
      ) == "ftyp"
    ) "audio/mp4"
    else "application/octet-stream"
  }
}
