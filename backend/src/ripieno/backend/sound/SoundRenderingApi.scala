package ripieno.backend.sound

import ripieno.backend.ByteRanges
import ripieno.shared.routes.SoundRoutes

import java.nio.file.Files

import sttp.model.{HeaderNames, StatusCode}
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint

final class SoundRenderingApi(renderer: SoundRenderer) {

  // `SoundRoutes.renderedAudioPath`. An audio element asks for the file by
  // byte ranges, open-ended ones included (`bytes=0-`), which tapir's own
  // static files answer with a `Content-Range` no browser accepts.
  private val renderedAudio: ServerEndpoint[Any, Identity] =
    endpoint.get
      .in("api" / "rendered" / path[String]("file"))
      .in(header[Option[String]](HeaderNames.Range))
      .errorOut(
        statusCode.and(header[Option[String]](HeaderNames.ContentRange))
      )
      .out(statusCode)
      .out(header(HeaderNames.ContentType, "audio/wav"))
      .out(header(HeaderNames.AcceptRanges, "bytes"))
      .out(header[Option[String]](HeaderNames.ContentRange))
      .out(header[Long](HeaderNames.ContentLength))
      .out(fileRangeBody)
      .serverLogic[Identity] { (fileName, range) =>
        renderer.soundFile(fileName.stripSuffix(".wav")) match {
          case None       => Left((StatusCode.NotFound, None))
          case Some(file) =>
            val size = Files.size(file)
            ByteRanges.of(range, size) match {
              case ByteRanges.Request.Whole =>
                Right((StatusCode.Ok, None, size, FileRange(file.toFile)))
              case ByteRanges.Request.Part(first, last) =>
                val part = FileRange(
                  file.toFile,
                  Some(RangeValue(Some(first), Some(last), size))
                )
                Right(
                  (
                    StatusCode.PartialContent,
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
    SoundRoutes.renderSound.serverLogic[Identity](renderer.render),
    SoundRoutes.renderedSound.serverLogic[Identity](id =>
      renderer.status(id).toRight(s"No rendering $id")
    ),
    renderedAudio
  )
}
