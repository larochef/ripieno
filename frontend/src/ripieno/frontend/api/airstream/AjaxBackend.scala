package ripieno.frontend.api.airstream

import ripieno.frontend.api.airstream.AjaxBackend.*

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.*

import com.raquo.airstream.web.AjaxStream
import com.raquo.airstream.web.AjaxStream.*
import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sttp.capabilities.Effect
import sttp.client4.*
import sttp.client4.SttpClientException.{ReadException, TimeoutException}
import sttp.client4.internal.{BodyFromResponseAs, SttpFile}
import sttp.client4.ws.{GotAWebSocketException, NotAWebSocketException}
import sttp.model.*
import sttp.monad.MonadError

// An sttp backend sending requests with Airstream's AjaxStream. An
// XMLHttpRequest holds the whole body when it answers, so reading it needs no
// promise, unlike fetch. Streaming bodies and web sockets aren't supported.
class AjaxBackend extends Backend[EventStream] {

  override given monad: MonadError[EventStream] = AirstreamMonad

  override def send[T](
      request: GenericRequest[T, Any & Effect[EventStream]]
  ): EventStream[Response[T]] =
    SttpClientException.adjustExceptions(monad)(sendRegular(request))(
      toSttpClientException(request, _)
    )

  override def close(): EventStream[Unit] = monad.unit(())

  private def sendRegular[T](
      request: GenericRequest[T, Any & Effect[EventStream]]
  ): EventStream[Response[T]] =
    AjaxStream(
      method = request.method.method,
      url = request.uri.toString,
      data = requestBody(request.body),
      timeoutMs = timeoutMs(request.options.readTimeout),
      headers = requestHeaders(request),
      withCredentials = false,
      responseType = "arraybuffer",
      // Error statuses are regular responses, whose bodies tapir decodes.
      isStatusCodeSuccess = _ => true
    ).flatMapSwitch { xhr =>
      val response = AjaxResponse(xhr)
      request.options.onBodyReceived(response.metadata)
      bodyFromResponseAs(request.response, response.metadata, Left(response))
        .map { body =>
          Response(
            body = body,
            code = response.code,
            statusText = response.statusText,
            headers = response.headers,
            history = Nil,
            request = request.onlyMetadata
          )
        }
    }

  private def toSttpClientException(
      request: GenericRequest[?, ?],
      error: Exception
  ): Option[Exception] =
    error match {
      case timeout: AjaxTimeout => Some(TimeoutException(request, timeout))
      // The browser doesn't tell whether the request reached the server.
      case failure: (AjaxNetworkError | AjaxAbort) =>
        Some(ReadException(request, failure))
      case other =>
        SttpClientException.defaultExceptionToSttpClientException(
          request,
          other
        )
    }

  private val bodyFromResponseAs =
    new BodyFromResponseAs[EventStream, AjaxResponse, Nothing, Nothing] {

      override protected def withReplayableBody(
          response: AjaxResponse,
          replayableBody: Either[Array[Byte], SttpFile]
      ): EventStream[AjaxResponse] =
        replayableBody match {
          case Left(bytes) => monad.unit(response.copy(body = bytes))
          case Right(_)    => monad.error(unsupported("File response bodies"))
        }

      override protected def regularIgnore(
          response: AjaxResponse
      ): EventStream[Unit] = monad.unit(())

      override protected def regularAsByteArray(
          response: AjaxResponse
      ): EventStream[Array[Byte]] = monad.unit(response.body)

      override protected def regularAsFile(
          response: AjaxResponse,
          file: SttpFile
      ): EventStream[SttpFile] =
        monad.error(unsupported("File response bodies"))

      override protected def regularAsStream(
          response: AjaxResponse
      ): EventStream[(Nothing, () => EventStream[Unit])] =
        monad.error(unsupported("Streaming response bodies"))

      override protected def handleWS[T](
          responseAs: GenericWebSocketResponseAs[T, ?],
          meta: ResponseMetadata,
          ws: Nothing
      ): EventStream[T] = ws

      override protected def cleanupWhenNotAWebSocket(
          response: AjaxResponse,
          e: NotAWebSocketException
      ): EventStream[Unit] = monad.unit(())

      override protected def cleanupWhenGotWebSocket(
          response: Nothing,
          e: GotAWebSocketException
      ): EventStream[Unit] = response
    }
}

object AjaxBackend {

  final case class AjaxResponse(
      code: StatusCode,
      statusText: String,
      headers: Seq[Header],
      body: Array[Byte]
  ) {
    def metadata: ResponseMetadata = ResponseMetadata(code, statusText, headers)
  }

  object AjaxResponse {
    def apply(xhr: dom.XMLHttpRequest): AjaxResponse =
      AjaxResponse(
        code = StatusCode(xhr.status),
        statusText = xhr.statusText,
        headers = Option(xhr.getAllResponseHeaders()).toSeq
          .flatMap(_.split("\r\n"))
          .filter(_.contains(':'))
          .map { line =>
            val separator = line.indexOf(':')
            Header(line.take(separator).trim, line.drop(separator + 1).trim)
          },
        body = xhr.response match {
          case buffer: ArrayBuffer => new Int8Array(buffer).toArray
          case _                   => Array.emptyByteArray
        }
      )
  }

  // Set by the browser only: XMLHttpRequest refuses them, with a console error.
  private val forbiddenHeaders = Set(
    "accept-charset",
    "accept-encoding",
    "access-control-request-headers",
    "access-control-request-method",
    "connection",
    "content-length",
    "cookie",
    "cookie2",
    "date",
    "dnt",
    "expect",
    "host",
    "keep-alive",
    "origin",
    "referer",
    "set-cookie",
    "te",
    "trailer",
    "transfer-encoding",
    "upgrade",
    "via"
  )

  private def isForbidden(name: String): Boolean = {
    val lowerCaseName = name.toLowerCase
    forbiddenHeaders.contains(lowerCaseName) ||
    lowerCaseName.startsWith("proxy-") || lowerCaseName.startsWith("sec-")
  }

  private def requestHeaders(
      request: GenericRequest[?, ?]
  ): Map[String, String] =
    request.headers
      .filterNot(header => isForbidden(header.name))
      // FormData sets the multipart content type, with its boundary.
      .filterNot(header =>
        request.body.isInstanceOf[MultipartBody[?]] &&
          header.is(HeaderNames.ContentType)
      )
      .groupMap(_.name.toLowerCase)(_.value)
      .map((name, values) => name -> values.mkString(", "))

  private def timeoutMs(timeout: Duration): Int =
    timeout match {
      case finite: FiniteDuration => finite.toMillis.min(Int.MaxValue).toInt
      // XMLHttpRequest reads 0 as no timeout.
      case _ => 0
    }

  private def requestBody(body: GenericRequestBody[?]): AjaxStream.InputData =
    body match {
      case NoBody                      => null
      case part: BasicBodyPart         => basicBody(part)
      case multipart: MultipartBody[?] => formData(multipart)
      case StreamBody(_) => throw unsupported("Streaming request bodies")
    }

  private def basicBody(part: BasicBodyPart): AjaxStream.InputData =
    part match {
      case StringBody(content, encoding, _) =>
        if (encoding.equalsIgnoreCase("utf-8")) content
        else content.getBytes(encoding).toTypedArray
      case ByteArrayBody(bytes, _)   => bytes.toTypedArray
      case ByteBufferBody(buffer, _) =>
        val bytes = new Array[Byte](buffer.remaining())
        buffer.duplicate().get(bytes)
        bytes.toTypedArray
      case FileBody(file, _)     => file.toDomFile
      case InputStreamBody(_, _) =>
        throw unsupported("Input stream request bodies")
    }

  private def formData(multipart: MultipartBody[?]): dom.FormData = {
    val formData = new dom.FormData()
    multipart.parts.foreach { part =>
      val content: dom.Blob = part.body match {
        case FileBody(file, _)   => file.toDomFile
        case body: BasicBodyPart =>
          new dom.Blob(
            js.Array(basicBody(body).asInstanceOf[dom.BlobPart]),
            new dom.BlobPropertyBag { `type` = part.contentType.orUndefined }
          )
        case _ => throw unsupported("Streaming multipart bodies")
      }
      part.fileName match {
        case Some(fileName) => formData.append(part.name, content, fileName)
        case None           => formData.append(part.name, content)
      }
    }
    formData
  }

  private def unsupported(what: String): IllegalArgumentException =
    IllegalArgumentException(s"$what aren't supported by the Ajax backend")
}
