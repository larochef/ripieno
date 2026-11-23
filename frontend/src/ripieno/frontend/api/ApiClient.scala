package ripieno.frontend.api

import ripieno.frontend.api.airstream.AjaxBackend
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.AppResult
import ripieno.frontend.utils.AppStreams
import ripieno.shared.*
import ripieno.shared.routes.*

import scala.scalajs.js.typedarray.*
import scala.util.{Failure, Success}

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sttp.client4.Backend
import sttp.model.Uri
import sttp.tapir.PublicEndpoint
import sttp.tapir.client.sttp4.SttpClientInterpreter

// Every result is an Either: failures travel as values, not stream errors.
// A stream keeps no result: restarting it sends the request again.
class ApiClient(baseUri: Uri) {
  private val backend: Backend[EventStream] = AjaxBackend()
  private val interpreter = SttpClientInterpreter()

  def shelves(): EventStream[AppResult[Shelves]] =
    call(LibraryRoutes.shelves, ())

  def search(
      words: String,
      instrument: Option[String]
  ): EventStream[AppResult[List[WorkSummary]]] =
    call(LibraryRoutes.search, (words, instrument))

  def collection(name: String): EventStream[AppResult[List[WorkSummary]]] =
    call(LibraryRoutes.collection, name)

  def work(id: Long): EventStream[AppResult[WorkDetail]] =
    call(LibraryRoutes.work, id)

  def renamePart(
      work: Long,
      part: Long,
      name: String
  ): EventStream[AppResult[WorkDetail]] =
    call(LibraryRoutes.renamePart, (work, part, name))

  def played(work: Long, played: Played): EventStream[AppResult[Unit]] =
    call(LibraryRoutes.played, (work, played))

  def importScores(scores: ScoreImport): EventStream[AppResult[WorkDetail]] =
    call(LibraryRoutes.importScores, scores)

  def exportPdf(pages: ExportPages): EventStream[AppResult[Array[Byte]]] =
    call(LibraryRoutes.exportPdf, pages)

  def prepareScore(
      request: ScoreRequest
  ): EventStream[AppResult[PreparedScore]] =
    call(ScoreRoutes.prepareScore, request)

  def timeScore(request: ScoreRequest): EventStream[AppResult[ScoreTiming]] =
    call(ScoreRoutes.timeScore, request)

  def renderSound(
      engine: String,
      midi: Uint8Array
  ): EventStream[AppResult[RenderedSound]] =
    call(
      SoundRoutes.renderSound,
      (engine, new Int8Array(midi.buffer, midi.byteOffset, midi.length).toArray)
    )

  def renderedSound(id: String): EventStream[AppResult[RenderedSound]] =
    call(SoundRoutes.renderedSound, id)

  def editions(path: String): EventStream[AppResult[Editions]] =
    call(EditionRoutes.editions, path)

  def tempi(path: String): EventStream[AppResult[ScoreTempi]] =
    call(TempoRoutes.tempi, path)

  def saveTempi(
      path: String,
      tempi: ScoreTempi
  ): EventStream[AppResult[ScoreTempi]] =
    call(TempoRoutes.saveTempi, (path, tempi))

  def saveEditions(
      path: String,
      editions: Editions
  ): EventStream[AppResult[Editions]] =
    call(EditionRoutes.saveEditions, (path, editions))

  def microphoneLevel(): EventStream[AppResult[MicrophoneLevel]] =
    call(MicrophoneRoutes.level, ())

  def setMicrophoneLevel(
      volume: Double
  ): EventStream[AppResult[MicrophoneLevel]] =
    call(MicrophoneRoutes.setLevel, volume)

  def listReviews(score: String): EventStream[AppResult[ReviewSummaries]] =
    call(ReviewRoutes.listReviews, score)

  def deleteReview(id: String): EventStream[AppResult[Unit]] =
    call(ReviewRoutes.deleteReview, id)

  def startReview(
      reference: PerformanceReference
  ): EventStream[AppResult[ReviewStatus]] =
    call(ReviewRoutes.startReview, reference)

  def sendRecording(
      id: String,
      recording: ArrayBuffer
  ): EventStream[AppResult[ReviewStatus]] =
    call(ReviewRoutes.sendRecording, (id, new Int8Array(recording).toArray))

  def review(id: String): EventStream[AppResult[ReviewStatus]] =
    call(ReviewRoutes.review, id)

  // Tens of megabytes handed to the synthesizer as they are: fetched as a
  // buffer, not through sttp, which would copy them into a Scala array.
  def soundBank(): EventStream[AppResult[ArrayBuffer]] = {
    AppStreams
      .fromPromise(dom.fetch(SoundRoutes.soundBankPath))
      .flatMapSwitch {
        case Right(response) if response.ok =>
          AppStreams.fromPromise(response.arrayBuffer())
        case Right(_) =>
          EventStream.fromValue(
            Left(
              Tr(
                "the server has no sound bank",
                "le serveur n'a pas de banque de sons"
              )
            )
          )
        case Left(reason) => EventStream.fromValue(Left(reason))
      }
  }

  private def call[I, O](
      endpoint: PublicEndpoint[I, String, O, Any],
      input: I
  ): EventStream[AppResult[O]] = {
    val request = interpreter
      .toRequestThrowDecodeFailures(endpoint, Some(baseUri))
      .apply(input)
    backend.send(request).recoverToTry.map {
      case Success(response) => response.body
      case Failure(error)    =>
        Left(
          Tr(
            s"The server could not be reached: ${error.getMessage}",
            s"Le serveur est injoignable : ${error.getMessage}"
          )
        )
    }
  }
}
