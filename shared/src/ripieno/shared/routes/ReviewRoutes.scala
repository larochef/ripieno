package ripieno.shared.routes

import ripieno.shared.*

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

object ReviewRoutes {

  /** Starts the review of a recording of a part: the part first, then its
    * recording (`sendRecording`), then the review is asked about until it is
    * ready.
    */
  val startReview
      : PublicEndpoint[PerformanceReference, String, ReviewStatus, Any] =
    Routes.baseRoute.post
      .in("reviews")
      .in(jsonBody[PerformanceReference])
      .errorOut(Routes.unreadable)
      .out(jsonBody[ReviewStatus])

  /** The recording, as the browser recorded it (WebM or Ogg, in Opus). */
  val sendRecording
      : PublicEndpoint[(String, Array[Byte]), String, ReviewStatus, Any] =
    Routes.baseRoute.put
      .in("reviews" / path[String]("id") / "recording")
      .in(byteArrayBody)
      .errorOut(Routes.unreadable)
      .out(jsonBody[ReviewStatus])

  val review: PublicEndpoint[String, String, ReviewStatus, Any] =
    Routes.baseRoute.get
      .in("reviews" / path[String]("id"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[ReviewStatus])

  /** The recordings reviewed of a score, by its path in the library, the latest
    * first.
    */
  val listReviews: PublicEndpoint[String, String, ReviewSummaries, Any] =
    Routes.baseRoute.get
      .in("reviews")
      .in(query[String]("score"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[ReviewSummaries])

  /** Throws a recording and its review away. */
  val deleteReview: PublicEndpoint[String, String, Unit, Any] =
    Routes.baseRoute.delete
      .in("reviews" / path[String]("id"))
      .errorOut(Routes.unreadable)

  /** Where a review's recording is played back from. A plain path: it is the
    * source of an audio element, which asks for the parts of the file it needs.
    */
  def recordingAudioPath(id: String): String = s"/api/recordings/$id"
}
