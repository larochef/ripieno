package ripieno.shared.routes

import ripieno.shared.*
import ripieno.shared.PreparedScore.given
import ripieno.shared.ScoreRequest.given
import ripieno.shared.ScoreTiming.given

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

object ScoreRoutes {

  /** A score made ready to engrave: its editions played over it, its notes
    * given their ids, and timed with the tempi chosen and the cuts made.
    */
  val prepareScore: PublicEndpoint[ScoreRequest, String, PreparedScore, Any] =
    Routes.baseRoute.post
      .in("score" / "prepared")
      .in(jsonBody[ScoreRequest])
      .errorOut(Routes.unreadable)
      .out(jsonBody[PreparedScore])

  /** A score timed again: other tempi chosen, other cuts made. */
  val timeScore: PublicEndpoint[ScoreRequest, String, ScoreTiming, Any] =
    Routes.baseRoute.post
      .in("score" / "timing")
      .in(jsonBody[ScoreRequest])
      .errorOut(Routes.unreadable)
      .out(jsonBody[ScoreTiming])
}
