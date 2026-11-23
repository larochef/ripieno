package ripieno.shared.routes

import ripieno.shared.MicrophoneLevel

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

object MicrophoneRoutes {

  val level: PublicEndpoint[Unit, String, MicrophoneLevel, Any] =
    Routes.baseRoute.get
      .in("microphone")
      .errorOut(Routes.unreadable)
      .out(jsonBody[MicrophoneLevel])

  /** Sets the input volume of the server machine's microphone, from 0 to 1. */
  val setLevel: PublicEndpoint[Double, String, MicrophoneLevel, Any] =
    Routes.baseRoute.put
      .in("microphone")
      .in(query[Double]("volume"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[MicrophoneLevel])
}
