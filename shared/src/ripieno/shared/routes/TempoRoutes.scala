package ripieno.shared.routes

import ripieno.shared.ScoreTempi

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

object TempoRoutes {

  /** The tempi a score's sections are played at, by its path in the library;
    * none kept yet is none.
    */
  val tempi: PublicEndpoint[String, String, ScoreTempi, Any] =
    Routes.baseRoute.get
      .in("tempi")
      .in(query[String]("path"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[ScoreTempi])

  /** Keeps the tempi of a score's sections, replacing those kept before. */
  val saveTempi: PublicEndpoint[(String, ScoreTempi), String, ScoreTempi, Any] =
    Routes.baseRoute.put
      .in("tempi")
      .in(query[String]("path"))
      .in(jsonBody[ScoreTempi])
      .errorOut(Routes.unreadable)
      .out(jsonBody[ScoreTempi])
}
