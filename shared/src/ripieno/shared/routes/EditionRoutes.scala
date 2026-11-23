package ripieno.shared.routes

import ripieno.shared.Editions

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

object EditionRoutes {

  /** The editions of a score, by its path in the library; none yet is no
    * edition.
    */
  val editions: PublicEndpoint[String, String, Editions, Any] =
    Routes.baseRoute.get
      .in("editions")
      .in(query[String]("path"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[Editions])

  /** Keeps the editions of a score, replacing those kept before. */
  val saveEditions: PublicEndpoint[(String, Editions), String, Editions, Any] =
    Routes.baseRoute.put
      .in("editions")
      .in(query[String]("path"))
      .in(jsonBody[Editions])
      .errorOut(Routes.unreadable)
      .out(jsonBody[Editions])
}
