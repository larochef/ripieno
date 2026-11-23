package ripieno.shared.routes

import sttp.model.StatusCode
import sttp.tapir.*

object Routes {

  /** Every endpoint's `/api` prefix: Vite's dev proxy forwards nothing else,
    * and the backend answers any other path with the page's shell.
    */
  val baseRoute: PublicEndpoint[Unit, Unit, Unit, Any] = endpoint.in("api")

  /** What was asked for does not exist or cannot be read, and why. */
  val unreadable: EndpointOutput[String] =
    statusCode(StatusCode.UnprocessableEntity).and(stringBody)
}
