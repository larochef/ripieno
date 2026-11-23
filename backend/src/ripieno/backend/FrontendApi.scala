package ripieno.backend

import scala.io.Source
import scala.util.Using

import sttp.model.StatusCode
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.files.*
import sttp.tapir.server.ServerEndpoint

/** Serves the page: its bundled assets, and its shell for any other path. */
object FrontendApi {
  private def classLoader: ClassLoader =
    Thread.currentThread().getContextClassLoader

  private def shell: String = {
    Using.resource(
      Source.fromInputStream(
        classLoader.getResourceAsStream("index.html"),
        "UTF-8"
      )
    )(_.mkString)
  }

  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    staticResourcesGetServerEndpoint[Identity]("assets")(classLoader, "assets"),
    endpoint.get
      .in(paths)
      .errorOut(statusCode(StatusCode.NotFound))
      .out(header("Content-Type", "text/html; charset=utf-8"))
      // The shell must always be revalidated: a cached index.html keeps
      // referencing an old hashed bundle.
      .out(header("Cache-Control", "no-cache"))
      .out(stringBody)
      .serverLogic[Identity] { segments =>
        // An unmatched `/api/...` path is a mistake, not a page.
        if (segments.headOption.contains("api")) Left(()) else Right(shell)
      }
  )
}
