package ripieno.backend.scores

import ripieno.backend.library.LibraryStore
import ripieno.backend.reading.ScorePreparation
import ripieno.shared.Library
import ripieno.shared.routes.ScoreRoutes

import sttp.shared.Identity
import sttp.tapir.server.ServerEndpoint

/** The scores the page engraves: movements of the library, one or several in a
  * row (`movements/12,13`), or a file of the folder of scores by its path.
  */
final class ScoreApi(library: ScoreLibrary, movements: LibraryStore) {
  private def source(path: String): Either[String, String] =
    if (Library.movementsOf(path).isDefined) movements.musicXml(path)
    else library.musicXml(path)

  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    ScoreRoutes.prepareScore.serverLogic[Identity](request =>
      source(request.path).map(ScorePreparation.prepared(_, request))
    ),
    ScoreRoutes.timeScore.serverLogic[Identity](request =>
      source(request.path).map(ScorePreparation.timing(_, request))
    )
  )
}
