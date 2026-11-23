package ripieno.backend.editions

import ripieno.shared.routes.EditionRoutes

import sttp.shared.Identity
import sttp.tapir.server.ServerEndpoint

final class EditionApi(store: EditionStore) {
  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    EditionRoutes.editions.serverLogic[Identity](store.editions),
    EditionRoutes.saveEditions.serverLogic[Identity](store.save)
  )
}
