package ripieno.backend.tempi

import ripieno.shared.routes.TempoRoutes

import sttp.shared.Identity
import sttp.tapir.server.ServerEndpoint

final class TempoApi(store: TempoStore) {
  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    TempoRoutes.tempi.serverLogic[Identity](store.tempi),
    TempoRoutes.saveTempi.serverLogic[Identity](store.save)
  )
}
