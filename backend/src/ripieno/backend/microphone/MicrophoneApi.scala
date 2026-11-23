package ripieno.backend.microphone

import ripieno.shared.routes.MicrophoneRoutes

import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint

final class MicrophoneApi(microphone: Microphone) {
  // where the request comes from: the volume is only set for this machine
  private val remote = extractFromRequest(_.connectionInfo.remote)

  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    MicrophoneRoutes.level.in(remote).serverLogic[Identity](microphone.level),
    MicrophoneRoutes.setLevel
      .in(remote)
      .serverLogic[Identity](microphone.setLevel)
  )
}
