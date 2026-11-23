package ripieno.backend.library

import ripieno.shared.routes.LibraryRoutes

import sttp.shared.Identity
import sttp.tapir.server.ServerEndpoint

final class LibraryApi(
    library: LibraryStore,
    removal: WorkRemoval,
    pdfExport: PdfExport
) {
  val endpoints: List[ServerEndpoint[Any, Identity]] = List(
    LibraryRoutes.shelves.serverLogic[Identity](_ => library.shelves()),
    LibraryRoutes.search.serverLogic[Identity](library.search),
    LibraryRoutes.collection.serverLogic[Identity](library.collection),
    LibraryRoutes.work.serverLogic[Identity](library.work),
    LibraryRoutes.renamePart.serverLogic[Identity](library.renamePart),
    LibraryRoutes.deleteWork.serverLogic[Identity](removal.delete),
    LibraryRoutes.played.serverLogic[Identity](library.played),
    LibraryRoutes.importScores.serverLogic[Identity](library.importScores),
    LibraryRoutes.exportPdf.serverLogic[Identity](pdfExport.pdf)
  )
}
