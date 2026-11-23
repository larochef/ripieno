package ripieno.shared.routes

import ripieno.shared.*
import ripieno.shared.Library.given

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

object LibraryRoutes {
  private val library = Routes.baseRoute.in("library")

  /** The library's home. */
  val shelves: PublicEndpoint[Unit, String, Shelves, Any] =
    library.get
      .in("shelves")
      .errorOut(Routes.unreadable)
      .out(jsonBody[Shelves])

  /** Works whose names hold those words (accents and case aside), for that
    * instrument if one is given.
    */
  val search: PublicEndpoint[(String, Option[String]), String, List[
    WorkSummary
  ], Any] =
    library.get
      .in("search")
      .in(query[String]("q").default(""))
      .in(query[Option[String]]("instrument"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[List[WorkSummary]])

  /** The works of a collection. */
  val collection: PublicEndpoint[String, String, List[WorkSummary], Any] =
    library.get
      .in("collection")
      .in(query[String]("name"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[List[WorkSummary]])

  val work: PublicEndpoint[Long, String, WorkDetail, Any] =
    library.get
      .in("works" / path[Long]("work"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[WorkDetail])

  /** Gives a part of a work another name. */
  val renamePart
      : PublicEndpoint[(Long, Long, String), String, WorkDetail, Any] =
    library.put
      .in("works" / path[Long]("work") / "parts" / path[Long]("part"))
      .in(stringBody)
      .errorOut(Routes.unreadable)
      .out(jsonBody[WorkDetail])

  /** Takes a work of the player's own out of the library, with all that was
    * kept of it.
    */
  val deleteWork: PublicEndpoint[Long, String, Unit, Any] =
    library.delete
      .in("works" / path[Long]("work"))
      .errorOut(Routes.unreadable)

  /** That the player opened movements of a work: what Continue offers. */
  val played: PublicEndpoint[(Long, Played), String, Unit, Any] =
    library.post
      .in("works" / path[Long]("work") / "played")
      .in(jsonBody[Played])
      .errorOut(Routes.unreadable)

  /** Makes a work of the player's own of the score files given. */
  val importScores: PublicEndpoint[ScoreImport, String, WorkDetail, Any] =
    library.post
      .in("works")
      .in(jsonBody[ScoreImport])
      .errorOut(Routes.unreadable)
      .out(jsonBody[WorkDetail])

  /** Pages engraved by the page, made into one PDF. */
  val exportPdf: PublicEndpoint[ExportPages, String, Array[Byte], Any] =
    library.post
      .in("export")
      .in(jsonBody[ExportPages])
      .errorOut(Routes.unreadable)
      .out(byteArrayBody)
      .out(header("Content-Type", "application/pdf"))
}
