package ripieno.frontend.appServices

import ripieno.frontend.api.ApiClient
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.utils.AppStreams
import ripieno.shared.*

import com.raquo.laminar.api.L.*

/** The library as the pages show it: the home's shelves, a collection, what a
  * search finds, a work; and what the player changes of it.
  */
class LibraryService(bus: AppBus, apiClient: ApiClient) {
  // the work whose movements are on the sheet, to say what is played of it
  private var playing: Option[Page.Score] = None
  private var lastWork: Option[WorkDetail] = None

  bus.events.events.foreach {
    case PageShown(page)     => load(page)
    case PartChosen(part, _) =>
      for {
        score <- playing
        work <- lastWork
      } {
        val name = part.flatMap(work.parts.lift).map(_.name)
        AppStreams.runOnce(
          apiClient.played(score.work, Played(score.movements, name))
        )(_ => ())
      }
    case WorkLoaded(work) => lastWork = Some(work)
    case _                => ()
  }(using unsafeWindowOwner)

  bus.commands.events.foreach {
    case command @ RenamePart(work, part, name) =>
      AppStreams.runOnce(apiClient.renamePart(work, part, name)) {
        case Right(renamed) => bus.publish(WorkLoaded(renamed))
        case Left(reason)   => bus.publish(RequestFailed(command, reason))
      }
    case command @ ImportScores(scores) =>
      AppStreams.runOnce(apiClient.importScores(scores)) {
        case Right(work)  => bus.send(Navigate(Page.Work(work.id)))
        case Left(reason) => bus.publish(RequestFailed(command, reason))
      }
    case _ => ()
  }(using unsafeWindowOwner)

  private def load(page: Page): Unit = page match {
    case Page.Home =>
      playing = None
      AppStreams.runOnce(apiClient.shelves()) {
        case Right(shelves) => bus.publish(ShelvesLoaded(shelves))
        case Left(reason)   =>
          bus.publish(
            RequestFailed(
              Navigate(page),
              Tr(s"No library: $reason", s"Pas de bibliothèque : $reason")
            )
          )
      }
    case Page.Collection(name) =>
      playing = None
      list(page, apiClient.collection(name))
    case Page.Search(words, instrument) =>
      playing = None
      list(page, apiClient.search(words, instrument))
    case Page.Work(id) =>
      playing = None
      work(page, id)
    case score @ Page.Score(id, movements) =>
      playing = Some(score)
      if (!lastWork.exists(_.id == id)) work(page, id)
      bus.send(OpenScore(Library.movementsPath(movements)))
      AppStreams.runOnce(apiClient.played(id, Played(movements, None)))(_ => ())
  }

  private def list(
      page: Page,
      works: EventStream[AppResult[List[WorkSummary]]]
  ): Unit =
    AppStreams.runOnce(works) {
      case Right(found) => bus.publish(WorksListed(page, found))
      case Left(reason) => bus.publish(RequestFailed(Navigate(page), reason))
    }

  private def work(page: Page, id: Long): Unit =
    AppStreams.runOnce(apiClient.work(id)) {
      case Right(found) => bus.publish(WorkLoaded(found))
      case Left(reason) => bus.publish(RequestFailed(Navigate(page), reason))
    }
}
