package ripieno.frontend

import ripieno.frontend.api.ApiClient
import ripieno.frontend.appServices.*
import ripieno.frontend.components.*
import ripieno.frontend.i18n.{Language, Tr}
import ripieno.frontend.model.{AppBus, Page}
import ripieno.frontend.model.AppEvent.PageShown
import ripieno.frontend.utils.AppStreams

import scala.scalajs.js

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sttp.model.Uri

object Application {

  private val baseUri: Uri = {
    val location = dom.window.location
    Uri.unsafeParse(s"${location.protocol}//${location.host}")
  }

  private val bus = AppBus()

  private val apiClient = ApiClient(baseUri)

  private val page: Signal[Page] =
    bus.events.events
      .collect { case PageShown(now) => now }
      .startWith(Page.Home)

  // Every page stays mounted, the others hidden: what each shows follows the
  // bus's events, which a page away would miss.
  private lazy val shell: HtmlElement = div(
    cls := "layout",
    div(
      cls := "page-library",
      hidden <-- page.map {
        case Page.Home | _: Page.Search | _: Page.Collection => false
        case _                                               => true
      },
      LibraryPage(bus)
    ),
    div(
      cls := "page-work",
      hidden <-- page.map(!_.isInstanceOf[Page.Work]),
      WorkPage(bus)
    ),
    div(
      cls := "stage",
      hidden <-- page.map(!_.isInstanceOf[Page.Score]),
      ScoreHeader(bus),
      div(cls := "sheet-tools", ScoreEditor(bus), RangeBar(bus)),
      div(
        cls := "stage-body",
        // the pencil floats over the sheet's corner
        div(cls := "sheet-area", ScoreSheet(bus), AnnotateButton(bus)),
        // beside the sheet, so as to leave it its height
        div(
          cls := "side-panel",
          ChecksPanel(bus),
          TunerPanel(bus),
          MetronomePanel(bus),
          TempoPanel(bus),
          PassagesPanel(bus),
          SoundsPanel(bus),
          PerformanceReviewPanel(bus),
          RecordingHistory(bus)
        )
      ),
      PlayerBar(bus)
    )
  )

  private def failure(reason: String): HtmlElement = {
    div(
      cls := "notification is-danger m-4",
      Tr(
        s"The engraver could not be loaded: $reason",
        s"Le graveur n'a pas pu être chargé : $reason"
      )
    )
  }

  // Services live through their subscription to the bus.
  private def start(verovio: js.Any): Unit = {
    dom.document.documentElement.setAttribute("lang", Language.current.code)
    LibraryService(bus, apiClient)
    ScoreService(bus, apiClient, verovio)
    EditionService(bus, apiClient)
    PlaybackService(bus, apiClient)
    ReviewService(bus, apiClient)
    MetronomeService(bus)
    PageService(bus)
    TunerService(bus)
    ExportService(bus, apiClient, verovio)
    val navigation = NavigationService(bus)
    render(appContainer, shell)
    navigation.showAddressed()
  }

  private lazy val appContainer: dom.Element =
    dom.document.querySelector("#app")

  // Nothing can be shown before the engraver, a WebAssembly module, is there.
  def main(args: Array[String]): Unit = {
    AppStreams.runOnce(
      AppStreams.fromPromise(Libraries.createVerovioModule())
    ) {
      case Right(verovio) => start(verovio)
      case Left(reason)   => render(appContainer, failure(reason))
    }
  }
}
