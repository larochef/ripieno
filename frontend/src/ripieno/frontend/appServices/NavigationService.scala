package ripieno.frontend.appServices

import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Which page is shown, kept in the page's address: a link to it opens it
  * again, and the browser's Back goes back to the page before.
  */
class NavigationService(bus: AppBus) {
  private var current: Option[Page] = None

  bus.commands.events.foreach {
    case Navigate(page, replace) =>
      if (!current.contains(page)) {
        if (replace) dom.window.history.replaceState(null, "", address(page))
        else dom.window.history.pushState(null, "", address(page))
        show(page)
      }
    case _ => ()
  }(using unsafeWindowOwner)

  // the browser's Back and Forward
  dom.window.addEventListener(
    "popstate",
    (_: dom.Event) => show(addressed())
  )

  /** Shows the page the address names, once the page has started. */
  def showAddressed(): Unit = show(addressed())

  private def show(page: Page): Unit = {
    current = Some(page)
    dom.window.scrollTo(0, 0)
    bus.publish(PageShown(page))
  }

  private def address(page: Page): String = {
    val query = page.query
    if (query.isEmpty) dom.window.location.pathname else query
  }

  private def addressed(): Page = {
    val parameters = new dom.URLSearchParams(dom.window.location.search)
    val names = List("work", "movements", "collection", "search", "instrument")
    Page.of(
      names.flatMap(name => Option(parameters.get(name)).map(name -> _)).toMap
    )
  }
}
