package ripieno.frontend.components

import ripieno.frontend.i18n.*
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.*

import com.raquo.laminar.api.L.*

/** The library: its home (what to go on with, the player's own works, what the
  * application provides), a collection's works, or what a search found.
  */
class LibraryPage(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val page: Signal[Page] =
    events.collect { case PageShown(now) => now }.startWith(Page.Home)

  private val shelves: Signal[Option[Shelves]] =
    events.collect { case ShelvesLoaded(now) => Some(now) }.startWith(None)

  private val listed: Signal[Option[WorksListed]] = events
    .collect {
      case found: WorksListed => Some(found)
      case PageShown(_)       => None
    }
    .startWith(None)

  private val failure: Signal[Option[String]] = events
    .collect {
      case RequestFailed(_: Navigate | _: ImportScores, reason) => Some(reason)
      case _: PageShown | _: ShelvesLoaded | _: WorksListed     => None
    }
    .startWith(None)

  private val importing = Var(false)

  // A link to a page of the application: it opens in another tab when asked,
  // here otherwise.
  private def link(page: Page, contents: Modifier[HtmlElement]*): HtmlElement =
    a(
      href := (if (page.query.isEmpty) "/" else page.query),
      onClick
        .filter(event => !event.ctrlKey && !event.metaKey)
        .preventDefault --> (_ => bus.send(Navigate(page))),
      contents
    )

  private def movementCount(count: Int): String =
    Tr(s"$count movements", s"$count mouvements")

  private def pieceCount(count: Int): String =
    Tr(
      s"$count piece${if (count > 1) "s" else ""}",
      s"$count pièce${if (count > 1) "s" else ""}"
    )

  private def workCard(work: WorkSummary): HtmlElement =
    link(
      Page.Work(work.id),
      cls := "work-card",
      span(cls := "work-card-title", work.title),
      span(
        cls := "work-card-meta",
        List(
          work.composer.map(CatalogWords.composer),
          Option.when(work.movements > 1)(movementCount(work.movements)),
          Option.when(work.parts.nonEmpty)(work.parts.take(4).mkString(", "))
        ).flatten.mkString(" · ")
      )
    )

  private def workRow(work: WorkSummary): HtmlElement =
    link(
      Page.Work(work.id),
      cls := "work-row",
      span(cls := "work-row-title", work.title),
      span(
        cls := "work-row-meta",
        List(
          work.composer.filterNot(_ == "Traditional"),
          Option.when(work.movements > 1)(movementCount(work.movements)),
          Option.when(work.parts.sizeIs > 1)(work.parts.mkString(", ")),
          Option.when(work.origin == "mine")(Tr("mine", "à moi"))
        ).flatten.mkString(" · ")
      )
    )

  private def recentCard(recent: RecentWork): HtmlElement =
    link(
      Page.Score(recent.work.id, recent.movements),
      cls := "work-card is-recent",
      span(cls := "work-card-title", recent.work.title),
      span(
        cls := "work-card-meta",
        (recent.work.composer.map(CatalogWords.composer).toList ++ List(
          recent.movementTitles.mkString(", ")
        ) ++ recent.part.toList).mkString(" · ")
      ),
      span(cls := "work-card-action", Tr("Go on ›", "Reprendre ›"))
    )

  private def collectionCard(collection: CollectionSummary): HtmlElement =
    link(
      Page.Collection(collection.name),
      cls := "work-card is-collection",
      span(cls := "work-card-title", CatalogWords.collection(collection.name)),
      span(
        cls := "work-card-meta",
        (collection.composer.map(CatalogWords.composer).toList :+ pieceCount(
          collection.works
        ))
          .mkString(" · ")
      )
    )

  private def section(
      title: String,
      note: String,
      content: HtmlElement
  ): HtmlElement =
    sectionTag(
      cls := "library-section",
      div(
        cls := "library-section-head",
        h2(cls := "library-section-title", title),
        span(cls := "library-section-note", note)
      ),
      content
    )

  private def home(found: Shelves): List[HtmlElement] =
    List(
      Option.when(found.recent.nonEmpty)(
        section(
          Tr("Continue", "Reprendre"),
          Tr("where you left off", "là où vous en étiez"),
          div(cls := "work-grid", found.recent.map(recentCard))
        )
      ),
      Some(
        section(
          Tr("My scores", "Mes partitions"),
          if (found.mine.isEmpty)
            Tr("nothing imported yet", "rien d'importé pour l'instant")
          else
            Tr(
              s"imported by you · ${found.mine.size}",
              s"importées par vous · ${found.mine.size}"
            ),
          div(
            cls := "work-grid",
            found.mine.map(workCard) :+ button(
              cls := "work-card is-import",
              Tr("＋ Import a score", "＋ Importer une partition"),
              onClick --> (_ => importing.set(true))
            )
          )
        )
      ),
      Some(
        section(
          Tr("From the app", "Fournies par l'application"),
          Tr(
            "free editions, public domain or open licences",
            "éditions libres, domaine public ou licences ouvertes"
          ),
          div(
            cls := "work-grid",
            found.collections.map(collectionCard) ++ found.single.map(workCard)
          )
        )
      )
    ).flatten

  private def listing(
      title: String,
      note: String,
      works: List[WorkSummary]
  ): List[HtmlElement] =
    List(
      div(
        cls := "library-crumbs",
        link(Page.Home, Tr("Library", "Bibliothèque")),
        span(" › "),
        span(title)
      ),
      section(
        title,
        note,
        if (works.isEmpty)
          p(cls := "library-empty", Tr("Nothing found.", "Aucun résultat."))
        else div(cls := "work-list", works.map(workRow))
      )
    )

  // Searches as the words are typed, once they stop for a moment.
  private val searchField: HtmlElement = input(
    cls := "input library-search",
    idAttr := "library-search",
    typ := "search",
    placeholder := Tr(
      "Search a title, a composer, a tune…",
      "Rechercher un titre, un compositeur, un air…"
    ),
    aria.label := Tr("Search the library", "Rechercher dans la bibliothèque"),
    value <-- page.map {
      case Page.Search(words, _) => words
      case _                     => ""
    },
    onInput.mapToValue.compose(
      _.debounce(350).withCurrentValueOf(page)
    ) --> { (words, now) =>
      val instrument = now match {
        case Page.Search(_, kind) => kind
        case _                    => None
      }
      val next =
        if (words.trim.isEmpty && instrument.isEmpty) Page.Home
        else Page.Search(words.trim, instrument)
      // the search refined stays one page of the history
      bus.send(Navigate(next, replace = now.isInstanceOf[Page.Search]))
    }
  )

  private val instrumentChips: HtmlElement = div(
    cls := "library-chips",
    children <-- shelves
      .map(_.fold(Nil)(_.instruments))
      .combineWith(page)
      .map { (instruments, now) =>
        val (words, chosen) = now match {
          case Page.Search(w, kind) => (w, kind)
          case _                    => ("", None)
        }
        instruments.map { kind =>
          val on = chosen.contains(kind)
          button(
            cls := "library-chip",
            cls("is-active") := on,
            Instruments.name(kind),
            onClick --> (_ =>
              bus.send(
                Navigate(
                  if (on && words.isEmpty) Page.Home
                  else Page.Search(words, Option.when(!on)(kind))
                )
              )
            )
          )
        }
      }
  )

  override lazy val node: HtmlElement = div(
    cls := "library-page",
    headerTag(
      cls := "library-header",
      link(
        Page.Home,
        cls := "brand",
        div(cls := "brand-mark", aria.hidden := true),
        span(cls := "brand-name", "Ripieno")
      ),
      searchField,
      instrumentChips,
      button(
        cls := "button is-link library-import",
        Tr("＋ Import a score", "＋ Importer une partition"),
        onClick --> (_ => importing.set(true))
      ),
      LanguageChoice()
    ),
    mainTag(
      cls := "library-main",
      child.maybe <-- failure.map(
        _.map(reason => div(cls := "notification is-danger is-light", reason))
      ),
      children <-- page.combineWith(shelves, listed).map {
        (now, home_, found) =>
          now match {
            case Page.Home =>
              home_.fold(
                List(
                  p(
                    cls := "library-empty",
                    Tr("Loading the library…", "Chargement de la bibliothèque…")
                  )
                )
              )(home)
            case Page.Collection(name) =>
              found.fold(
                List(p(cls := "library-empty", Tr("Loading…", "Chargement…")))
              )(f =>
                listing(
                  CatalogWords.collection(name),
                  pieceCount(f.works.size),
                  f.works
                )
              )
            case Page.Search(words, kind) =>
              val title =
                (Option.when(words.nonEmpty)(s"“$words”") ++ kind
                  .map(Instruments.name))
                  .mkString(" · ")
              found.fold(
                List(p(cls := "library-empty", Tr("Searching…", "Recherche…")))
              )(f =>
                listing(
                  title,
                  if (f.works.sizeIs >= 200)
                    Tr("the first 200", "les 200 premières")
                  else
                    Tr(
                      s"${f.works.size} found",
                      s"${f.works.size} trouvée${
                          if (f.works.sizeIs > 1) "s" else ""
                        }"
                    ),
                  f.works
                )
              )
            case _ => Nil
          }
      }
    ),
    ImportDialog(bus, importing).node
  )
}
