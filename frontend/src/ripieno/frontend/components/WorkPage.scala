package ripieno.frontend.components

import ripieno.frontend.i18n.*
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.*

import com.raquo.laminar.api.L.*

/** A work: its movements, to open one or several in a row; its parts, which the
  * player may rename; where it comes from; and its export.
  */
class WorkPage(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val work: Signal[Option[WorkDetail]] = events
    .collect {
      case WorkLoaded(found)        => Some(found)
      case PageShown(Page.Work(id)) => None
    }
    .startWith(None)

  private val failure: Signal[Option[String]] = events
    .collect {
      case RequestFailed(_: Navigate | _: RenamePart, reason) => Some(reason)
      case _: PageShown | _: WorkLoaded                       => None
    }
    .startWith(None)

  // the movements ticked to be opened in a row
  private val ticked = Var(Set.empty[Long])
  private val exporting = Var(false)

  private def link(page: Page, contents: Modifier[HtmlElement]*): HtmlElement =
    a(
      href := (if (page.query.isEmpty) "/" else page.query),
      onClick
        .filter(event => !event.ctrlKey && !event.metaKey)
        .preventDefault --> (_ => bus.send(Navigate(page))),
      contents
    )

  private def roman(number: Int): String =
    List(10 -> "X", 9 -> "IX", 5 -> "V", 4 -> "IV", 1 -> "I")
      .foldLeft((number, "")) { case ((left, written), (value, letters)) =>
        (left % value, written + letters * (left / value))
      }
      ._2

  private def movementRow(
      found: WorkDetail,
      movement: MovementSummary
  ): HtmlElement =
    div(
      cls := "movement-row",
      Option.when(found.movements.sizeIs > 1)(
        label(
          cls := "movement-tick",
          input(
            typ := "checkbox",
            aria.label := Tr(
              s"Play ${movement.title} in a row with the others ticked",
              s"Enchaîner ${movement.title} avec les autres mouvements cochés"
            ),
            checked <-- ticked.signal.map(_.contains(movement.id)),
            onChange.mapToChecked --> (on =>
              ticked.update(set =>
                if (on) set + movement.id else set - movement.id
              )
            )
          )
        )
      ),
      span(cls := "movement-number", roman(movement.position)),
      div(
        cls := "movement-name",
        span(cls := "movement-title", movement.title),
        span(
          cls := "movement-meta",
          List(
            movement.measures.map(count =>
              Tr(
                s"$count bars",
                s"$count mesure${if (count > 1) "s" else ""}"
              )
            ),
            Option.when(movement.recordings > 0)({
              val count = movement.recordings
              val plural = if (count > 1) "s" else ""
              Tr(
                s"$count recording$plural",
                s"$count enregistrement$plural"
              )
            }),
            Option.when(movement.checks > 0)({
              val count = movement.checks
              Tr(
                s"$count places to check",
                s"$count passage${if (count > 1) "s" else ""} à vérifier"
              )
            })
          ).flatten.mkString(" · ")
        )
      ),
      link(
        Page.Score(found.id, List(movement.id)),
        cls := "button is-link movement-open",
        Tr("Open", "Ouvrir")
      )
    )

  private def inRow(found: WorkDetail): HtmlElement =
    div(
      cls := "movements-row",
      child <-- ticked.signal.map { chosen =>
        val inOrder = found.movements.filter(m => chosen.contains(m.id))
        if (inOrder.sizeIs < 2)
          span(
            cls := "movements-row-note",
            Tr(
              "Tick two movements or more to play them one after the other.",
              "Cochez au moins deux mouvements pour les enchaîner."
            )
          )
        else
          div(
            cls := "movements-row-go",
            span({
              val numbers = inOrder.map(m => roman(m.position)).mkString(", ")
              Tr(s"$numbers, one after the other", s"$numbers, enchaînés")
            }),
            link(
              Page.Score(found.id, inOrder.map(_.id)),
              cls := "button is-link",
              idAttr := "open-in-row",
              Tr(
                s"Open ${inOrder.size} movements",
                s"Ouvrir ${inOrder.size} mouvements"
              )
            )
          )
      },
      button(
        cls := "button is-small is-text",
        Tr("Tick all", "Tout cocher"),
        onClick --> (_ => ticked.set(found.movements.map(_.id).toSet))
      )
    )

  private def partRow(found: WorkDetail, part: PartSummary): HtmlElement =
    div(
      cls := "part-row",
      input(
        cls := "input part-name",
        aria.label := Tr(
          s"Name of part ${part.position}",
          s"Nom de la partie ${part.position}"
        ),
        value := part.name,
        onChange.mapToValue
          .map(_.trim)
          .filter(name => name.nonEmpty && name != part.name) --> (name =>
          bus.send(RenamePart(found.id, part.id, name))
        )
      ),
      span(cls := "part-instrument", part.instrument.fold("")(Instruments.name))
    )

  private def detail(found: WorkDetail): HtmlElement =
    div(
      cls := "work-page-body",
      sectionTag(
        cls := "work-movements",
        h2(cls := "library-section-title", Tr("Movements", "Mouvements")),
        div(cls := "movement-list", found.movements.map(movementRow(found, _))),
        Option.when(found.movements.sizeIs > 1)(inRow(found))
      ),
      asideTag(
        cls := "work-aside",
        h2(cls := "library-section-title", Tr("Parts", "Parties")),
        div(cls := "part-list", found.parts.map(partRow(found, _))),
        p(
          cls := "dialog-note",
          Tr(
            "Names are yours to change: the sheet and the recordings use them.",
            "Vous pouvez changer ces noms : la partition et les enregistrements les reprennent."
          )
        ),
        h2(
          cls := "library-section-title",
          Tr("Where it comes from", "Provenance")
        ),
        div(
          cls := "work-source",
          List(
            found.edition.map(p(_)),
            found.licence.map(text => p(cls := "dialog-note", text)),
            Option.when(found.sources.nonEmpty)(
              p(
                cls := "dialog-note",
                Tr(
                  s"Imported from ${found.sources.mkString(", ")}",
                  s"Importée de ${found.sources.mkString(", ")}"
                )
              )
            )
          ).flatten
        )
      )
    )

  override lazy val node: HtmlElement = div(
    cls := "library-page work-page",
    headerTag(
      cls := "library-header",
      link(
        Page.Home,
        cls := "brand",
        div(cls := "brand-mark", aria.hidden := true),
        span(cls := "brand-name", "Ripieno")
      ),
      div(
        cls := "work-heading",
        child.maybe <-- work.map(_.map { found =>
          div(
            div(
              cls := "library-crumbs",
              link(Page.Home, Tr("Library", "Bibliothèque")),
              found.collection.toList.flatMap(name =>
                List(
                  span(" › "),
                  link(Page.Collection(name), CatalogWords.collection(name))
                )
              )
            ),
            h1(cls := "work-title", found.title),
            p(
              cls := "work-subtitle",
              (found.composer
                .map(CatalogWords.composer)
                .toList ++ found.arranger.map(a =>
                Tr(s"arranged by $a", s"arrangé par $a")
              )).mkString(" · ")
            )
          )
        })
      ),
      LanguageChoice(),
      button(
        cls := "button library-import",
        idAttr := "export",
        disabled <-- work.map(_.isEmpty),
        Tr("Export…", "Exporter…"),
        onClick --> (_ => exporting.set(true))
      )
    ),
    mainTag(
      cls := "library-main",
      child.maybe <-- failure.map(
        _.map(reason => div(cls := "notification is-danger is-light", reason))
      ),
      child <-- work.map(
        _.fold(
          p(
            cls := "library-empty",
            Tr("Loading the work…", "Chargement de l'œuvre…")
          )
        )(detail)
      )
    ),
    // a new work: nothing ticked
    work.changes.map(_.map(_.id)).distinct --> (_ => ticked.set(Set.empty)),
    child.maybe <-- work.map(
      _.map(found => ExportDialog(bus, found, exporting).node)
    )
  )
}
