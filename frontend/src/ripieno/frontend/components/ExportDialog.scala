package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.WorkDetail

import com.raquo.laminar.api.L.*

/** Exports a work to PDF: the full score, one part, or an accompanist's score
  * (the part played at full size, the others smaller), the movements ticked,
  * with the player's editions or as printed.
  */
class ExportDialog(bus: AppBus, work: WorkDetail, open: Var[Boolean])
    extends ReactiveElement {
  private val layout = Var[ExportLayout](ExportLayout.Full)

  // the part an accompanist plays: the piano, else the continuo, else the
  // last part
  private val accompanied = Var(
    Seq("piano", "continuo")
      .flatMap(kind =>
        work.parts.indexWhere(_.instrument.contains(kind)) match {
          case -1    => None
          case found => Some(found)
        }
      )
      .headOption
      .getOrElse(math.max(0, work.parts.size - 1))
  )
  private val movements = Var(work.movements.map(_.id).toSet)
  private val edited = Var(true)
  private val compact = Var(false)

  private val working: Signal[Boolean] = bus.events.events
    .collect { case ExportProgress(now) => now }
    .startWith(false)

  private val failed: Signal[Option[String]] = bus.events.events
    .collect {
      case RequestFailed(_: ExportPdf, reason) => Some(reason)
      case ExportProgress(true)                => None
    }
    .startWith(None)

  // The part played at full size, the others smaller over and under it.
  private lazy val accompanistChoice: HtmlElement = div(
    cls := "dialog-choice",
    label(
      cls := "radio",
      input(
        typ := "radio",
        nameAttr := "export-parts",
        checked <-- layout.signal.map {
          case ExportLayout.Accompanying(_) => true
          case _                            => false
        },
        onChange.compose(_.sample(accompanied.signal)) --> (index =>
          layout.set(ExportLayout.Accompanying(index))
        )
      ),
      Tr(" For the accompanist: ", " Pour l'accompagnateur : ")
    ),
    div(
      cls := "select is-small",
      select(
        aria.label := Tr("The part you play", "La partie que vous jouez"),
        work.parts.zipWithIndex.map((found, index) =>
          option(value := index.toString, found.name)
        ),
        controlled(
          value <-- accompanied.signal.map(_.toString),
          onChange.mapToValue.map(_.toInt) --> { index =>
            accompanied.set(index)
            layout.set(ExportLayout.Accompanying(index))
          }
        )
      )
    ),
    span(
      cls := "dialog-note",
      Tr(
        " at full size, the other parts smaller",
        " en taille normale, les autres parties en plus petit"
      )
    )
  )

  override lazy val node: HtmlElement = div(
    cls := "modal",
    cls("is-active") <-- open.signal,
    div(cls := "modal-background", onClick --> (_ => open.set(false))),
    div(
      cls := "modal-card dialog",
      role := "dialog",
      aria.label := Tr("Export", "Exporter"),
      headerTag(
        cls := "modal-card-head",
        p(
          cls := "modal-card-title",
          Tr(s"Export · ${work.title}", s"Exporter · ${work.title}")
        ),
        button(
          cls := "delete",
          aria.label := Tr("Close", "Fermer"),
          onClick --> (_ => open.set(false))
        )
      ),
      sectionTag(
        cls := "modal-card-body dialog-body",
        span(cls := "dialog-label", Tr("Parts", "Parties")),
        label(
          cls := "radio dialog-choice",
          input(
            typ := "radio",
            nameAttr := "export-parts",
            checked <-- layout.signal.map(_ == ExportLayout.Full),
            onChange --> (_ => layout.set(ExportLayout.Full))
          ),
          Tr(" The full score", " La partition complète")
        ),
        work.parts.zipWithIndex.map((found, index) =>
          label(
            cls := "radio dialog-choice",
            input(
              typ := "radio",
              nameAttr := "export-parts",
              checked <-- layout.signal.map(_ == ExportLayout.Alone(index)),
              onChange --> (_ => layout.set(ExportLayout.Alone(index)))
            ),
            Tr(s" ${found.name} alone", s" ${found.name} seule")
          )
        ),
        Option.when(work.parts.sizeIs > 1)(accompanistChoice),
        Option.when(work.movements.sizeIs > 1)(
          div(
            span(cls := "dialog-label", Tr("Movements", "Mouvements")),
            work.movements.map(movement =>
              label(
                cls := "checkbox dialog-choice",
                input(
                  typ := "checkbox",
                  checked <-- movements.signal.map(_.contains(movement.id)),
                  onChange.mapToChecked --> (on =>
                    movements.update(set =>
                      if (on) set + movement.id else set - movement.id
                    )
                  )
                ),
                s" ${movement.position}. ${movement.title}"
              )
            )
          )
        ),
        span(cls := "dialog-label", Tr("With", "Avec")),
        label(
          cls := "checkbox dialog-choice",
          input(
            typ := "checkbox",
            checked <-- edited.signal,
            onChange.mapToChecked --> edited.writer
          ),
          Tr(
            " Your corrections and fingerings (the editions shown)",
            " Vos corrections et doigtés (les modifications affichées)"
          )
        ),
        label(
          cls := "checkbox dialog-choice",
          input(
            typ := "checkbox",
            idAttr := "export-compact",
            checked <-- compact.signal,
            onChange.mapToChecked --> compact.writer
          ),
          Tr(
            " More compact: the music up to 9 % smaller where that saves a page",
            " Plus compact : la musique jusqu'à 9 % plus petite si cela fait gagner une page"
          )
        ),
        p(
          cls := "dialog-note",
          Tr(
            "A PDF, A4, engraved as on the screen.",
            "Un PDF au format A4, gravé comme à l'écran."
          )
        ),
        child.maybe <-- failed.map(
          _.map(reason => p(cls := "notification is-danger is-light", reason))
        )
      ),
      footerTag(
        cls := "modal-card-foot dialog-foot",
        button(
          cls := "button",
          Tr("Close", "Fermer"),
          onClick --> (_ => open.set(false))
        ),
        button(
          cls := "button is-link",
          idAttr := "export-go",
          disabled <-- movements.signal
            .combineWith(working)
            .map((chosen, busy) => chosen.isEmpty || busy),
          child.text <-- working.map(
            if (_) Tr("Making the PDF…", "Création du PDF…")
            else Tr("Export as PDF", "Exporter en PDF")
          ),
          onClick --> (_ =>
            bus.send(
              ExportPdf(
                work,
                work.movements.map(_.id).filter(movements.now().contains),
                layout.now(),
                edited.now(),
                compact.now()
              )
            )
          )
        )
      )
    )
  )
}
