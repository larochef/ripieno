package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.{ImportedFile, ScoreImport}

import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Imports score files the player has as a work of their own: MusicXML (plain
  * or compressed) or ABC, one movement per file, in the order chosen.
  */
class ImportDialog(bus: AppBus, open: Var[Boolean]) extends ReactiveElement {
  private val files = Var(List.empty[ImportedFile])
  private val title = Var("")
  private val composer = Var("")
  private val sending = Var(false)

  private val failed: Signal[Option[String]] = bus.events.events
    .collect {
      case RequestFailed(_: ImportScores, reason) => Some(reason)
      case _: PageShown                           => None
    }
    .startWith(None)

  // A file's bytes in base 64, as the server takes them.
  private def read(file: dom.File): js.Promise[ImportedFile] =
    file
      .asInstanceOf[js.Dynamic]
      .arrayBuffer()
      .asInstanceOf[js.Promise[ArrayBuffer]]
      .`then`[ImportedFile] { buffer =>
        val bytes = new Uint8Array(buffer)
        val text = new StringBuilder
        for (index <- 0 until bytes.length) {
          text.append(bytes(index).toChar)
        }
        ImportedFile(file.name, dom.window.btoa(text.toString))
      }

  private def chosen(list: dom.FileList): Unit = {
    val picked = (0 until list.length).map(list(_)).toList.sortBy(_.name)
    js.Promise
      .all(js.Array(picked.map(read)*))
      .`then`[Unit] { read =>
        files.set(read.toList)
        if (title.now().isEmpty) {
          title.set(
            picked.headOption.fold("")(
              _.name
                .replaceFirst("\\.[^.]+$", "")
                .replace('-', ' ')
                .replace('_', ' ')
            )
          )
        }
      }
  }

  private def close(): Unit = {
    open.set(false)
    sending.set(false)
    files.set(Nil)
    title.set("")
    composer.set("")
  }

  override lazy val node: HtmlElement = div(
    cls := "modal",
    cls("is-active") <-- open.signal,
    div(cls := "modal-background", onClick --> (_ => close())),
    div(
      cls := "modal-card dialog",
      role := "dialog",
      aria.label := Tr("Import a score", "Importer une partition"),
      headerTag(
        cls := "modal-card-head",
        p(
          cls := "modal-card-title",
          Tr("Import a score", "Importer une partition")
        ),
        button(
          cls := "delete",
          aria.label := Tr("Close", "Fermer"),
          onClick --> (_ => close())
        )
      ),
      sectionTag(
        cls := "modal-card-body dialog-body",
        label(
          cls := "dialog-label",
          forId := "import-files",
          Tr("Files", "Fichiers")
        ),
        input(
          idAttr := "import-files",
          typ := "file",
          multiple := true,
          accept := ".musicxml,.xml,.mxl,.abc",
          inContext(field =>
            onChange --> (_ => Option(field.ref.files).foreach(chosen))
          )
        ),
        p(
          cls := "dialog-note",
          Tr(
            "MusicXML (.musicxml, .xml, .mxl) or ABC: one movement per file, in their names' order. Scans (PDF) come next.",
            "MusicXML (.musicxml, .xml, .mxl) ou ABC : un mouvement par fichier, dans l'ordre de leurs noms. Les partitions scannées (PDF) viendront ensuite."
          )
        ),
        ul(
          cls := "dialog-files",
          children <-- files.signal.map(
            _.zipWithIndex.map((file, index) =>
              li(s"${index + 1}. ${file.name}")
            )
          )
        ),
        label(
          cls := "dialog-label",
          forId := "import-title",
          Tr("Title", "Titre")
        ),
        input(
          cls := "input",
          idAttr := "import-title",
          controlled(
            value <-- title.signal,
            onInput.mapToValue --> title.writer
          )
        ),
        label(
          cls := "dialog-label",
          forId := "import-composer",
          Tr("Composer", "Compositeur")
        ),
        input(
          cls := "input",
          idAttr := "import-composer",
          controlled(
            value <-- composer.signal,
            onInput.mapToValue --> composer.writer
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
          Tr("Cancel", "Annuler"),
          onClick --> (_ => close())
        ),
        button(
          cls := "button is-link",
          idAttr := "import-go",
          disabled <-- files.signal
            .combineWith(title.signal, sending.signal)
            .map((chosen, named, busy) =>
              chosen.isEmpty || named.trim.isEmpty || busy
            ),
          child.text <-- sending.signal.map(
            if (_) Tr("Importing…", "Importation…")
            else Tr("Add to my scores", "Ajouter à mes partitions")
          ),
          onClick --> { _ =>
            sending.set(true)
            bus.send(
              ImportScores(
                ScoreImport(
                  title.now().trim,
                  Option(composer.now().trim).filter(_.nonEmpty),
                  files.now()
                )
              )
            )
          }
        )
      ),
      // a new page shown: the import is done, or another one is started
      bus.events.events.collect { case _: PageShown => () } --> (_ => close()),
      failed.changes.collect { case Some(_) => () } --> (_ =>
        sending.set(false)
      )
    )
  )
}
