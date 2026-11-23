package ripieno.frontend.appServices

import ripieno.frontend.Libraries.VerovioToolkit
import ripieno.frontend.api.ApiClient
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.MusicXml
import ripieno.frontend.utils.AppStreams
import ripieno.shared.*

import scala.scalajs.js
import scala.scalajs.js.typedarray.Int8Array
import scala.util.Try

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Makes a PDF of movements of a work: each engraved here, as on the screen, on
  * A4 pages, with its editions if asked; the server joins the pages into one
  * PDF, which the browser then saves.
  */
class ExportService(bus: AppBus, apiClient: ApiClient, verovio: js.Any) {
  private lazy val toolkit = {
    val made = new VerovioToolkit(verovio).asInstanceOf[js.Dynamic]
    made.setOptions(
      js.Dynamic.literal(
        inputFrom = "musicxml",
        scale = 40,
        breaks = "auto",
        adjustPageHeight = false,
        svgViewBox = true,
        header = "auto",
        // the page numbers, not the engraver's own mark
        footer = "none"
      )
    )
    made
  }

  // An A4 page `enlarged` times as large as the music: printed on A4, the
  // music is that many times smaller. Margins stay a centimetre.
  private def pageSized(enlarged: Double): Unit = {
    def scaled(length: Int) = math.round(length * enlarged).toInt
    toolkit.setOptions(
      js.Dynamic.literal(
        pageWidth = scaled(2100),
        pageHeight = scaled(2970),
        pageMarginTop = scaled(100),
        pageMarginBottom = scaled(100),
        pageMarginLeft = scaled(100),
        pageMarginRight = scaled(100)
      )
    )
  }

  private def pageCount: Int = toolkit.getPageCount().asInstanceOf[Int]

  bus.commands.events.foreach {
    case command: ExportPdf =>
      bus.publish(ExportProgress(true))
      engrave(command, command.movements, Nil)
    case _ => ()
  }(using unsafeWindowOwner)

  // One movement after the other: its score, its editions, its pages.
  private def engrave(
      command: ExportPdf,
      left: List[Long],
      pages: List[String]
  ): Unit = left match {
    case Nil              => send(command, pages)
    case movement :: rest =>
      val path = Library.movementsPath(List(movement))
      // its editions played over it by the server, if asked for
      val editions =
        if (!command.edited) EventStream.fromValue(Editions(Nil))
        else apiClient.editions(path).map(_.getOrElse(Editions(Nil)))
      val read = editions.flatMapSwitch(found =>
        apiClient.prepareScore(ScoreRequest(path, found.editions, Nil, Nil))
      )
      AppStreams.runOnce(read) {
        case Left(reason)    => failed(command, reason)
        case Right(prepared) =>
          Try(movementPages(command, prepared)).toEither match {
            case Left(error)     => failed(command, error.getMessage)
            case Right(engraved) => engrave(command, rest, pages ++ engraved)
          }
      }
  }

  private def movementPages(
      command: ExportPdf,
      prepared: PreparedScore
  ): List[String] = {
    val closed = prepared.musicXml
    def part(index: Int) = prepared.reading.parts.lift(index)
    val laidOut = command.layout match {
      case ExportLayout.Full         => closed
      case ExportLayout.Alone(index) =>
        part(index).fold(closed)(MusicXml.only(closed, _))
      case ExportLayout.Accompanying(index) =>
        part(index).fold(closed)(
          MusicXml.withOthersSmaller(closed, _, ExportService.otherPartsPercent)
        )
    }
    pageSized(1.0)
    if (!js.DynamicImplicits.truthValue(toolkit.loadData(laidOut))) {
      throw IllegalStateException(
        Tr(
          "the engraver could not read a movement",
          "le graveur n'a pas pu lire un mouvement"
        )
      )
    }
    if (command.compact) {
      // the least the music is made smaller for the fewest pages
      val tried = ExportService.enlargements.map { enlarged =>
        pageSized(enlarged)
        toolkit.redoLayout()
        enlarged -> pageCount
      }
      val fewest = tried.map(_._2).min
      tried.find(_._2 == fewest).foreach { (enlarged, _) =>
        pageSized(enlarged)
        toolkit.redoLayout()
      }
    }
    (1 to pageCount).toList.map(page =>
      toolkit.renderToSVG(page).asInstanceOf[String]
    )
  }

  private def send(command: ExportPdf, pages: List[String]): Unit = {
    def partName(index: Int) = command.work.parts.lift(index).map(_.name)
    val named = command.layout match {
      case ExportLayout.Full                => None
      case ExportLayout.Alone(index)        => partName(index)
      case ExportLayout.Accompanying(index) =>
        partName(index).map(found =>
          Tr(s"$found (accompanist)", s"$found (accompagnement)")
        )
    }
    val name = (command.work.title :: named.toList).mkString(" - ")
    AppStreams.runOnce(apiClient.exportPdf(ExportPages(name, pages))) {
      case Left(reason) => failed(command, reason)
      case Right(bytes) =>
        bus.publish(ExportProgress(false))
        save(s"$name.pdf", bytes)
    }
  }

  // Hands the file to the browser, which saves it where it saves downloads.
  private def save(name: String, bytes: Array[Byte]): Unit = {
    val array = new Int8Array(bytes.length)
    bytes.indices.foreach(index => array(index) = bytes(index))
    val blob = new dom.Blob(
      js.Array(array.buffer),
      new dom.BlobPropertyBag { `type` = "application/pdf" }
    )
    val url = dom.URL.createObjectURL(blob)
    val anchor =
      dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
    anchor.href = url
    anchor.setAttribute("download", name.replaceAll("[/\\\\:*?\"<>|]", "-"))
    dom.document.body.appendChild(anchor)
    anchor.click()
    anchor.remove()
    dom.window.setTimeout(() => dom.URL.revokeObjectURL(url), 10000)
  }

  private def failed(command: ExportPdf, reason: String): Unit = {
    bus.publish(ExportProgress(false))
    bus.publish(
      RequestFailed(command, Tr(s"No PDF: $reason", s"Pas de PDF : $reason"))
    )
  }
}

object ExportService {
  // the size of the parts an accompanist reads over the part played, as cue
  // staves are printed
  val otherPartsPercent = 70
  // how much larger than the music a compact page may be: the music down to
  // 91 % of its size, by steps, to save a page
  val enlargements: List[Double] = List(1.0, 1.02, 1.04, 1.06, 1.08, 1.1)
}
