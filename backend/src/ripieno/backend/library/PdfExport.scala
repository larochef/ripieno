package ripieno.backend.library

import ripieno.shared.ExportPages

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/** Pages engraved by the page, as SVG, made into one PDF: each converted by
  * `rsvg-convert`, then joined by `pdfunite` (poppler). The page engraves, so
  * that what is exported looks as what is shown.
  */
final class PdfExport(work: Path) {
  private val longest = 120L

  def pdf(pages: ExportPages): Either[String, Array[Byte]] =
    if (pages.pages.isEmpty) Left("Nothing to export")
    else {
      Files.createDirectories(work)
      val folder = Files.createTempDirectory(work, "export-")
      try {
        val pdfs = pages.pages.zipWithIndex.map { (svg, index) =>
          val source = folder.resolve(f"page-$index%03d.svg")
          Files.writeString(source, svg)
          val target = folder.resolve(f"page-$index%03d.pdf")
          // on A4 paper, as the page engraved it
          run(
            List(
              "rsvg-convert",
              "-f",
              "pdf",
              "--page-width=210mm",
              "--page-height=297mm",
              "--width=210mm",
              "--height=297mm",
              "-o",
              target.toString,
              source.toString
            )
          )
            .map(_ => target)
        }
        pdfs.collectFirst { case Left(reason) => reason } match {
          case Some(reason) => Left(reason)
          case None         =>
            val joined = folder.resolve("export.pdf")
            val parts = pdfs.collect { case Right(file) => file.toString }
            val done =
              if (parts.sizeIs == 1) {
                Files.copy(Path.of(parts.head), joined)
                Right(())
              } else run(("pdfunite" :: parts) :+ joined.toString)
            done.map(_ => Files.readAllBytes(joined))
        }
      } finally {
        Files.list(folder).iterator().asScala.foreach(Files.deleteIfExists)
        Files.deleteIfExists(folder)
      }
    }

  private def run(command: List[String]): Either[String, Unit] = {
    val process =
      new ProcessBuilder(command.asJava).redirectErrorStream(true).start()
    if (!process.waitFor(longest, TimeUnit.SECONDS)) {
      process.destroyForcibly()
      Left(s"${command.head} took too long")
    } else if (process.exitValue() != 0) {
      val said = new String(process.getInputStream.readAllBytes()).trim
      Left(s"${command.head} failed: ${said.take(300)}")
    } else Right(())
  }
}
