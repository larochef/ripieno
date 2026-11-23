package ripieno.backend.scores

import java.io.ByteArrayOutputStream
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

import com.sheetmusic4j.core.io.ScoreFile
import com.sheetmusic4j.core.musicxml.MusicXmlWriter

/** The scores under a folder, each handed out as MusicXML text in UTF-8: that
  * is all the page's engraver is given, whatever the file on disk is.
  */
final class ScoreLibrary(root: Path) {
  import ScoreLibrary.*

  private val libraryRoot = root.toAbsolutePath.normalize

  // Nothing outside the library is reachable, whatever the path asks for.
  private def resolved(path: String): Option[Path] = {
    Some(libraryRoot.resolve(path).normalize).filter(_.startsWith(libraryRoot))
  }

  /** Whether there is a score at that path of the library. */
  def isScore(path: String): Boolean =
    resolved(path).exists(Files.isRegularFile(_))

  def musicXml(path: String): Either[String, String] = {
    resolved(path)
      .filter(Files.isRegularFile(_))
      .toRight(s"No score at $path")
      .flatMap { file =>
        Try(musicXmlOf(file, extensionOf(file))).toEither.left
          .map(error => s"Could not read $path: $error")
      }
  }
}

object ScoreLibrary {

  private val declaration = """^\s*<\?xml[^>]*\?>""".r
  private val declaredEncoding = """encoding\s*=\s*["']([^"']+)["']""".r
  private val rootFile = """full-path\s*=\s*"([^"]+)"""".r

  private def nameOf(file: Path): String = file.getFileName.toString

  private def extensionOf(file: Path): String = {
    val name = nameOf(file)
    name.substring(name.lastIndexOf('.') + 1).toLowerCase
  }

  /** A score file as MusicXML text, read as its extension says. */
  def musicXmlOf(file: Path, extension: String): String =
    extension.toLowerCase match {
      case "mxl"              => decode(unzipped(file))
      case "xml" | "musicxml" => decode(Files.readAllBytes(file))
      case _                  => converted(file)
    }

  /** A score file's content as MusicXML text, read as its extension says. */
  def musicXmlOfBytes(bytes: Array[Byte], extension: String): String = {
    val file = Files.createTempFile("score-", s".${extension.toLowerCase}")
    try {
      Files.write(file, bytes)
      musicXmlOf(file, extension)
    } finally {
      Files.deleteIfExists(file)
    }
  }

  // A compressed MusicXML file is a zip whose container names the score in it.
  private def unzipped(file: Path): Array[Byte] = {
    Using.resource(new ZipFile(file.toFile)) { zip =>
      def read(name: String): Option[Array[Byte]] = {
        Option(zip.getEntry(name)).map(entry =>
          Using.resource(zip.getInputStream(entry))(_.readAllBytes())
        )
      }
      val named = read("META-INF/container.xml")
        .flatMap(container =>
          rootFile.findFirstMatchIn(
            new String(container, StandardCharsets.UTF_8)
          )
        )
        .flatMap(found => read(found.group(1)))
      named
        .orElse(
          zip.entries.asScala
            .map(_.getName)
            .find(name =>
              !name.startsWith("META-INF") && name.toLowerCase
                .matches(""".*\.(xml|musicxml)""")
            )
            .flatMap(read)
        )
        .getOrElse(sys.error("no score in the archive"))
    }
  }

  // The text without its XML declaration, which names an encoding that no longer holds.
  private def decode(bytes: Array[Byte]): String = {
    val utf16 = bytes.length >= 2 && Set((0xff, 0xfe), (0xfe, 0xff)).contains(
      (bytes(0) & 0xff, bytes(1) & 0xff)
    )
    val charset =
      if (utf16) {
        StandardCharsets.UTF_16
      } else {
        val head = new String(bytes.take(200), StandardCharsets.ISO_8859_1)
        declaration
          .findFirstIn(head)
          .flatMap(declaredEncoding.findFirstMatchIn)
          .flatMap(found => Try(Charset.forName(found.group(1))).toOption)
          .getOrElse(StandardCharsets.UTF_8)
      }
    declaration.replaceFirstIn(new String(bytes, charset).stripPrefix("﻿"), "")
  }

  private def converted(file: Path): String = {
    val out = new ByteArrayOutputStream()
    new MusicXmlWriter().write(ScoreFile.load(file), out)
    decode(out.toByteArray)
  }
}
