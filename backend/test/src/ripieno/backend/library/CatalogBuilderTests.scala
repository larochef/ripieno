package ripieno.backend.library

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import utest.*

/** The catalog made of a folder of scores, and a tune book split into tunes.
  * When it was ported, it gave the 4308 works and 3189 tunes of the Python it
  * replaces.
  */
object CatalogBuilderTests extends TestSuite {

  private def musicXml(work: String, movement: String, tempo: String): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 4.0 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd">
       |<score-partwise><work><work-title>$work</work-title></work><movement-title>$movement</movement-title>
       |<identification><creator type="composer">Someone</creator></identification>
       |<part-list><score-part id="P1"><part-name>Violino</part-name></score-part>
       |<score-part id="P2"><part-name>Violino</part-name></score-part></part-list>
       |<part id="P1"><measure number="1"><direction><direction-type><words>$tempo</words></direction-type></direction></measure>
       |<measure number="2"/></part><part id="P2"><measure number="1"/><measure number="2"/></part></score-partwise>
       |""".stripMargin

  private def write(root: Path, path: String, text: String): Unit = {
    val file = root.resolve(path)
    Files.createDirectories(file.getParent)
    Files.writeString(file, text)
  }

  val tests: Tests = Tests {
    test("a work's movements, named from their files and their folder") {
      val root = Files.createTempDirectory("catalog")
      write(
        root,
        "vivaldi/op01/vivaldi-op01n01m02.musicxml",
        musicXml("Sonata", "Op. 1 no. 1", "Allegro")
      )
      write(
        root,
        "vivaldi/op01/vivaldi-op01n01m01.musicxml",
        musicXml("Sonata", "Sonata", "Grave")
      )
      write(
        root,
        "vivaldi/op01/vivaldi-op01n01m01-part.musicxml",
        musicXml("Sonata", "", "")
      )
      write(root, "vivaldi/op01/SOURCE.txt", "An edition\nmore of it\n")
      write(
        root,
        "airds-airs/book1/0001-a-tune.abc",
        "X:1\nT:A tune.\nK:G\nGABc|\n"
      )
      val works = CatalogBuilder(root).works
      assert(
        works.map(_.key) == List(
          "airds-airs/book1/0001-a-tune",
          "vivaldi/op01/vivaldi-op01n01"
        )
      )
      val tune = works.head
      assert(
        tune.title == "A tune",
        tune.composer.contains("Traditional"),
        tune.collection.contains("Aird's Airs, book 1"),
        tune.movements.map(_.title) == List("Whole piece")
      )
      val sonata = works(1)
      assert(
        sonata.title == "Sonata",
        sonata.composer.contains("Antonio Vivaldi"),
        sonata.collection.contains("Trio sonatas, op. 1"),
        sonata.edition.contains("An edition"),
        sonata.parts.map(p => (p.name, p.instrument)) ==
          List(("Violino 1", Some("violin")), ("Violino 2", Some("violin"))),
        sonata.movements.map(m =>
          (m.position, m.title, m.measures, m.xmlParts)
        ) == List(
          (1, "Grave", Some(2), List("P1", "P2")),
          (2, "Allegro", Some(2), List("P1", "P2"))
        )
      )
    }
    test("a tune book, a file a tune, its header kept") {
      val book =
        "%%a book\n\nX:7\nT:The First One\nK:D\nDEF|\n\nX:12\nT:Second: Air!\nK:G\nGAB|\n\n"
      val tunes = CatalogDownload.split(
        book.getBytes(StandardCharsets.UTF_8),
        Path.of("book")
      )
      assert(
        tunes.map(_._1.toString) == List(
          "book/0007-the-first-one.abc",
          "book/0012-second-air.abc"
        ),
        new String(
          tunes(1)._2,
          StandardCharsets.UTF_8
        ) == "%%a book\nX:12\nT:Second: Air!\nK:G\nGAB|\n"
      )
    }
    test("a file's Git hash") {
      // `printf hello | git hash-object --stdin`
      assert(
        CatalogDownload.gitHash("hello".getBytes(StandardCharsets.UTF_8)) ==
          "b6fc4c620b67d95f953a5c1c1230aaab5db5a1b0"
      )
    }
  }
}
