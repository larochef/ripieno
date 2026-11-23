package ripieno.backend.library

import ripieno.backend.library.CatalogIndex.*

import java.io.StringReader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import javax.xml.parsers.DocumentBuilderFactory
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal
import scala.util.matching.Regex

import com.typesafe.scalalogging.Logger
import org.w3c.dom.{Element, Node}
import org.xml.sax.InputSource

/** The works of a folder of scores: its files grouped into works, named from
  * what they say and from what is known of their folders. A work is one piece,
  * of one or more movements, each movement one file:
  *   - files named `<prefix>m<NN>` (`vivaldi-op01n01m03`, `bach-bwv1007m02`)
  *     are the movements of `<prefix>`;
  *   - files named `movement<N>` in a folder, or `<N><title>` (Corelli's
  *     `1grave`), are the movements of that folder;
  *   - any other file is a work of its own.
  *
  * ABC tunes are one work each, gathered in a collection named after their
  * book. Names come from the files (MusicXML work and movement titles,
  * creators; ABC `T:` and `C:`), a folder's `SOURCE.txt` gives the source and
  * licence, and the folders known below fill in what the files lack. Only
  * scores that may be shared belong in the catalog: the player's own scans are
  * imported into the library, never put there.
  */
final class CatalogBuilder(root: Path) {
  import CatalogBuilder.*

  private val logger = Logger(getClass)

  def works: List[CatalogWork] = {
    val files = Using
      .resource(Files.walk(root))(_.iterator.asScala.toList)
      .filter(file =>
        Files.isRegularFile(file) &&
          ScoreSuffixes.contains(suffix(file)) &&
          AlternativeReadings
            .findFirstIn(withoutSuffix(relative(file)))
            .isEmpty
      )
      .sortWith(pathOrder)
    grouped(files).toList
      .sortBy(_._1)
      .flatMap((key, members) => work(key, members.sortWith(memberOrder)))
  }

  private def relative(file: Path): String =
    root.relativize(file).iterator.asScala.mkString("/")

  private def work(
      key: String,
      members: List[(Int, Path)]
  ): Option[CatalogWork] = {
    val top =
      if (key.startsWith("omr/")) key.split("/")(1) else key.split("/")(0)
    val (composer, collection, licence) =
      Folders.getOrElse(top, (None, None, None))
    var parts = Option.empty[List[(String, String)]]
    var firstFacts = Option.empty[Facts]
    val movements = members.zipWithIndex.flatMap { case ((_, file), index) =>
      val position = index + 1
      val found =
        try Some(facts(file))
        catch {
          case NonFatal(error) =>
            logger.warn(s"Catalog: ${relative(file)} left out: $error")
            None
        }
      found.map { found =>
        if (firstFacts.isEmpty) firstFacts = Some(found)
        if (parts.isEmpty) parts = Some(found.parts)
        // a movement title is kept when it names the movement, not the work
        // again ("Op. 18 no. 1 mvmnt 1")
        val named = Some(found.movementTitle).filter(title =>
          title.nonEmpty && title != found.workTitle &&
            WorkNamedAgain.findFirstIn(title).isEmpty
        )
        val title = named.orElse(found.tempo).getOrElse("") match {
          case "" if members.size > 1 =>
            val stem = stemOf(file)
            if (NumberedTitle.findPrefixOf(stem).nonEmpty)
              nice(stem.replaceFirst("^\\d+", ""))
            else s"Movement $position"
          case title => title
        }
        CatalogMovement(
          position,
          if (title.isEmpty) "Whole piece" else title,
          relative(file),
          found.measures,
          found.parts.map(_._1)
        )
      }
    }
    firstFacts.filter(_ => movements.nonEmpty).map { first =>
      val workTitle = WorkTitles
        .get(key)
        .orElse(quartetTitle(key))
        .orElse(Some(first.workTitle).filter(_.nonEmpty))
        .getOrElse(nice(key.split("/").last))
      val note = sourceNote(members.head._2.getParent)
      val omr = key.startsWith("omr/")
      val shelf = Collections
        .collectFirst { case (start, name) if key.startsWith(start) => name }
        .orElse(
          collection
            .filter(_ == "String quartets")
            .map(c => s"$c, ${composer.getOrElse("").split(" ").last}")
        )
        .orElse(
          collection.map(c =>
            if (top == "airds-airs") {
              val book = key.split("/").dropRight(1).lastOption.getOrElse("")
              s"$c, ${book.replace("book", "book ")}"
            } else c
          )
        )
      val partNames = parts.getOrElse(Nil)
      val shown =
        numbered(partNames.zipWithIndex.map { case ((_, name), index) =>
          (if (name.isEmpty) s"Part ${index + 1}" else name)
            .replace("\\n", " / ")
            .replace("\n", " / ")
        })
      CatalogWork(
        key = key,
        title = workTitle,
        // the folder's name for the composer, spelt alike everywhere; the
        // file's when the folder has none
        composer = composer.orElse(first.composer),
        arranger = first.arranger,
        collection = shelf,
        licence = licence.orElse(
          Option.when(omr)(
            "public domain (scan read by optical recognition, unreviewed)"
          )
        ),
        edition = note
          .filter(_.nonEmpty)
          .map(_.linesIterator.next())
          .orElse(Some(first.rights).filter(_.nonEmpty)),
        parts = shown
          .zip(partNames)
          .map((name, part) => CatalogPart(name, instrument(part._2))),
        movements = movements
      )
    }
  }

  /** The text of the `SOURCE.txt` nearest a file's folder, up to the root. */
  private def sourceNote(folder: Path): Option[String] =
    Iterator
      .iterate(folder)(_.getParent)
      .takeWhile(candidate => candidate != null && candidate.startsWith(root))
      .map(_.resolve("SOURCE.txt"))
      .find(Files.exists(_))
      .map(note =>
        new String(Files.readAllBytes(note), StandardCharsets.UTF_8).strip
      )

  /** The works' files, by work key, each with its order among its work's. */
  private def grouped(files: List[Path]): Map[String, List[(Int, Path)]] =
    files
      .map { file =>
        val path = relative(file)
        val folder = path.split("/").dropRight(1) match {
          case Array() => "."
          case parts   => parts.mkString("/")
        }
        val stem = stemOf(file)
        val (key, order) =
          if (suffix(file) == ".abc") (withoutSuffix(path), 0)
          else
            stem match {
              case MovementOf(prefix, number) if prefix.nonEmpty =>
                (s"$folder/$prefix", number.toInt)
              case Movement(number) => (folder, number.toInt)
              case _                =>
                NumberedTitle.findPrefixMatchOf(stem) match {
                  case Some(found) => (folder, found.group(1).toInt)
                  case None        => (withoutSuffix(path), 0)
                }
            }
        key -> (order, file)
      }
      .groupMap(_._1)(_._2)

  // Python's order of paths: by their parts, one after the other
  private def pathOrder(a: Path, b: Path): Boolean =
    partsOrder(relative(a).split("/").toList, relative(b).split("/").toList)

  private def memberOrder(a: (Int, Path), b: (Int, Path)): Boolean =
    if (a._1 != b._1) a._1 < b._1 else pathOrder(a._2, b._2)
}

object CatalogBuilder {

  /** What is read of a score's file to name its work and its movement. */
  final case class Facts(
      workTitle: String,
      movementTitle: String,
      composer: Option[String],
      arranger: Option[String],
      rights: String,
      parts: List[(String, String)],
      measures: Option[Int],
      tempo: Option[String]
  )

  private val ScoreSuffixes = Set(".musicxml", ".xml", ".mxl", ".abc")

  // what the files do not say, by top folder: composer, collection, licence
  private val Folders
      : Map[String, (Option[String], Option[String], Option[String])] =
    Map(
      "airds-airs" -> (
        Some("Traditional"),
        Some("Aird's Airs"),
        Some("public domain (music21 corpus)")
      ),
      "oneills-1850" -> (
        Some("Traditional"),
        Some("O'Neill's Music of Ireland (1850)"),
        Some("public domain (music21 corpus)")
      ),
      "ryans-mammoth" -> (
        Some("Traditional"),
        Some("Ryan's Mammoth Collection"),
        Some("public domain (music21 corpus)")
      ),
      "bach" -> (Some("Johann Sebastian Bach"), None, None),
      "beethoven" -> (
        Some("Ludwig van Beethoven"),
        Some("String quartets"),
        Some("music21 corpus")
      ),
      "corelli" -> (Some("Arcangelo Corelli"), None, Some("music21 corpus")),
      "haydn" -> (
        Some("Joseph Haydn"),
        Some("String quartets"),
        Some("music21 corpus")
      ),
      "mozart" -> (
        Some("Wolfgang Amadeus Mozart"),
        Some("String quartets"),
        Some("music21 corpus")
      ),
      "schumann" -> (
        Some("Robert Schumann"),
        Some("String quartets"),
        Some("music21 corpus")
      ),
      "vivaldi" -> (
        Some("Antonio Vivaldi"),
        None,
        Some("CCARH MuseData, personal and academic use")
      ),
      "tchaikovsky" -> (Some("Pyotr Ilyich Tchaikovsky"), None, None),
      "saint-saens" -> (Some("Camille Saint-Saëns"), None, None),
      "pachelbel" -> (Some("Johann Pachelbel"), None, None)
    )

  // collections, by the start of a work's key, in this order
  private val Collections = List(
    "vivaldi/op01" -> "Trio sonatas, op. 1",
    "vivaldi/op02" -> "Violin sonatas, op. 2",
    "vivaldi/op06" -> "Violin concertos, op. 6",
    "omr/vivaldi/op03" -> "L'estro armonico, op. 3",
    "omr/bach" -> "Violin concertos"
  )

  // names a folder's files cannot give, by the work's key
  private val WorkTitles = Map(
    "bach/bwv1007" -> "Cello Suite no. 1 in G major, BWV 1007",
    "tchaikovsky/op35" -> "Violin Concerto in D major, op. 35",
    "omr/bach/bwv1041" -> "Violin Concerto in A minor, BWV 1041",
    "omr/bach/bwv1042" -> "Violin Concerto in E major, BWV 1042",
    "omr/bach/bwv1043" -> "Concerto for two violins in D minor, BWV 1043",
    "corelli/opus3no1" -> "Trio Sonata op. 3 no. 1"
  )

  private val Instruments = List(
    "violoncell" -> "cello",
    "cello" -> "cello",
    "viola" -> "viola",
    "bratsche" -> "viola",
    "violin" -> "violin",
    "violone" -> "bass",
    "violon" -> "violin",
    "guitar" -> "guitar",
    "chitarra" -> "guitar",
    "mandolin" -> "mandolin",
    "vln" -> "violin",
    "piano" -> "piano",
    "pianoforte" -> "piano",
    "cembalo" -> "harpsichord",
    "harpsichord" -> "harpsichord",
    "continuo" -> "continuo",
    "basso" -> "bass",
    "bass" -> "bass",
    "flute" -> "flute",
    "oboe" -> "oboe",
    "organ" -> "organ",
    "voice" -> "voice"
  )

  // a separate part next to the score it is a part of: the score holds it already
  private val AlternativeReadings = "(^|/)parts/|-part$".r

  private val TempoWords =
    ("(?i)\\b(grave|largo|larghetto|lento|adagio|adagietto|andante|andantino|moderato|allegretto|allegro|vivace|vivo|" +
      "presto|prestissimo|menuetto|minuetto|menuet|scherzo|trio|rondo|finale|fuga|fuge|tempo di|alla|siciliano|" +
      "gavotte|gigue|giga|sarabande|courante|allemande|prélude|prelude|polonaise|romance|canzonetta)\\b").r

  private val WorkNamedAgain =
    "(?i)\\b(op\\.?|opus|quartet|quartett|mvmnt|movement|score|k\\.)\\s*\\d*".r
  private val MovementOf = "^(.*?)m(\\d+)$".r
  private val Movement = "^movement(\\d+)$".r
  private val NumberedTitle = "^(\\d+)[a-z]".r

  def instrument(name: String): Option[String] = {
    val lowered = name.toLowerCase
    Instruments.collectFirst {
      case (word, kind) if lowered.contains(word) => kind
    }
  }

  /** Names given to several parts, numbered: Violin, Violin is Violin 1, Violin 2.
    */
  def numbered(names: List[String]): List[String] = {
    val counts = names.groupMapReduce(identity)(_ => 1)(_ + _)
    names
      .foldLeft((List.empty[String], Map.empty[String, Int])) {
        case ((out, seen), name) =>
          val n = seen.getOrElse(name, 0) + 1
          (
            (if (counts(name) > 1) s"$name $n" else name) :: out,
            seen.updated(name, n)
          )
      }
      ._1
      .reverse
  }

  /** `'the-lads-of-the-village'` → `'The lads of the village'`; numbers before
    * a dash are dropped.
    */
  def nice(stem: String): String = {
    val bare = stem.replaceFirst("^\\d+[-_ ]*", "")
    val words = bare
      .replace("-", " ")
      .replace("_", " ")
      .replaceAll("(?<=[a-z])(?=[A-Z])", " ")
      .strip
    if (words.isEmpty) bare else words.take(1).toUpperCase + words.drop(1)
  }

  /** `beethoven/opus18no1` → `String Quartet op. 18 no. 1`; `mozart/k155` →
    * `String Quartet K. 155`.
    */
  def quartetTitle(key: String): Option[String] = {
    val name = key.split("/").last
    val opus = "opus(\\d+)(?:no(\\d+))?".r
    val kochel = "k(\\d+)".r
    name match {
      case opus("133", _)       => Some("Große Fuge, op. 133")
      case opus(number, null)   => Some(s"String Quartet op. $number")
      case opus(number, within) =>
        Some(s"String Quartet op. $number no. $within")
      case kochel(number) => Some(s"String Quartet K. $number")
      case _              => None
    }
  }

  private def suffix(file: Path): String = {
    val name = file.getFileName.toString
    val dot = name.lastIndexOf('.')
    if (dot <= 0) "" else name.substring(dot)
  }

  private def stemOf(file: Path): String = {
    val name = file.getFileName.toString
    name.stripSuffix(suffix(file))
  }

  private def withoutSuffix(path: String): String = {
    val name = path.split("/").last
    val dot = name.lastIndexOf('.')
    if (dot <= 0) path else path.dropRight(name.length - dot)
  }

  private def partsOrder(a: List[String], b: List[String]): Boolean =
    (a, b) match {
      case (x :: xs, y :: ys) => if (x == y) partsOrder(xs, ys) else x < y
      case (Nil, _ :: _)      => true
      case _                  => false
    }

  /** What a file says of its work and movement. */
  def facts(file: Path): Facts =
    if (suffix(file) == ".abc") abcFacts(file) else musicXmlFacts(file)

  private def decoded(raw: Array[Byte]): String =
    if (
      raw.length >= 2 && ((raw(0) == 0xff.toByte && raw(1) == 0xfe.toByte) ||
        (raw(0) == 0xfe.toByte && raw(1) == 0xff.toByte))
    ) new String(raw, StandardCharsets.UTF_16)
    else new String(raw, StandardCharsets.UTF_8)

  private def abcFacts(file: Path): Facts = {
    val text = decoded(Files.readAllBytes(file))
    def field(letter: String): Option[String] =
      s"(?m)^$letter:\\s*(.+)$$".r.findFirstMatchIn(text).map(_.group(1).strip)
    val title =
      field("T")
        .getOrElse(nice(stemOf(file)))
        .reverse
        .dropWhile(_ == '.')
        .reverse
    Facts(title, "", field("C"), None, "", List("P1" -> "Melody"), None, None)
  }

  private def musicXmlText(file: Path): String =
    if (suffix(file) == ".mxl")
      Using.resource(new ZipFile(file.toFile)) { archive =>
        def read(name: String) =
          Using.resource(archive.getInputStream(archive.getEntry(name)))(
            _.readAllBytes()
          )
        val container = new String(
          read("META-INF/container.xml"),
          StandardCharsets.UTF_8
        )
        val name = "full-path\\s*=\\s*\"([^\"]+)\"".r
          .findFirstMatchIn(container)
          .map(_.group(1))
          .getOrElse(
            archive.entries.asScala
              .map(_.getName)
              .find(n => n.endsWith(".xml") && !n.contains("META"))
              .get
          )
        decoded(read(name))
      }
    else decoded(Files.readAllBytes(file))

  private def musicXmlFacts(file: Path): Facts = {
    val text = musicXmlText(file)
      .replaceFirst("^\\s*<\\?xml[^>]*\\?>", "")
      .replaceAll("<!DOCTYPE[^>]*>", "")
    val factory = DocumentBuilderFactory.newInstance()
    factory.setFeature(
      "http://apache.org/xml/features/nonvalidating/load-external-dtd",
      false
    )
    val root = factory
      .newDocumentBuilder()
      .parse(new InputSource(new StringReader(text)))
      .getDocumentElement
    val creators = children(root, "identification")
      .flatMap(children(_, "creator"))
      .map(c => c.getAttribute("type") -> textOf(c).strip)
      .toMap
    val parts = children(root, "part-list")
      .flatMap(children(_, "score-part"))
      .map(p =>
        p.getAttribute("id") -> children(p, "part-name").headOption
          .fold("")(textOf(_).strip)
      )
    val first = children(root, "part").headOption
    val measures = first.map(children(_, "measure"))
    // the tempo word the music opens with: its first measures' words
    val opening = measures
      .getOrElse(Nil)
      .take(2)
      .flatMap(descendants(_, "words"))
      .map(textOf(_).strip)
    def findText(path: String*): String =
      path
        .foldLeft(List(root))((at, name) => at.flatMap(children(_, name)))
        .headOption
        .fold("")(textOf(_).strip)
    Facts(
      workTitle = findText("work", "work-title"),
      movementTitle = findText("movement-title"),
      composer = creators.get("composer"),
      arranger = creators.get("arranger"),
      rights = findText("identification", "rights"),
      parts = parts,
      measures = Some(measures.fold(0)(_.size)),
      tempo =
        opening.find(w => TempoWords.findFirstIn(w).nonEmpty && w.length < 40)
    )
  }

  private def children(element: Element, name: String): List[Element] = {
    val nodes = element.getChildNodes
    (0 until nodes.getLength).toList.map(nodes.item).collect {
      case child: Element if child.getTagName == name => child
    }
  }

  private def descendants(element: Element, name: String): List[Element] = {
    val nodes = element.getElementsByTagName(name)
    (0 until nodes.getLength).toList.map(nodes.item).collect {
      case e: Element => e
    }
  }

  // an element's own text, before its first child element (ElementTree's `text`)
  private def textOf(element: Element): String = {
    val nodes = element.getChildNodes
    (0 until nodes.getLength)
      .map(nodes.item)
      .takeWhile(_.getNodeType != Node.ELEMENT_NODE)
      .collect {
        case node
            if node.getNodeType == Node.TEXT_NODE || node.getNodeType == Node.CDATA_SECTION_NODE =>
          node.getNodeValue
      }
      .mkString
  }
}
