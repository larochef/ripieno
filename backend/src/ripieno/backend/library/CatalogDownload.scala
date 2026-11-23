package ripieno.backend.library

import java.net.URI
import java.net.http.*
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.security.MessageDigest
import java.util.{HexFormat, Locale}

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.typesafe.scalalogging.Logger
import ox.flow.Flow

/** The catalog's scores that can be downloaded, into `catalog`: the corpus
  * music21 bundles (https://github.com/cuthbertLab/music21, `music21/corpus`),
  * at a pinned commit, each file checked by the hash Git gives it. Each
  * encoding keeps its own licence, some of them non-commercial: see
  * `music21/corpus/license.txt`. MusicXML and single ABC tunes are copied as
  * they are; tune books holding many tunes are split, a file a tune.
  *
  * Other scores may sit in `catalog` beside them: they are the player's, and
  * indexed alike (`CatalogBuilder`).
  */
final class CatalogDownload(client: HttpClient, catalog: Path) {
  import CatalogDownload.*

  private val logger = Logger(getClass)

  // written once every file is there
  private val done = catalog.resolve(s".music21-${Commit.take(12)}")

  def installed: Boolean = Files.exists(done)

  /** Downloads what is not there yet; whether everything is. */
  def fetched(): Boolean =
    installed || {
      logger.info("Downloading the catalog's scores (music21's corpus)")
      val files = listing().filter(entry =>
        entry.`type` == "blob" && destination(entry.path).nonEmpty
      )
      Flow
        .fromIterable(files)
        .mapParUnordered(8)(entry => entry -> content(entry))
        .runForeach((entry, bytes) => written(entry.path, bytes))
      Files.writeString(done, Commit)
      logger.info(s"Catalog: ${files.size} files of music21's corpus")
      true
    }

  private def listing(): List[TreeEntry] = {
    val response = client.send(
      HttpRequest
        .newBuilder(
          URI.create(
            s"https://api.github.com/repos/cuthbertLab/music21/git/trees/$Commit?recursive=1"
          )
        )
        .header("User-Agent", "ripieno")
        .build(),
      HttpResponse.BodyHandlers.ofByteArray()
    )
    if (response.statusCode != 200)
      throw IllegalStateException(
        s"GitHub answered ${response.statusCode} for music21's files"
      )
    val tree = readFromArray[Tree](response.body)
    if (tree.truncated)
      throw IllegalStateException("GitHub listed music21's files in part only")
    tree.tree
  }

  /** A file's content, checked by its Git hash. */
  private def content(entry: TreeEntry): Array[Byte] = {
    val response = client.send(
      HttpRequest
        .newBuilder(
          URI(
            "https",
            "raw.githubusercontent.com",
            s"/cuthbertLab/music21/$Commit/${entry.path}",
            null,
            null
          )
        )
        .header("User-Agent", "ripieno")
        .build(),
      HttpResponse.BodyHandlers.ofByteArray()
    )
    val bytes = response.body
    if (response.statusCode != 200)
      throw IllegalStateException(
        s"GitHub answered ${response.statusCode} for ${entry.path}"
      )
    if (gitHash(bytes) != entry.sha)
      throw IllegalStateException(s"${entry.path} is not the file listed")
    bytes
  }

  private def written(path: String, bytes: Array[Byte]): Unit =
    destination(path).foreach { (target, book) =>
      if (book) split(bytes, target).foreach(writeAtomically.tupled)
      else writeAtomically(target, bytes)
    }

  private def writeAtomically(target: Path, bytes: Array[Byte]): Unit = {
    Files.createDirectories(target.getParent)
    val partial = target.resolveSibling(s".${target.getFileName}.part")
    Files.write(partial, bytes)
    Files.move(partial, target, StandardCopyOption.REPLACE_EXISTING)
  }

  /** Where a file of the corpus goes, and whether it is a book to split (then
    * into the folder named after it).
    */
  private def destination(path: String): Option[(Path, Boolean)] = {
    val inCorpus = path.stripPrefix(s"$Corpus/")
    Copied
      .collectFirst {
        case (source, target, kinds)
            if inCorpus
              .startsWith(s"$source/") && kinds.exists(path.endsWith) =>
          catalog.resolve(target + inCorpus.stripPrefix(source)) -> false
      }
      .orElse(Books.collectFirst {
        case (source, target)
            if inCorpus.startsWith(s"$source/") && path.endsWith(".abc") =>
          val book = inCorpus.stripPrefix(s"$source/").stripSuffix(".abc")
          catalog.resolve(target).resolve(book) -> true
      })
      .filter(_ => path.startsWith(s"$Corpus/"))
  }
}

object CatalogDownload {

  // music21's master on 2026-11-11
  private val Commit = "86a7dc8a5fb3b1996ac0202d6bb9b4d4b9d19197"
  private val Corpus = "music21/corpus"
  private val MusicXml = Seq(".mxl", ".xml", ".musicxml")
  private val Abc = Seq(".abc")

  // corpus folder -> catalog folder, the files of these kinds copied as they
  // are: MusicXML, and ABC tunes one a file
  private val Copied = List(
    ("beethoven", "beethoven", MusicXml),
    ("mozart/k80", "mozart/k80", MusicXml),
    ("mozart/k155", "mozart/k155", MusicXml),
    ("mozart/k156", "mozart/k156", MusicXml),
    ("mozart/k458", "mozart/k458", MusicXml),
    ("haydn", "haydn", MusicXml),
    ("schumann_robert/opus41no1", "schumann/opus41no1", MusicXml),
    ("corelli", "corelli", MusicXml),
    ("ryansMammoth", "ryans-mammoth", Abc)
  )

  // tune books holding many tunes a file, split into a file a tune
  private val Books = List(
    "oneills1850" -> "oneills-1850",
    "airdsAirs" -> "airds-airs"
  )

  final case class TreeEntry(path: String, `type`: String, sha: String)
  final case class Tree(tree: List[TreeEntry], truncated: Boolean)
  given JsonValueCodec[Tree] = JsonCodecMaker.make

  /** Git's hash of a file: SHA-1 of `blob <length>\0` and its bytes. */
  def gitHash(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-1")
    digest.update(
      s"blob ${bytes.length}\u0000".getBytes(StandardCharsets.US_ASCII)
    )
    HexFormat.of().formatHex(digest.digest(bytes))
  }

  /** Each tune of an ABC tune book as a file of its own in `folder`, the book's
    * header kept: `<number>-<title>.abc`.
    */
  def split(book: Array[Byte], folder: Path): List[(Path, Array[Byte])] = {
    val lines =
      new String(book, StandardCharsets.UTF_8).split("\r\n|\r|\n", -1).toList
    // a final line break ends the last line, it does not start another
    val all = if (lines.lastOption.contains("")) lines.dropRight(1) else lines
    val starts = all.indices.filter(all(_).startsWith("X:")).toList
    starts.headOption.toList.flatMap { first =>
      val header = all.take(first).filter(_.strip.nonEmpty)
      starts.zip(starts.drop(1) :+ all.size).map { (start, end) =>
        val tune =
          all.slice(start, end).reverse.dropWhile(_.strip.isEmpty).reverse
        val number = BigInt(
          Some(tune.head.filter(_.isDigit)).filter(_.nonEmpty).getOrElse("0")
        )
        val title = tune
          .collectFirst {
            case line if line.startsWith("T:") => line.drop(2).strip
          }
          .getOrElse("untitled")
        folder.resolve(f"$number%04d-${slug(title)}.abc") ->
          ((header ++ tune).mkString("\n") + "\n")
            .getBytes(StandardCharsets.UTF_8)
      }
    }
  }

  private def slug(text: String): String =
    text
      .toLowerCase(Locale.ROOT)
      .replaceAll("[^a-z0-9]+", "-")
      .replaceAll("^-+|-+$", "")
      .take(60)
}
