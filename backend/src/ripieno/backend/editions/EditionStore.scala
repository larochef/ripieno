package ripieno.backend.editions

import ripieno.shared.{Editions, Library}

import java.nio.file.*
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

import com.github.plokhotnyuk.jsoniter_scala.core.*

/** The editions of the library's scores, each score's in a file of `root` at
  * the path the page opens it by (`movements/12`): the scores themselves are
  * never written to.
  */
final class EditionStore(root: Path, exists: String => Boolean) {
  private val storeRoot = root.toAbsolutePath.normalize

  // Only for a score of the library, and never outside the store.
  private def file(path: String): Either[String, Path] =
    Some(storeRoot.resolve(s"$path.editions.json").normalize)
      .filter(file => exists(path) && file.startsWith(storeRoot))
      .toRight(s"No score at $path")

  def editions(path: String): Either[String, Editions] =
    file(path).flatMap { file =>
      if (!Files.exists(file)) Right(Editions(Nil))
      else
        Try(readFromArray[Editions](Files.readAllBytes(file))).toEither.left
          .map(error =>
            s"The editions of $path could not be read: ${error.getMessage}"
          )
    }

  // Written aside, then moved over: a reader never sees half a file.
  def save(path: String, editions: Editions): Either[String, Editions] =
    file(path).map { file =>
      Files.createDirectories(file.getParent)
      val written = file.resolveSibling(s"${file.getFileName}.part")
      Files.write(
        written,
        writeToArray(editions, WriterConfig.withIndentionStep(2))
      )
      Files.move(
        written,
        file,
        StandardCopyOption.REPLACE_EXISTING,
        StandardCopyOption.ATOMIC_MOVE
      )
      editions
    }

  /** Deletes the editions of every path naming one of those movements, alone or
    * among others.
    */
  def forget(movements: Set[Long]): Unit = {
    val folder = storeRoot.resolve("movements")
    if (Files.isDirectory(folder)) {
      Using
        .resource(Files.list(folder))(_.iterator.asScala.toList)
        .filter { file =>
          val name = file.getFileName.toString
          name.endsWith(".editions.json") && Library
            .movementsOf(s"movements/${name.stripSuffix(".editions.json")}")
            .exists(_.exists(movements.contains))
        }
        .foreach(Files.delete)
    }
  }
}
