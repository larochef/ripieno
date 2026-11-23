package ripieno.backend.tempi

import ripieno.shared.{Library, ScoreTempi}

import java.nio.file.*
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

import com.github.plokhotnyuk.jsoniter_scala.core.*

/** The tempi of the library's scores' sections, each score's in a file of
  * `root` at the path the page opens it by (`movements/12.tempi.json`), beside
  * its editions: the scores themselves are never written to.
  */
final class TempoStore(root: Path, exists: String => Boolean) {
  private val storeRoot = root.toAbsolutePath.normalize

  // Only for a score of the library, and never outside the store.
  private def file(path: String): Either[String, Path] =
    Some(storeRoot.resolve(s"$path.tempi.json").normalize)
      .filter(file => exists(path) && file.startsWith(storeRoot))
      .toRight(s"No score at $path")

  def tempi(path: String): Either[String, ScoreTempi] =
    file(path).flatMap { file =>
      if (!Files.exists(file)) Right(ScoreTempi(Nil))
      else
        Try(readFromArray[ScoreTempi](Files.readAllBytes(file))).toEither.left
          .map(error =>
            s"The tempi of $path could not be read: ${error.getMessage}"
          )
    }

  // Written aside, then moved over: a reader never sees half a file.
  def save(path: String, tempi: ScoreTempi): Either[String, ScoreTempi] =
    file(path).map { file =>
      Files.createDirectories(file.getParent)
      val written = file.resolveSibling(s"${file.getFileName}.part")
      Files.write(
        written,
        writeToArray(tempi, WriterConfig.withIndentionStep(2))
      )
      Files.move(
        written,
        file,
        StandardCopyOption.REPLACE_EXISTING,
        StandardCopyOption.ATOMIC_MOVE
      )
      tempi
    }

  /** Deletes the tempi of every path naming one of those movements, alone or
    * among others.
    */
  def forget(movements: Set[Long]): Unit = {
    val folder = storeRoot.resolve("movements")
    if (Files.isDirectory(folder)) {
      Using
        .resource(Files.list(folder))(_.iterator.asScala.toList)
        .filter { file =>
          val name = file.getFileName.toString
          name.endsWith(".tempi.json") && Library
            .movementsOf(s"movements/${name.stripSuffix(".tempi.json")}")
            .exists(_.exists(movements.contains))
        }
        .foreach(Files.delete)
    }
  }
}
