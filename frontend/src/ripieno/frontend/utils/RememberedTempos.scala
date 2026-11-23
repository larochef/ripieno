package ripieno.frontend.utils

import scala.util.Try

import org.scalajs.dom

/** The tempo last played of each score, kept by this browser alone: a
  * convenience, so that nothing fails when the browser keeps nothing.
  */
object RememberedTempos {

  private def key(path: String): String = s"tempo:$path"

  def get(path: String): Option[Int] =
    Try(Option(dom.window.localStorage.getItem(key(path)))).toOption.flatten
      .flatMap(_.toIntOption)

  private def cutKey(path: String, cut: Int): String = s"cut:$path:$cut"

  /** Whether a cut the score allows was last made. */
  def cut(path: String, cut: Int): Boolean =
    Try(
      Option(dom.window.localStorage.getItem(cutKey(path, cut)))
    ).toOption.flatten
      .contains("made")

  /** The cuts of the score last made. */
  def cutsMade(path: String): Set[Int] =
    Try {
      val prefix = s"cut:$path:"
      val storage = dom.window.localStorage
      (0 until storage.length).toList
        .flatMap(index => Option(storage.key(index)))
        .filter(_.startsWith(prefix))
        .flatMap(key => key.stripPrefix(prefix).toIntOption)
        .filter(cut(path, _))
        .toSet
    }.getOrElse(Set.empty)

  def setCut(path: String, cut: Int, made: Boolean): Unit = {
    Try {
      if (made) dom.window.localStorage.setItem(cutKey(path, cut), "made")
      else dom.window.localStorage.removeItem(cutKey(path, cut))
    }
  }

  private def repeatsKey(path: String): String = s"repeats:$path"

  /** The repeats of a score last left out. */
  def skippedRepeats(path: String): Set[Int] =
    Try(
      Option(dom.window.localStorage.getItem(repeatsKey(path)))
    ).toOption.flatten
      .fold(Set.empty[Int])(_.split(',').flatMap(_.toIntOption).toSet)

  def setRepeat(path: String, repeat: Int, played: Boolean): Unit = {
    val now =
      if (played) skippedRepeats(path) - repeat
      else skippedRepeats(path) + repeat
    Try {
      if (now.isEmpty) dom.window.localStorage.removeItem(repeatsKey(path))
      else
        dom.window.localStorage
          .setItem(repeatsKey(path), now.toList.sorted.mkString(","))
    }
  }

  /** Forgets it when it is the written one. */
  def set(path: String, beatsPerMinute: Int, written: Int): Unit = {
    Try {
      if (beatsPerMinute == written) {
        dom.window.localStorage.removeItem(key(path))
      } else {
        dom.window.localStorage.setItem(key(path), beatsPerMinute.toString)
      }
    }
  }
}
