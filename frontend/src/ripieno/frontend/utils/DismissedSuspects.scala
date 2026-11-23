package ripieno.frontend.utils

import scala.util.Try

import org.scalajs.dom

/** The suspect measures the player said are right, for each score, kept by this
  * browser alone: a convenience, so that nothing fails when the browser keeps
  * nothing.
  */
object DismissedSuspects {

  private def key(path: String): String = s"checked:$path"

  def of(path: String): Set[String] =
    Try(Option(dom.window.localStorage.getItem(key(path)))).toOption.flatten
      .fold(Set.empty[String])(_.split('\n').toSet.filter(_.nonEmpty))

  def add(path: String, suspect: String): Unit =
    Try(
      dom.window.localStorage
        .setItem(key(path), (of(path) + suspect).mkString("\n"))
    )
}
