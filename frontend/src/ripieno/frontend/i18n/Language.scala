package ripieno.frontend.i18n

import scala.util.Try

import org.scalajs.dom

/** A language the page speaks. */
enum Language(val code: String, val name: String) {
  case English extends Language("en", "English")
  case French extends Language("fr", "Français")
}

object Language {
  private val key = "language"

  /** The page's language, chosen once as it loads: the player's choice, else
    * the browser's. Every text is built once, so another choice loads the page
    * again.
    */
  lazy val current: Language = {
    val chosen =
      Try(Option(dom.window.localStorage.getItem(key))).toOption.flatten
    val wanted = chosen.getOrElse(dom.window.navigator.language)
    Language.values
      .find(language => wanted.startsWith(language.code))
      .getOrElse(English)
  }

  def choose(language: Language): Unit = {
    Try(dom.window.localStorage.setItem(key, language.code))
    dom.window.location.reload()
  }
}
