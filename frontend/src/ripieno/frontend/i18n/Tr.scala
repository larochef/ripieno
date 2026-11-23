package ripieno.frontend.i18n

/** A text of the page, in each language it speaks: the one of
  * `Language.current` is kept.
  */
object Tr {
  def apply(english: String, french: String): String =
    Language.current match {
      case Language.English => english
      case Language.French  => french
    }
}
