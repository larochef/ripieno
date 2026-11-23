package ripieno.frontend.i18n

/** The words the application's catalog names its collections and composers
  * with, in English, as the page's language says them. A score's own titles
  * stay as they are written.
  */
object CatalogWords {
  private val frenchKinds = List(
    "String quartets" -> "Quatuors à cordes",
    "Violin concertos" -> "Concertos pour violon",
    "Violin sonatas" -> "Sonates pour violon",
    "Trio sonatas" -> "Sonates en trio"
  )

  /** "String quartets, Haydn" is "Quatuors à cordes, Haydn" in French. */
  def collection(name: String): String =
    Tr(
      name,
      frenchKinds
        .collectFirst {
          case (english, french) if name.startsWith(english) =>
            french + name.drop(english.length)
        }
        .getOrElse(name.replace(", book ", ", livre "))
    )

  def composer(name: String): String =
    if (name == "Traditional") Tr(name, "Traditionnel") else name
}
