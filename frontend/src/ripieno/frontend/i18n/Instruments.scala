package ripieno.frontend.i18n

/** Instruments as the player names them, from the keys the server gives them:
  * "cello" is Cello in English and Violoncelle in French.
  */
object Instruments {
  private val french = Map(
    "violin" -> "Violon",
    "viola" -> "Alto",
    "cello" -> "Violoncelle",
    "bass" -> "Basse",
    "double bass" -> "Contrebasse",
    "piano" -> "Piano",
    "harpsichord" -> "Clavecin",
    "organ" -> "Orgue",
    "continuo" -> "Continuo",
    "flute" -> "Flûte",
    "recorder" -> "Flûte à bec",
    "oboe" -> "Hautbois",
    "clarinet" -> "Clarinette",
    "bassoon" -> "Basson",
    "horn" -> "Cor",
    "trumpet" -> "Trompette",
    "guitar" -> "Guitare",
    "mandolin" -> "Mandoline",
    "harp" -> "Harpe",
    "voice" -> "Voix"
  )

  /** An instrument's name, capitalized, in the page's language: the key itself
    * when it is not known.
    */
  def name(key: String): String =
    Tr(key.capitalize, french.getOrElse(key, key.capitalize))
}
