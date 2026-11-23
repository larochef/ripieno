package ripieno.frontend.model

import ripieno.frontend.i18n.Tr

/** How the player hears the other parts: on headphones, or on speakers, which
  * the microphone hears too and the review takes out of what it hears.
  */
enum Listening(english: String, french: String) {
  case Headphones extends Listening("Headphones", "Casque")
  case Speakers extends Listening("Speakers", "Haut-parleurs")

  def label: String = Tr(english, french)
}
