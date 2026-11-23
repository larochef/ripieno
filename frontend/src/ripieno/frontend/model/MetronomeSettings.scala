package ripieno.frontend.model

import ripieno.frontend.i18n.Tr

/** How the metronome sounds. */
enum MetronomeSound(english: String, french: String) {
  case Click extends MetronomeSound("Click", "Clic")
  case Woodblock extends MetronomeSound("Wood block", "Wood-block")
  case Clave extends MetronomeSound("Clave", "Claves")
  case Beep extends MetronomeSound("Beep", "Bip")
  case Soft extends MetronomeSound("Soft", "Doux")

  def label: String = Tr(english, french)
}

/** The metronome's sound, its volume from 0 to 100, whether the first beat of
  * each measure stands out, and its tempo when no score is open (a score's is
  * the one chosen for it).
  */
final case class MetronomeSettings(
    sound: MetronomeSound,
    volume: Int,
    accent: Boolean,
    beatsPerMinute: Int
)

object MetronomeSettings {
  val default: MetronomeSettings =
    MetronomeSettings(MetronomeSound.Click, 70, accent = true, 60)
}
