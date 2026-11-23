package ripieno.frontend.model

import ripieno.frontend.i18n.Tr

/** What makes the sound of the score: the page's own synthesizer, there at
  * once, or a rendering by one of the server's engines (`renderedBy`, its name
  * there), which takes about as long to make as the music lasts.
  */
enum SoundEngine(
    english: String,
    french: String,
    val renderedBy: Option[String]
) {
  case Sampled
      extends SoundEngine(
        "Sampled, at once",
        "Échantillonné, tout de suite",
        None
      )
  case Salamander
      extends SoundEngine(
        "Grand piano (Salamander), in seconds",
        "Piano à queue (Salamander), en quelques secondes",
        Some("salamander")
      )
  case MidiDdsp
      extends SoundEngine(
        "Modelled instruments (MIDI-DDSP), in minutes",
        "Instruments modélisés (MIDI-DDSP), en quelques minutes",
        Some("midi-ddsp")
      )
  case Violet
      extends SoundEngine(
        "Modelled violins (VIOLET), in minutes",
        "Violons modélisés (VIOLET), en quelques minutes",
        Some("violet")
      )

  def label: String = Tr(english, french)
}

object SoundEngine {

  /** The server's engine that renders a part the way the page samples it. */
  val sampledOnServer = "sampled"

  /** What an instrument sounds with until another is chosen: the violin
    * modelled, the piano recorded, made in seconds and far better than the
    * sampled one; the rest sampled.
    */
  def defaultFor(instrument: Option[String]): SoundEngine = instrument match {
    case Some("violin") => Violet
    case Some("piano")  => Salamander
    case _              => Sampled
  }
}

/** How one part sounds: its instrument (`Library.instrumentOf`), whose engine
  * it takes, and its own level from 0 to 100.
  */
final case class PartSound(
    instrument: Option[String],
    engine: SoundEngine,
    level: Int
)

/** How each part of a score sounds, in score order. */
final case class SoundMix(parts: Vector[PartSound]) {

  /** All of it sampled at full level: the page's synthesizer plays it, there at
    * once, at any tempo.
    */
  def plain: Boolean =
    parts.forall(part =>
      part.engine == SoundEngine.Sampled && part.level == 100
    )

  /** The engine of every part, when they all have the same. */
  def engine: Option[SoundEngine] =
    parts.map(_.engine).distinct match {
      case Vector(one) => Some(one)
      case _           => None
    }

  /** The instruments of the parts, each once, in score order. */
  def instruments: Vector[Option[String]] = parts.map(_.instrument).distinct

  def withEngine(engine: SoundEngine): SoundMix =
    SoundMix(parts.map(_.copy(engine = engine)))

  def withInstrument(
      instrument: Option[String],
      engine: SoundEngine
  ): SoundMix =
    SoundMix(
      parts.map(part =>
        if (part.instrument == instrument) part.copy(engine = engine) else part
      )
    )

  def withLevel(index: Int, level: Int): SoundMix =
    SoundMix(parts.updated(index, parts(index).copy(level = level)))
}
