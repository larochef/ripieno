package ripieno.frontend.utils

import ripieno.frontend.model.*

import scala.util.Try

import org.scalajs.dom

/** The metronome's sound, the tuner's A and instrument, each instrument's
  * sound, each score's levels, and how the player listens, kept by this browser
  * alone: a convenience, so that nothing fails when the browser keeps nothing.
  */
object RememberedSettings {
  private val metronomeKey = "metronome"
  private val referenceKey = "reference-pitch"
  private val listeningKey = "listening"
  private val tunerInstrumentKey = "tuner-instrument"

  private def get(key: String): Option[String] =
    Try(Option(dom.window.localStorage.getItem(key))).toOption.flatten

  private def set(key: String, value: String): Unit =
    Try(dom.window.localStorage.setItem(key, value))

  // "sound volume accent beatsPerMinute"
  def metronome: MetronomeSettings =
    get(metronomeKey)
      .map(_.split(' ').toList)
      .collect { case List(sound, volume, accent, beatsPerMinute) =>
        Try(
          MetronomeSettings(
            MetronomeSound.valueOf(sound),
            volume.toInt,
            accent.toBoolean,
            beatsPerMinute.toInt
          )
        ).toOption
      }
      .flatten
      .getOrElse(MetronomeSettings.default)

  def metronome_=(settings: MetronomeSettings): Unit =
    set(
      metronomeKey,
      s"${settings.sound} ${settings.volume} ${settings.accent} ${settings.beatsPerMinute}"
    )

  def referencePitch: Double =
    get(referenceKey).flatMap(_.toDoubleOption).getOrElse(440.0)

  def referencePitch_=(hertz: Double): Unit =
    set(referenceKey, hertz.toString)

  private def levelsKey(path: String) = s"mix:$path"

  // each part's level, in score order; an earlier "Engine:level" keeps its level
  def levelsOf(path: String, parts: Int): Option[Vector[Int]] =
    get(levelsKey(path))
      .map(_.split(',').toVector.flatMap(_.split(':').last.toIntOption))
      .filter(_.sizeIs == parts)

  def setLevels(path: String, mix: SoundMix): Unit =
    set(levelsKey(path), mix.parts.map(_.level).mkString(","))

  private def soundKey(instrument: Option[String]) =
    s"sound:${instrument.getOrElse("")}"

  /** What an instrument sounds with, in every score. */
  def soundOf(instrument: Option[String]): SoundEngine =
    get(soundKey(instrument))
      .flatMap(name => Try(SoundEngine.valueOf(name)).toOption)
      .getOrElse(SoundEngine.defaultFor(instrument))

  def setSound(instrument: Option[String], engine: SoundEngine): Unit =
    set(soundKey(instrument), engine.toString)

  /** The instrument last chosen in the tuner, for a score that says none. */
  def tunerInstrument: Option[String] =
    get(tunerInstrumentKey).filter(_.nonEmpty)

  def tunerInstrument_=(instrument: Option[String]): Unit =
    set(tunerInstrumentKey, instrument.getOrElse(""))

  def listening: Listening =
    get(listeningKey)
      .flatMap(name => Try(Listening.valueOf(name)).toOption)
      .getOrElse(Listening.Headphones)

  def listening_=(listening: Listening): Unit =
    set(listeningKey, listening.toString)
}
