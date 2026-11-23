package ripieno.backend.sound.salamander

import ripieno.backend.sound.*

import java.nio.file.{Files, Path}
import scala.collection.mutable

/** A score's piano parts (General MIDI programs 0 to 7) played on the
  * Salamander Grand Piano's samples, at 48 kHz in stereo, its other parts left
  * for another engine.
  *
  * A note takes the recording of its velocity layer nearest in pitch, shifted
  * by up to a semitone and a half. It sounds until its key is released, or
  * until the sustain pedal is lifted after that, then its damper stops it. A
  * key struck again stops what it was still sounding.
  */
object SalamanderPiano {

  val rate = 48000

  // the recorded keys, a minor third apart from A0 (21) to C8 (108)
  private val centres = (21 until 106 by 3).toVector :+ 108
  private val names =
    Vector("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  // the highest velocity of each of the sixteen layers (Data/notes.txt)
  private val layerTops =
    Vector(26, 34, 36, 43, 46, 50, 56, 64, 72, 80, 88, 96, 104, 112, 120, 127)
  // how much velocity changes the level inside a layer, as the instrument's
  // amp_veltrack does
  private val velocityTrack = 0.73
  // above E6 a piano has no dampers: its strings ring until they die out
  private val lastDamped = 88
  // the damper's fade, longer in the bass where the strings carry more
  private val dampingLow = 0.45
  private val dampingHigh = 0.12
  // a key struck again: what it sounded fades this fast
  private val restrike = 0.03
  // seconds left after the last event for the last notes to ring
  private val ringing = 6
  private val sustainPedal = 64
  // the largest denominator of a shift's resampling ratio
  private val ratioDenominator = 400

  // Parts a score gives no program play as program 0: their names tell them
  // from a piano.
  private val notKeyboards = Vector(
    "viol",
    "cello",
    "basso",
    "bass",
    "contrab",
    "flaut",
    "flute",
    "oboe",
    "clarin",
    "fagott",
    "bassoon",
    "corn",
    "horn",
    "tromb",
    "trump",
    "tromba",
    "voice",
    "voce",
    "soprano",
    "alto",
    "tenor"
  )

  /** The piano parts' sound, how many notes it played, and the other parts. */
  final case class Played(
      sound: Vector[Array[Float]],
      notes: Int,
      others: Vector[MidiInstrument]
  )

  def isPiano(instrument: MidiInstrument): Boolean = {
    val name = instrument.name.toLowerCase
    !instrument.isDrum && instrument.program < 8 && !notKeyboards.exists(
      name.contains
    )
  }

  /** The score played with the samples of the folder `samples`. */
  def played(midi: MidiFile, samples: Path): Played = {
    val pianos = midi.instruments.filter(instrument =>
      isPiano(instrument) && instrument.notes.nonEmpty
    )
    val others = midi.instruments.filter(instrument =>
      !isPiano(instrument) && instrument.notes.nonEmpty
    )
    val recordings = Recordings(samples)
    val length = ((midi.endTime + ringing) * rate).toInt
    val out = Vector.fill(2)(new Array[Float](length))
    var count = 0
    for (instrument <- pianos) {
      val spans = pedalDown(instrument)
      val notes = instrument.notes.sortBy(_.start)
      // the next time each key is struck, to stop the one before it
      val nextStrike = new Array[Double](notes.size)
      val struck = mutable.Map.empty[Int, Double]
      for (index <- notes.indices.reverse) {
        val note = notes(index)
        nextStrike(index) =
          struck.getOrElse(note.pitch, Double.PositiveInfinity)
        struck(note.pitch) = note.start
      }
      for ((note, index) <- notes.zipWithIndex) {
        val pitch = math.min(108, math.max(21, note.pitch))
        val centre = centres.minBy(centre => math.abs(centre - pitch))
        val layer = layerTops.indexWhere(math.max(1, note.velocity) <= _) + 1
        val sound = recordings.shifted(centre, layer, pitch - centre)
        val top = layerTops(layer - 1) / 127.0
        val gain = 1 - velocityTrack + velocityTrack * math.pow(
          note.velocity / 127.0 / top,
          2
        )
        val start = (note.start * rate).toInt
        // where it stops: its damper, or the key struck again
        val (stop, fade) =
          if (pitch > lastDamped) (nextStrike(index), restrike)
          else {
            val damper = released(note.end, spans)
            val damping =
              dampingLow + (dampingHigh - dampingLow) * (pitch - 21) / (lastDamped - 21)
            (
              math.min(damper, nextStrike(index)),
              if (nextStrike(index) < damper) restrike else damping
            )
          }
        val heard = {
          val whole = math.min(sound.head.length, length - start)
          if (stop.isInfinite) whole
          else math.min(whole, ((stop - note.start + fade) * rate).toInt)
        }
        if (heard > 0) {
          val fadeFrom =
            if (stop.isInfinite) heard else ((stop - note.start) * rate).toInt
          for ((channel, rank) <- sound.zipWithIndex) {
            val target = out(rank)
            for (sample <- 0 until heard) {
              var voice = channel(sample) * gain.toFloat
              if (sample >= fadeFrom) {
                val t = (sample - fadeFrom) / (fade * rate)
                voice = (voice * (math.cos(
                  math.Pi * math.min(t, 1)
                ) * 0.5 + 0.5)).toFloat
              }
              target(start + sample) += voice
            }
          }
          count += 1
        }
      }
    }
    // the end of the sound, not the seconds left for its last notes to ring
    val end = (length - 1 to 0 by -1)
      .find(index => out.exists(channel => math.abs(channel(index)) > 1e-4))
      .fold(0)(_ + 1)
    Played(out.map(_.take(end)), count, others)
  }

  /** The spans the sustain pedal is down: (down, up). */
  private def pedalDown(
      instrument: MidiInstrument
  ): Vector[(Double, Double)] = {
    val (spans, down) = instrument.controls
      .sortBy(_.time)
      .filter(_.number == sustainPedal)
      .foldLeft((Vector.empty[(Double, Double)], Option.empty[Double])) {
        case ((found, None), change) if change.value >= 64 =>
          (found, Some(change.time))
        case ((found, Some(from)), change) if change.value < 64 =>
          (found :+ (from -> change.time), None)
        case (state, _) => state
      }
    spans ++ down.map(_ -> Double.PositiveInfinity)
  }

  /** When the damper falls on a note whose key is let go at `end`. */
  private def released(end: Double, spans: Vector[(Double, Double)]): Double =
    spans
      .collectFirst { case (down, up) if down <= end && end < up => up }
      .getOrElse(end)

  // the samples read, and shifted, once each for a rendering
  final private class Recordings(folder: Path) {
    private val read = mutable.Map.empty[(Int, Int), Vector[Array[Float]]]
    private val shiftedOnes =
      mutable.Map.empty[(Int, Int, Int), Vector[Array[Float]]]

    private def recording(centre: Int, layer: Int): Vector[Array[Float]] =
      read.getOrElseUpdate(
        (centre, layer), {
          val name = s"${names(centre % 12)}${centre / 12 - 1}v$layer.flac"
          val flac = Flac.decode(Files.readAllBytes(folder.resolve(name)))
          require(flac.rate == rate, s"$name is at ${flac.rate} Hz")
          flac.floats
        }
      )

    /** The recording played `semitones` higher: resampled by a ratio close to
      * 2^(s/12).
      */
    def shifted(centre: Int, layer: Int, semitones: Int): Vector[Array[Float]] =
      shiftedOnes.getOrElseUpdate(
        (centre, layer, semitones),
        if (semitones == 0) recording(centre, layer)
        else {
          val (up, down) =
            Resampling.fraction(
              math.pow(2, -semitones / 12.0),
              ratioDenominator
            )
          recording(centre, layer).map(Resampling.resampled(_, up, down))
        }
      )
  }
}
