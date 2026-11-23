package ripieno.backend.review

import ripieno.backend.review.ReviewFeatures.*
import ripieno.shared.{Accompaniment, ReferenceNote}

/** What is heard in a recording of a part, for its review (`ReviewFeatures`):
  * the pitch the tracker hears (`PitchTracking`), the level, the attacks, and
  * how present each pitch is (`salience`), the accompaniment taken out first
  * when the microphone heard it on speakers.
  */
object RecordingFeatures {

  private val frameLength = 2048
  private val pitchResolution = 0.1
  // a sample this close to full scale is a clipped one
  private val clippedAt = 0.98f
  // semitones above a note of its first five harmonics, and their weights
  private val harmonics = Vector(0, 12, 19, 24, 28)
  private val harmonicWeights = Vector(1.0, 0.8, 0.6, 0.5, 0.4)
  // how much a pitch's salience is compressed (log(1 + c x)) against the strongest
  private val salienceCompression = 30
  // an accompaniment heard through speakers: 5.4 Hz bins, a semitone apart
  // down to the violin's lowest notes' partials; the partials of each of its
  // notes taken out, this many cents around each and this many more a
  // partial, a piano's running sharp (f_k = k f0 sqrt(1 + B k²)), what is
  // left of their bins; how long a note and the room's echo go on after it
  private val accompanimentFft = 4096
  private val accompanimentPartials = 10
  private val accompanimentCents = 30
  private val accompanimentCentsPerPartial = 5
  private val accompanimentInharmonicity = 0.0004
  private val accompanimentLeft = 0.03f
  private val accompanimentEcho = 0.4
  // seconds the accompaniment may reach the microphone after the recording
  // starts, or before (the page knows where the score stood to a tenth of a
  // second); seconds around where a note of the part is written that keep its
  // partials
  private val accompanimentLongestDelay = 0.4
  private val accompanimentEarliest = 0.15
  private val accompanimentPartReach = 0.3

  /** A MIDI pitch's frequency. */
  def hz(pitch: Double): Double = 440 * math.pow(2, (pitch - 69) / 12)

  /** A frequency's MIDI pitch. */
  def midi(frequency: Double): Double = 12 * (log2(frequency) - log2(440)) + 69

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  def of(
      recording: Array[Float],
      notes: Vector[ReferenceNote],
      accompaniment: Option[Accompaniment]
  ): ReviewFeatures = {
    val pitches = notes.flatMap(_.pitches)
    val (lowest, highest) = (pitches.min - 7, pitches.max + 7)
    // on headphones, the microphone does not hear the accompaniment
    val audio = accompaniment
      .filter(_.onSpeakers)
      .fold(recording)(withoutAccompaniment(recording, _, notes))
    val (f0, voiced) = PitchTracking.pyin(
      audio,
      hz(lowest),
      hz(highest),
      Rate,
      frameLength,
      Hop,
      pitchResolution
    )
    val level = Spectra.amplitudeToDb(Spectra.rms(audio, frameLength, Hop))
    val onsets = Spectra.onsetStrength(audio, Rate, frameLength, Hop)
    val length = Seq(f0.length, level.length, onsets.length).min
    val (heardLevel, onsetStrength) = (level.take(length), onsets.take(length))
    val loudest = heardLevel.max
    // a pitch heard in what is nearly silence is a resonance dying away, or noise
    val heard = Array.tabulate(length)(t =>
      if (!voiced(t) || heardLevel(t) < loudest - SilentBelowPeakDb) Double.NaN
      else midi(f0(t))
    )
    val (values, linear) = salience(audio, lowest, highest, length, heardLevel)
    ReviewFeatures(
      heard,
      values,
      linear,
      lowest,
      heardLevel,
      onsetStrength,
      audio.length.toDouble / Rate,
      clippedShare(audio)
    )
  }

  /** The share of the recording's seconds in which the sound reaches the top of
    * its range: a saturated sound has harmonics of its own, which the pitch is
    * no longer heard right through.
    */
  private def clippedShare(audio: Array[Float]): Double = {
    val seconds = math.max(1, audio.length / Rate)
    (0 until seconds).count { second =>
      (second * Rate until math.min(audio.length, (second + 1) * Rate)).exists(
        i => math.abs(audio(i)) >= clippedAt
      )
    }.toDouble / seconds
  }

  /** How present each pitch from `lowest` to `highest` is in each frame, from 0
    * to 1, compressed and not: the sum of its first harmonics in a constant-Q
    * spectrum (three bins a semitone, the best of the three), each frame scaled
    * to its most salient pitch. Unlike the one pitch the tracker settles on,
    * this says that a note sounds even while an open string rings beside it.
    */
  private def salience(
      audio: Array[Float],
      lowest: Int,
      highest: Int,
      length: Int,
      level: Array[Float]
  ): (Array[Array[Double]], Array[Array[Double]]) = {
    val bins = 3
    // room for the fifth harmonic
    val top = highest + 28
    val semitones = top - lowest + 1
    val spectrum = ConstantQ.magnitudes(
      audio,
      Rate,
      Hop,
      hz(lowest - 0.5),
      semitones * bins,
      12 * bins
    )
    val frames = math.min(length, spectrum.head.length)
    val bySemitone = Array.tabulate(semitones, frames)((s, t) =>
      (0 until bins).map(b => spectrum(s * bins + b)(t)).max
    )
    val pitches = highest - lowest + 1
    val loudest = level.max
    val linear = Array.tabulate(frames) { t =>
      val total = Array.tabulate(pitches) { p =>
        harmonics.indices
          .map { h =>
            val index = math.max(0, math.min(semitones - 1, p + harmonics(h)))
            // each product in floats, as NumPy makes it, the sum in doubles
            (harmonicWeights(h).toFloat * bySemitone(index)(t)).toDouble
          }
          .foldLeft(0.0)(_ + _)
      }
      val strongest = math.max(total.max, 1e-9)
      if (level(t) < loudest - SilentBelowPeakDb) Array.fill(pitches)(0.0)
      else total.map(_ / strongest)
    }
    // compressed for following the score, so that a note sounding over a
    // louder open string still counts
    val compressed = linear.map(
      _.map(x =>
        math.log1p(salienceCompression * x) / math.log1p(salienceCompression)
      )
    )
    (compressed, linear)
  }

  /** The bins of the spectrum near the first `partials` partials of `pitch`,
    * `cents` around each and wider the higher they are (a piano's partials run
    * sharp), in order, each partial's own.
    */
  private def partialBins(
      pitch: Int,
      frequencies: Array[Double],
      cents: Int
  ): Vector[Int] = {
    val f0 = hz(pitch)
    (1 to accompanimentPartials).toVector.flatMap { k =>
      val f = k * f0 * math.sqrt(1 + accompanimentInharmonicity * k * k)
      val width = math.max(
        f * (math
          .pow(2, (cents + accompanimentCentsPerPartial * k) / 1200.0) - 1),
        frequencies(1) * 1.01
      )
      frequencies.indices.filter(bin => math.abs(frequencies(bin) - f) <= width)
    }
  }

  /** The recording with what the accompaniment played taken out of it, as
    * speakers heard by the microphone. The accompaniment started with the
    * recording, at `rate` times the written tempo, from `startMillis` of the
    * score: its notes are known in time, give or take how late its sound
    * reached the microphone, which is found by trying delays. Where a note of
    * it sounds (and for its echo after it), the bins of its partials are
    * silenced, unless the part reviewed is written there too at about the same
    * time: the player's own notes keep theirs.
    */
  private def withoutAccompaniment(
      audio: Array[Float],
      accompaniment: Accompaniment,
      notes: Vector[ReferenceNote]
  ): Array[Float] = {
    val start = accompaniment.startMillis
    val rate = if (accompaniment.rate == 0) 1.0 else accompaniment.rate
    val played = accompaniment.notes
      .filter(note => note.onset + note.duration > start)
      .map(note =>
        (
          (note.onset - start) / 1000 / rate,
          (note.onset + note.duration - start) / 1000 / rate,
          note.pitch
        )
      )
      .toVector
    if (played.isEmpty) audio
    else {
      val spectra = Spectra.stft(
        audio,
        accompanimentFft,
        Hop,
        Spectra.hann(accompanimentFft)
      )
      val spacing = 1.0 / (accompanimentFft * (1.0 / Rate))
      val frequencies =
        Array.tabulate(accompanimentFft / 2 + 1)(k => k * spacing)
      val frames = spectra.size
      val frame = Hop.toDouble / Rate
      val magnitude = spectra.map(s =>
        Array.tabulate(s.length)(k =>
          math.log1p(math.hypot(s.re(k), s.im(k)).toFloat).toFloat
        )
      )
      val binsOf = played
        .map(_._3)
        .distinct
        .map(p => p -> partialBins(p, frequencies, accompanimentCents))
        .toMap
      // how late the accompaniment reached the microphone: the delay that puts
      // the most energy under its attacks
      val attacks = played
        .filter((a, _, _) => a / frame < frames)
        .map((a, _, p) => ((a / frame).toInt, binsOf(p)))
        .take(400)
      val shifts =
        -(accompanimentEarliest / frame).toInt to (accompanimentLongestDelay / frame).toInt
      val totals = shifts.map { shift =>
        attacks.foldLeft(0.0f) { case (sum, (f, bins)) =>
          if (f + shift < 0 || f + shift + 4 > frames) sum
          else {
            val values = for {
              bin <- bins
              t <- f + shift until f + shift + 4
            } yield magnitude(t)(bin).toDouble
            sum + (values.sum / values.size).toFloat
          }
        }
      }
      val delay = shifts(ReviewNumerics.argmax(totals.map(_.toDouble)))
      val mask = Array.fill(frames, accompanimentFft / 2 + 1)(1.0f)
      for ((a, b, p) <- played) {
        val first = (a / frame).toInt + delay
        val last =
          math.min(frames, ((b + accompanimentEcho) / frame).toInt + delay + 1)
        if (first < frames && last > math.max(first, 0)) {
          for {
            t <- math.max(first, 0) until last
            bin <- binsOf(p)
          } mask(t)(bin) = accompanimentLeft
        }
      }
      // the part's own notes keep their partials, at about where the
      // accompaniment's time puts them
      for (note <- notes) {
        val a = (note.onset - start) / 1000 / rate
        val b = (note.onset + note.duration - start) / 1000 / rate
        val first =
          math.max(0, ((a - accompanimentPartReach) / frame).toInt + delay)
        val last = math.min(
          frames,
          ((b + accompanimentPartReach) / frame).toInt + delay + 1
        )
        for (pitch <- note.pitches if last > first) {
          val bins = partialBins(pitch, frequencies, 0)
          for {
            t <- first until last
            bin <- bins
          } mask(t)(bin) = 1.0f
        }
      }
      val masked = spectra
        .zip(mask)
        .map((s, m) =>
          Complex(
            s.re.indices.map(k => s.re(k) * m(k)).toArray,
            s.im.indices.map(k => s.im(k) * m(k)).toArray
          )
        )
      Spectra.istft(masked, accompanimentFft, Hop, audio.length)
    }
  }
}
