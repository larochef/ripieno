package ripieno.backend.sound.violet

import ripieno.backend.sound.*

/** The engines' stems mixed into one sound. Each stem is brought to the same
  * level per part before they are added, since the two engines do not agree on
  * how loud a part is; the mix is then kept clear of clipping. The lines under
  * a violin part's chords (`partN-chordK`, `VioletLines`) are mostly silence:
  * they take their part's gain, not a level of their own; so do a part's
  * sections (`-sectionK`), its level measured on all of them.
  *
  * VIOLET makes sound where its part has no note (a line under a few chords is
  * otherwise a noise all along): a VIOLET stem is kept only around its line's
  * notes, faded in and out.
  */
object VioletMix {

  val rate = 48000
  // RMS of one part in the mix
  private val partLevel = 0.06
  // a chord's lower notes under its highest
  private val chordLine = 0.8f
  // seconds kept before a note and after it, and their fades
  private val before = 0.03
  private val release = 0.3
  private val fade = 0.02
  // the Conductor's velocity of a note with no dynamic: VIOLET's level as it is
  private val middleVelocity = 80
  // p (55) about 6 dB under it, ff (108) 7 dB over
  private val dbAVelocity = 0.25
  // dB a note is moved by, at most: a note VIOLET left out is not made a noise
  private val quietest = -6.0
  private val loudest = 9.0
  // seconds: a note's level is that of its loudest moment this long, as it is
  // heard (a short note's sound is mostly its attack)
  private val heardOver = 0.05
  private val peak = 0.98f

  /** A line's sound from VIOLET, the line as it was rendered (`VioletBowing`'s
    * file, read again) and its name (`VioletLines.Line`).
    */
  final case class Stem(name: String, audio: Array[Float], line: MidiFile)

  /** The mix of VIOLET's stems, each `lateSeconds` late, and of the other
    * engine's sound of `otherParts` parts.
    */
  def mixed(
      lateSeconds: Double,
      stems: Vector[Stem],
      others: Option[Array[Float]],
      otherParts: Int
  ): Array[Float] = {
    val late = math.rint(lateSeconds * rate).toInt
    val audio = stems.map(stem =>
      placed(gated(stem.audio.drop(late), stem.line), stem.line)
    )
    val partOf = stems.map(stem =>
      stem.name.replaceAll("(-chord\\d+)?(-section\\d+)?$", "")
    )
    val isChord = stems.map(_.name.contains("-chord"))
    // a part's level from its highest line, all its sections together
    val highest = stems.indices
      .filterNot(isChord)
      .groupBy(partOf)
      .view
      .mapValues(indices => summed(indices.map(audio)))
      .toMap
    val gains = highest.view.mapValues(gain(_, 1)).toMap
    val scaled = stems.indices.map { index =>
      val factor = gains.getOrElse(partOf(index), 1.0).toFloat *
        (if (isChord(index)) chordLine else 1.0f)
      audio(index).map(_ * factor)
    }
    val rest = others.map { sound =>
      val factor = gain(sound, math.max(1, otherParts)).toFloat
      sound.map(_ * factor)
    }
    val mix = summed(scaled ++ rest)
    val loudestSample =
      mix.foldLeft(0.0f)((found, value) => math.max(found, math.abs(value)))
    if (loudestSample > peak) mix.map(_ * (peak / loudestSample)) else mix
  }

  private def summed(sounds: Seq[Array[Float]]): Array[Float] = {
    val sum = new Array[Float](sounds.map(_.length).maxOption.getOrElse(0))
    for {
      sound <- sounds
      index <- sound.indices
    } {
      sum(index) += sound(index)
    }
    sum
  }

  private def gain(audio: Array[Float], parts: Int): Double = {
    val rms = math.sqrt(meanSquare(audio, 0, audio.length))
    if (rms > 0) partLevel * math.sqrt(parts.toDouble) / rms else 1.0
  }

  private def meanSquare(audio: Array[Float], from: Int, until: Int): Double = {
    var sum = 0.0
    for (index <- from until until) {
      val value = audio(index).toDouble
      sum += value * value
    }
    sum / (until - from)
  }

  /** The stem silenced outside its line's notes, and each note brought to the
    * level its velocity asks. VIOLET plays every note at one level, or rather
    * at the level it learnt for it (a run of short notes much softer than held
    * ones): each note is measured (its loudest moment) and set at the stem's
    * typical note level, moved by its velocity (the page Conductor's dynamics,
    * accents and balance), within `quietest` and `loudest`: VIOLET plays that
    * velocity too, as its dynamics, in its sound.
    */
  private def gated(audio: Array[Float], line: MidiFile): Array[Float] = {
    val gate = new Array[Float](audio.length)
    val notes = line.instruments.head.notes
      .filter(_.pitch >= VioletLines.lowest)
      .sortBy(_.start)
    def at(seconds: Double) = (seconds * rate).toInt
    val window = at(heardOver)
    val measured = notes.map { note =>
      val from = math.min(at(note.start), audio.length)
      val until =
        math.min(math.max(at(note.end), at(note.start) + 1), audio.length)
      Option.when(until > from) {
        val strongest =
          if (until - from <= window) meanSquare(audio, from, until)
          else
            (from to until - window by window / 5)
              .map(start => meanSquare(audio, start, start + window))
              .max
        10 * math.log10(strongest + 1e-12)
      }
    }
    val typical = median(measured.flatten)
    for ((note, level) <- notes.zip(measured)) {
      val wanted = typical + (note.velocity - middleVelocity) * dbAVelocity
      val change = level.fold(0.0)(heard =>
        math.min(loudest, math.max(quietest, wanted - heard))
      )
      val value = math.pow(10, change / 20).toFloat
      val from = math.max(0, at(note.start - before))
      val until = math.min(at(note.end + release), gate.length)
      for (index <- from until until) gate(index) = value
    }
    // a moving average over `fade`, centred as numpy's convolution is
    val width = (fade * rate).toInt
    val sums = new Array[Double](gate.length + 1)
    for (index <- gate.indices) sums(index + 1) = sums(index) + gate(index)
    Array.tabulate(audio.length) { index =>
      val from = math.max(0, index - (width - 1 - (width - 1) / 2))
      val until = math.min(gate.length, index + (width - 1) / 2 + 1)
      audio(index) * ((sums(until) - sums(from)) / width).toFloat
    }
  }

  private def median(values: Vector[Double]): Double =
    if (values.isEmpty) 0.0
    else {
      val sorted = values.sorted
      if (sorted.size % 2 == 1) sorted(sorted.size / 2)
      else (sorted(sorted.size / 2 - 1) + sorted(sorted.size / 2)) / 2
    }

  /** A section's sound where it stands in the score: `VioletLines` has it
    * rendered from its own start.
    */
  private def placed(audio: Array[Float], line: MidiFile): Array[Float] = {
    val offset = line.texts
      .collectFirst {
        case text if text.text.startsWith("offset ") =>
          text.text.split(" ")(1).toDouble
      }
      .getOrElse(0.0)
    Array.fill(math.rint(offset * rate).toInt)(0.0f) ++ audio
  }
}
