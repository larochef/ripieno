package ripieno.runner.models

/** A part's notes as MIDI-DDSP takes them (`note_list_to_sequence`): in the
  * order given (pretty_midi's, by their ends), frames of 4 ms rounded half to
  * even, a rest before each note that starts after the one before ends (none
  * for overlapping ones, which are not shortened: what follows comes later), a
  * rest from the start before the first note, a second's rest at the end. Pitch
  * 0 is a rest.
  */
final case class MidiDdspSequence(pitches: Vector[Int], frames: Vector[Int]) {

  /** Each note's length in seconds, as the model reads it: frames as floats,
    * times 0.004 in floats.
    */
  def seconds: Vector[Float] = frames.map(frame => frame.toFloat * 0.004f)

  /** Each note's frames again from its length in seconds (`round(l / 0.004)`),
    * where the frames' values are filled from: [onset, offset).
    */
  def spans: Vector[(Int, Int)] = {
    val lengths =
      seconds.map(length => math.rint(length.toDouble / 0.004).toInt)
    lengths
      .scanLeft(0)(_ + _)
      .zip(lengths)
      .map((onset, length) => (onset, onset + length))
  }

  def frameCount: Int = spans.lastOption.fold(0)(_._2)

  /** Each frame's value, from each note's: a note's from its onset to its
    * offset, the next note's over its own (`conditioning_df_to_dict`).
    */
  def framed(values: Vector[Float]): Array[Float] = {
    val out = new Array[Float](frameCount)
    for (((onset, offset), value) <- spans.zip(values)) {
      for (t <- onset until math.min(frameCount, offset + 1)) out(t) = value
    }
    out
  }

  /** The written pitch of each frame. */
  def pitchFrames: Array[Float] = framed(pitches.map(_.toFloat))

  /** Where each note (rests too) starts: one at its onset. */
  def onsets: Array[Float] = {
    val out = new Array[Float](frameCount)
    for ((onset, _) <- spans if onset < frameCount) out(onset) = 1
    out
  }

  /** Where each note (rests too) ends: one at its last frame, the frame before
    * its onset for one of no length (the last frame for a first one).
    */
  def offsets: Array[Float] = {
    val out = new Array[Float](frameCount)
    for ((_, offset) <- spans) out((offset - 1 + frameCount) % frameCount) = 1
    out
  }
}

object MidiDdspSequence {

  val FrameRate = 250
  private val endingRest = 250

  private def frame(seconds: Double): Int = math.rint(seconds * FrameRate).toInt

  /** The notes (pitch, start, end in seconds) as a sequence. */
  def of(notes: Vector[(Int, Double, Double)]): MidiDdspSequence = {
    val (pitches, lengths, _) =
      notes.foldLeft((Vector.empty[Int], Vector.empty[Int], 1000000)) {
        case ((pitches, lengths, previousOff), (pitch, start, end)) =>
          val (on, off) = (frame(start), frame(end))
          val rest = on - previousOff
          val (p, l) =
            if (rest > 0) (pitches :+ 0, lengths :+ rest)
            else (pitches, lengths)
          (p :+ pitch, l :+ (off - on), off)
      }
    val leading = notes.headOption.fold(0)((_, start, _) => frame(start))
    MidiDdspSequence((0 +: pitches) :+ 0, (leading +: lengths) :+ endingRest)
  }
}
