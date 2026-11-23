package ripieno.backend.review

import ripieno.shared.ReferenceNote

/** What is heard in a recording, frame by frame (`ReviewFeatures.FrameSeconds`
  * apart): the pitch the tracker hears (`heard`, MIDI, NaN where none), how
  * present each pitch from `lowest` up is (`values`, compressed for following
  * the score, and `linear`, `[frames][pitches]`), the level (dB), and the
  * strength of attacks; the recording's length, and the share of its seconds
  * that saturate.
  */
final case class ReviewFeatures(
    heard: Array[Double],
    values: Array[Array[Double]],
    linear: Array[Array[Double]],
    lowest: Int,
    level: Array[Float],
    onsetStrength: Array[Float],
    seconds: Double,
    clipped: Double
) {
  def loudest: Float = level.max
}

object ReviewFeatures {
  val Rate = 22050
  val Hop = 256
  val FrameSeconds: Double = Hop.toDouble / Rate
  // this far under the loudest moment is silence
  val SilentBelowPeakDb = 40
}

/** Score seconds to recording seconds: the written notes against how present
  * each of them is in the recording, frame by frame, by dynamic time warping.
  */
object ScoreAlignment {
  import ReviewFeatures.*

  // the playing starts with a sound no further than this under the loudest,
  // held with a pitch this long; the alignment starts this much before it
  private val startBelowPeakDb = 25
  private val startHoldSeconds = 0.25
  private val startLeadSeconds = 0.15
  // how much before where an accompaniment has the player come in they may
  // start: the page knows where the score stood to a tenth of a second,
  // speakers bring the accompaniment a little late, a player may come in early
  private val entryLeadSeconds = 1.0
  // frames of the alignment over which an attack's rise is measured, and a
  // rise of a third of the frame's most salient pitch is a full attack
  private val onsetRiseFrames = 2
  private val onsetRiseGain = 3
  // what a written onset without a rise costs, against a note without its pitch
  private val onsetWeight = 0.6
  // what a rest costs where the recording sounds
  private val soundingRest = 0.7

  // Steps of the alignment, in (recording frames, score frames), and what each
  // weighs: the path may run from a third to three times the written speed,
  // and no further. Free to stall or leap, it would skip a wrong note to the
  // same pitch a little further on.
  private val steps = Vector(
    (1, 1) -> 1.0f,
    (1, 2) -> 1.5f,
    (2, 1) -> 1.5f,
    (1, 3) -> 2.0f,
    (3, 1) -> 2.0f
  )

  /** The frames from where the playing starts to the last that is not silence:
    * what is before and after is left out of the alignment, which would
    * otherwise have to stretch the score over it.
    *
    * The playing starts at the first sound held with a pitch, loud enough, for
    * a quarter of a second: not at the metronome's last click, the instrument
    * taken up or the bow set on the string before it. Where an accompaniment
    * played with the recording, the player came in no earlier than it has them
    * come in (`entry`, recording seconds), give or take a second: what sounds
    * before is the accompaniment, what is left of it once taken out. A sound
    * not silent is loud enough then: a player may come in softly, or late.
    * (What is left of the accompaniment can also pass for the player's first
    * note just before it; telling them apart by pitch fails where the player
    * doubles the accompaniment, or starts on a wrong note.)
    */
  def playingSpan(
      features: ReviewFeatures,
      entry: Option[Double]
  ): (Int, Int) = {
    val level = features.level
    val loudest = features.loudest
    val sounding =
      level.indices.filter(index => level(index) > loudest - SilentBelowPeakDb)
    val last = sounding.last + 1
    val earliest = entry
      .map(seconds =>
        math.max(0, ((seconds - entryLeadSeconds) / FrameSeconds).toInt)
      )
      .getOrElse(0)
    val loudEnough =
      loudest - (if (entry.isDefined) SilentBelowPeakDb else startBelowPeakDb)
    val playing = level.indices.map(index =>
      !features.heard(index).isNaN && level(index) > loudEnough
    )
    val hold = math.max(1, (startHoldSeconds * Rate / Hop).toInt)
    val held = (earliest to level.length - hold).find(start =>
      (start until start + hold).forall(playing)
    )
    val first =
      held.getOrElse(sounding.find(_ >= earliest).getOrElse(sounding.head))
    (math.max(earliest, first - (startLeadSeconds * Rate / Hop).toInt), last)
  }

  /** Score seconds and recording seconds, both increasing, one point for each
    * frame of the score the path goes through.
    */
  def aligned(
      features: ReviewFeatures,
      notes: Vector[ReferenceNote],
      entry: Option[Double]
  ): (Array[Double], Array[Double]) = {
    val frame = 2.0 * Hop / Rate
    val (first, cost) = costs(features, notes, entry)
    val offset = notes.head.onset / 1000
    val path = openEndedPath(cost)
    val scoreSeconds = path.map((_, column) => column * frame + offset)
    val recordingSeconds =
      path.map((row, _) => row * frame + first * Hop.toDouble / Rate)
    // one point a score frame: the first the path reaches it at
    val firsts = scoreSeconds.indices.filter(index =>
      index == 0 || scoreSeconds(index) != scoreSeconds(index - 1)
    )
    (firsts.map(scoreSeconds).toArray, firsts.map(recordingSeconds).toArray)
  }

  // the frame the alignment starts from, and what each frame of the recording
  // (every other one from there, `[rows]`) costs against each frame of the
  // score (`[columns]`)
  private def costs(
      features: ReviewFeatures,
      notes: Vector[ReferenceNote],
      entry: Option[Double]
  ): (Int, Array[Array[Float]]) = {
    val frame = 2.0 * Hop / Rate
    val (start, last) = playingSpan(features, entry)
    val first = start - start % 2
    // the salience may have a frame fewer than the level
    val rows =
      (first until math.min(last, features.values.length) by 2).toVector
    val values = rows.map(features.values)
    val width = features.values.head.length
    val loudest = features.loudest
    val loud =
      rows.map(row => features.level(row) > loudest - SilentBelowPeakDb)
    // the score from its first note, as the recording from its first sound
    val offset = notes.head.onset / 1000
    val end = notes.map(note => note.onset + note.duration).max / 1000 - offset
    val columns = (end / frame).toInt + 1
    val written = Array.fill(columns)(-1)
    for (note <- notes) {
      val from = ((note.onset / 1000 - offset) / frame).toInt
      val until =
        ((note.onset / 1000 + note.duration / 1000 - offset) / frame).toInt + 1
      val (a, b) = ReviewNumerics.span(columns, from, until)
      for (column <- a until b)
        written(column) = note.pitches.min - features.lowest
    }
    def clipped(pitch: Int) = math.max(0, math.min(width - 1, pitch))
    // Where a note begins in the score, its pitch must rise in the recording:
    // an open string ringing on is present all along, and only its attacks
    // say where its notes are.
    val beginning = Array.fill(columns)(-1)
    for (note <- notes) {
      val at = ((note.onset / 1000 - offset) / frame).toInt
      val (a, b) = ReviewNumerics.span(
        columns,
        math.max(0, at - 1),
        at + onsetRiseFrames + 1
      )
      for (column <- a until b)
        beginning(column) = note.pitches.min - features.lowest
    }
    val cost = Array.tabulate(values.size, columns) { (row, column) =>
      val base =
        if (written(column) < 0) (if (loud(row)) soundingRest else 0.0)
        else 1 - values(row)(clipped(written(column)))
      val attack =
        if (beginning(column) < 0) 0.0
        else {
          val pitch = clipped(beginning(column))
          val rise =
            if (row < onsetRiseFrames) 0.0
            else {
              // np.roll wraps around, but the first rows are set to no rise
              val before = values(
                (row - onsetRiseFrames + values.size) % values.size
              )(pitch)
              math.max(
                0.0,
                math.min(1.0, onsetRiseGain * (values(row)(pitch) - before))
              )
            }
          onsetWeight * (1 - rise)
        }
      (base + attack).toFloat
    }
    (first, cost)
  }

  /** The cheapest path from the first frame of both to the last frame of the
    * recording, ending wherever in the score suits it best: the player may stop
    * before the end. Rows of the recording are filled one at a time, each from
    * the rows before it, in floats as the reference does.
    */
  private def openEndedPath(cost: Array[Array[Float]]): Vector[(Int, Int)] = {
    val rows = cost.length
    val columns = cost.head.length
    val total = Array.fill(rows, columns)(Float.PositiveInfinity)
    val came = Array.ofDim[Byte](rows, columns)
    total(0)(0) = cost(0)(0)
    for (row <- 1 until rows) {
      val best = Array.fill(columns)(Float.PositiveInfinity)
      for ((((di, dj), weight), index) <- steps.zipWithIndex if row - di >= 0) {
        val before = total(row - di)
        for (column <- dj until columns) {
          val candidate = before(column - dj) + weight * cost(row)(column)
          if (candidate < best(column)) {
            best(column) = candidate
            came(row)(column) = index.toByte
          }
        }
      }
      total(row) = best
    }
    val reach =
      (0 until columns).filter(column => !total(rows - 1)(column).isInfinite)
    val end = reach(
      ReviewNumerics.argmin(
        reach.map(column => total(rows - 1)(column).toDouble / (rows + column))
      )
    )
    val path = Vector.newBuilder[(Int, Int)]
    var (row, column) = (rows - 1, end)
    var going = true
    while (going) {
      path += row -> column
      if (column == 0) going = false
      else {
        val ((di, dj), _) = steps(came(row)(column))
        row -= di
        column -= dj
        if (row < 0 || column < 0) going = false
      }
    }
    path.result().reverse
  }
}
