package ripieno.frontend.score

/** The engraver's time as played: stretched where the score goes slower or
  * faster than the engraver plays it (the changes the server reads in the
  * score, `ScoreTiming`), in milliseconds and in the MIDI.
  */
object TempoChanges {

  /** A time of the engraver's, in milliseconds, as played with the stretches
    * found at those times (each from then on, in order).
    */
  def stretched(changes: Vector[(Double, Double)])(millis: Double): Double =
    changes.indices.foldLeft(millis) { (time, index) =>
      val (from, slower) = changes(index)
      val until = changes.lift(index + 1).fold(millis)(_._1)
      time + (slower - 1) * math.max(0.0, math.min(millis, until) - from)
    }

  /** The time of the engraver's that is played at `millis`. */
  def unstretched(changes: Vector[(Double, Double)])(millis: Double): Double = {
    // the stretch is continuous and rises: search it
    var low = 0.0
    var high = math.max(millis, 1.0)
    while (stretched(changes)(high) < millis) high *= 2
    for (_ <- 0 until 60) {
      val middle = (low + high) / 2
      if (stretched(changes)(middle) < millis) low = middle else high = middle
    }
    (low + high) / 2
  }

  /** The sequence with its tempo stretched from each tick on, its own tempo
    * changes kept.
    */
  def stretched(
      sequence: MidiSequence,
      changes: Seq[(Int, Double)]
  ): MidiSequence = {
    def isTempo(event: MidiEvent) =
      event.data.headOption.contains(0xff) && event.data.lift(1).contains(0x51)
    val written = sequence.tracks.flatten
      .filter(isTempo)
      .map(event =>
        event.tick -> event.data
          .takeRight(3)
          .foldLeft(0)((sum, byte) => (sum << 8) | byte)
      )
      .sortBy(_._1)
    val ticks = (written.map(_._1) ++ changes.map(_._1)).distinct.sorted
    val tempi = ticks.map { tick =>
      val micros = written.findLast(_._1 <= tick).fold(500000)(_._2)
      val slower =
        changes.filter(_._1 <= tick).maxByOption(_._1).fold(1.0)(_._2)
      // a cut passes at once: a microsecond a quarter note
      val value =
        math.max(1, math.min(0xffffff, math.round(micros * slower).toInt))
      MidiEvent(
        tick,
        Vector(
          0xff,
          0x51,
          3,
          value >> 16 & 0xff,
          value >> 8 & 0xff,
          value & 0xff
        )
      )
    }
    // the notes of a cut are not played
    val sorted = changes.sortBy(_._1).toVector
    val cut = sorted.indices.collect {
      case index if sorted(index)._2 == 0 =>
        sorted(index)._1 -> sorted.lift(index + 1).fold(Int.MaxValue)(_._1)
    }
    def inCut(event: MidiEvent) =
      event.isNoteOn && cut.exists((from, to) =>
        event.tick >= from && event.tick < to
      )
    sequence.copy(tracks = sequence.tracks.zipWithIndex.map { (track, index) =>
      val kept = track.filterNot(event => isTempo(event) || inCut(event))
      if (index == 0) kept ++ tempi else kept
    })
  }
}
