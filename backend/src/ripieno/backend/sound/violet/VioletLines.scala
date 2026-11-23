package ripieno.backend.sound.violet

import ripieno.backend.sound.*

/** A score's MIDI split between the engines: a violin part (General MIDI
  * program 40) within the range VIOLET was trained on goes to VIOLET, every
  * other part to the engine of the other instruments.
  *
  * VIOLET plays one line: a violin part's double and triple stops are split
  * into lines, its highest notes with the rest of the part in `partN`, the
  * notes under them in `partN-chordK` (K from 1, downwards), each rendered as
  * its own line and mixed at the level of its part (`VioletMix`). A broken
  * chord's upper notes go on the lines of its lower ones, slurred to them by
  * the page: rendered on lines of their own, four violins' attacks at once were
  * heard as a din. Where the page marks a new part of the score ("section" text
  * events: a movement, a new tempo), each line is cut, its sections in files of
  * their own (`-sectionK`), each from its own start ("offset" text event): the
  * model's windows never join two, nor change when what comes before is played
  * longer.
  */
object VioletLines {

  private val violin = 40
  // G3 to A7, the rows of VIOLET's note roll
  val lowest = 55
  private val highest = 105
  // seconds: notes starting this close are one chord
  private val together = 0.03
  // how long before its first note a section's file starts: the bowing
  // switches set just before it (`VioletBowing`)
  private val leadIn = 1.0

  /** A line of a violin part, named as its file is (`part0-chord1-section2`).
    */
  final case class Line(name: String, file: MidiFile)

  /** The lines VIOLET renders, how many violin parts they come from, and the
    * other parts, for the other engine, when there are.
    */
  final case class Split(
      lines: Vector[Line],
      violins: Int,
      others: Option[MidiFile]
  )

  def split(midi: MidiFile): Split = {
    val starts = 0.0 +: midi.texts
      .filter(event => event.text == "section" && event.time > 0)
      .map(_.time)
      .sorted
    val played = midi.instruments.filter(_.notes.nonEmpty)
    val (violins, others) = played.partition(isViolet)
    val lines = violins.zipWithIndex.flatMap { (instrument, part) =>
      linesOf(instrument).zipWithIndex.flatMap { (notes, rank) =>
        sectionsOf(notes, starts).zipWithIndex.collect {
          case (inSection, index) if inSection.nonEmpty =>
            val offset = math.max(0.0, starts(index) - leadIn)
            val until =
              starts.lift(index + 1).getOrElse(Double.PositiveInfinity)
            val techniques = midi.texts
              .filter(event =>
                event.text.startsWith("technique") &&
                  starts(index) - 1e-3 <= event.time && event.time < until
              )
              .map(event => event.copy(time = event.time - offset))
            val name = s"part$part" +
              (if (rank > 0) s"-chord$rank" else "") +
              (if (starts.size > 1) s"-section$index" else "")
            Line(
              name,
              MidiFile(
                Vector(
                  MidiInstrument(
                    instrument.program,
                    false,
                    instrument.name,
                    inSection.map(note =>
                      note.copy(
                        start = note.start - offset,
                        end = note.end - offset
                      )
                    )
                  )
                ),
                techniques :+ MidiText(s"offset $offset", 0.0)
              )
            )
        }
      }
    }
    Split(
      lines,
      violins.size,
      Option.when(others.nonEmpty)(MidiFile(others, Vector.empty))
    )
  }

  private def isViolet(instrument: MidiInstrument): Boolean = {
    val pitches = instrument.notes.map(_.pitch)
    instrument.program == violin && !instrument.isDrum &&
    pitches.min >= lowest && pitches.max <= highest
  }

  /** The part's notes as lines: the highest of each chord with the single
    * notes, then the next ones down. A chord's notes are those sounding
    * together, not only those starting together: a broken chord's upper notes
    * come a little after the lower ones, while these still sound (`voices`).
    */
  private def linesOf(instrument: MidiInstrument): Vector[Vector[MidiNote]] = {
    // notes as indices: two notes alike are still two notes
    val notes = instrument.notes
    val sorted =
      notes.indices.sortBy(index => (notes(index).start, -notes(index).pitch))
    val chords = sorted.foldLeft(Vector.empty[Vector[Int]]) { (found, index) =>
      val note = notes(index)
      found.lastOption match {
        case Some(chord)
            if note.start - notes(chord.head).start < together ||
              note.start < chord.map(notes(_).end).min - together =>
          found.init :+ (chord :+ index)
        case _ => found :+ Vector(index)
      }
    }
    chords.foldLeft(Vector.empty[Vector[MidiNote]]) { (lines, chord) =>
      voices(chord, notes).zipWithIndex.foldLeft(lines) {
        case (found, (voice, rank)) =>
          val grown = found.padTo(rank + 1, Vector.empty)
          grown.updated(rank, grown(rank) ++ voice.map(notes))
      }
    }
  }

  /** A chord's notes as lines, the highest first. A broken chord's upper notes
    * come after its lower ones, which let go: the bow goes from the lower
    * strings to the upper, so each upper note goes on the line of a lower one
    * (the highest after the highest), and the notes held through sound on lines
    * of their own.
    */
  private def voices(
      chord: Vector[Int],
      notes: Vector[MidiNote]
  ): Vector[Vector[Int]] = {
    def pitch(index: Int) = notes(index).pitch
    val first = chord.filter(index =>
      notes(index).start - notes(chord.head).start < together
    )
    val late = chord.filterNot(first.contains).sortBy(-pitch(_))
    val heldUntil = chord.map(notes(_).end).max
    val leaving =
      first.filter(notes(_).end < heldUntil - together).sortBy(-pitch(_))
    if (late.isEmpty || leaving.isEmpty) {
      chord.sortBy(-pitch(_)).map(Vector(_))
    } else {
      val paired = leaving.zip(late).map((lower, upper) => Vector(lower, upper))
      val alone =
        (late.drop(leaving.size) ++ leaving.drop(late.size)).map(Vector(_))
      val held = first.filterNot(leaving.contains).map(Vector(_))
      (paired ++ alone ++ held).sortBy(voice => -voice.map(pitch).max)
    }
  }

  /** A line's notes by the section of the score they start in, its sections
    * starting at `starts`.
    */
  private def sectionsOf(
      notes: Vector[MidiNote],
      starts: Vector[Double]
  ): Vector[Vector[MidiNote]] = {
    val sectionOf =
      notes.map(note => starts.lastIndexWhere(_ <= note.start + 1e-3).max(0))
    starts.indices.toVector.map(index =>
      notes.zip(sectionOf).collect { case (note, `index`) => note }
    )
  }
}
