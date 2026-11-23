package ripieno.backend.sound.violet

import ripieno.backend.sound.*

import java.math.{BigDecimal, RoundingMode}
import scala.collection.mutable

/** How VIOLET is to bow each note of a line: VIOLET reads a stroke per note
  * from a keyswitch, a note below the violin's range sounding 10 ms before it
  * (work/violet/configs/ks_config.yaml). Given none, it plays everything as a
  * sustained stroke, which the parts it was trained on never had on a short
  * note: those were bounced, stopped or joined to the note before, and a run of
  * sustained sixteenths comes out with notes missing.
  *
  * The page's first desk (`Violinist`) says how each note is drawn, in a text
  * event at its start ("technique legato up"): this translates it into VIOLET's
  * strokes. A detached note long enough is sustained, never made staccato: the
  * breath the page leaves before a repeated note or a chord is not a stroke. A
  * note it says nothing of is bowed as before there was one: short notes get
  * the given stroke ("spiccato", "staccato" or "bow"), longer ones are
  * sustained.
  *
  * A change of bow ("bow") or a slur ("slur") joins a note to the one before,
  * which VIOLET wants overlapping it. A note repeating the one before it after
  * a breath (the page leaves one) is a change of bow, not a joined note; a
  * short note let go well before the next one is played staccato. VIOLET plays
  * one line: of notes starting together (a double stop, a chord) only the
  * highest is kept, the others throw it off the notes that follow. A trill
  * comes from the page as its notes (so that any engine plays it): VIOLET,
  * which knows trills, gets it back as one note with its trill stroke, the note
  * it stops on held after it.
  */
object VioletBowing {

  private val keyswitch = Map(
    "sustain" -> 36,
    "trill major" -> 38,
    "trill minor" -> 39,
    "staccato" -> 40,
    "spiccato" -> 41,
    "pizzicato" -> 43,
    "harmonic" -> 44,
    "bow" -> 48,
    "slur" -> 49
  )

  // seconds: the longest note the training parts bounced or stopped
  private val short = 0.4
  private val lead = 0.01
  private val overlap = 0.03
  // the strokes a joined change of bow or slur may follow
  private val joinable = Set("sustain", "bow", "slur")
  // seconds: notes starting this close are one chord
  private val together = 0.03
  // seconds: a detached note this long is drawn on the string, sustained, even
  // short, repeated or apart
  private val drawn = 0.25
  // seconds of silence after a short note that make it staccato
  private val detachedAfter = 0.06
  // the stroke of the note a trill stops on: slurred, its overlap with the
  // trill would split it off (`VioletLines`)
  private val tail = "sustain"
  // seconds: a technique this close to a note's start is its
  private val atOnce = 0.005
  // the first desk's techniques that have a stroke of their own
  private val strokes = Map(
    "staccato" -> "staccato",
    "spiccato" -> "spiccato",
    "volant" -> "spiccato",
    "pizzicato" -> "pizzicato",
    "harmonic" -> "harmonic",
    "legato" -> "slur",
    "loure" -> "sustain"
  )

  // a note whose end moves as the line is bowed; two notes alike stay two
  final private class Played(
      val velocity: Int,
      val pitch: Int,
      val start: Double,
      var end: Double
  ) {
    def note: MidiNote = MidiNote(velocity, pitch, start, end)
  }

  /** The line (one instrument, read from its file) with one note at a time and
    * the keyswitch of each note's stroke; short notes VIOLET is not told of
    * otherwise get `shortStroke`.
    */
  def bowed(line: MidiFile, shortStroke: String): MidiFile = {
    val part = line.instruments.head
    val named = techniques(line)
    val sorted = part.notes.sortBy(note => (note.start, -note.pitch))
    val single = sorted.foldLeft(Vector.empty[Played]) { (kept, note) =>
      if (kept.nonEmpty && note.start - kept.last.start < together) kept
      else {
        kept.lastOption.filter(_.end > note.start).foreach(_.end = note.start)
        kept :+ Played(note.velocity, note.pitch, note.start, note.end)
      }
    }
    val (notes, trills, tails) = joinedTrills(single, named)
    val following = notes.zip(notes.drop(1)).toMap
    val switches = Vector.newBuilder[MidiNote]
    var previous: Option[Played] = None
    var previousStroke = ""
    for (note <- notes) {
      val technique = techniqueAt(named, note.start)
      val detached = following
        .get(note)
        .exists(later => later.start - note.end >= detachedAfter)
      val repeated = previous.exists(before =>
        before.pitch == note.pitch && before.end < note.start
      )
      val length = note.end - note.start
      var stroke =
        if (trills.contains(note)) trills(note)
        else if (tails.contains(note)) tail
        else if (technique.exists(strokes.contains)) strokes(technique.get)
        else if (technique.contains("detache") && length >= drawn) "sustain"
        else if (length < short) {
          if (detached && !technique.contains("detache")) "staccato"
          else shortStroke
        } else "sustain"
      if (
        repeated && Set("sustain", "slur", shortStroke).contains(stroke) &&
        !technique.contains("loure") &&
        !(technique.contains("detache") && stroke == "sustain")
      ) {
        // the breath the page left before a repeated note: a change of bow,
        // not a joined note
        stroke = "bow"
      } else if (stroke == "bow" || stroke == "slur") {
        val follows = previous.filter(before =>
          before.end >= note.start - 0.02 && before.pitch != note.pitch
        )
        follows.filter(_ => joinable.contains(previousStroke)) match {
          case Some(before) =>
            before.end = note.start + math.min(overlap, length / 4)
          case None => stroke = "sustain"
        }
      }
      val start = math.max(0.0, note.start - lead)
      switches += MidiNote(100, keyswitch(stroke), start, start + lead)
      previous = Some(note)
      previousStroke = stroke
    }
    line.copy(instruments =
      Vector(part.copy(notes = notes.map(_.note) ++ switches.result()))
    )
  }

  // the first desk's technique at each time it names one, by its time to the
  // millisecond, in the order first named
  private def techniques(
      line: MidiFile
  ): mutable.LinkedHashMap[Double, String] = {
    val found = mutable.LinkedHashMap.empty[Double, String]
    for (event <- line.texts) {
      val words = event.text.split("\\s+").filter(_.nonEmpty)
      if (words.length >= 2 && words(0) == "technique") {
        found(milliseconds(event.time)) = words(1)
      }
    }
    found
  }

  // Python's round(time, 3): the exact value rounded, half to even
  private def milliseconds(time: Double): Double =
    BigDecimal(time).setScale(3, RoundingMode.HALF_EVEN).doubleValue

  private def techniqueAt(
      found: mutable.LinkedHashMap[Double, String],
      time: Double
  ): Option[String] =
    Vector(time, time - 0.001, time + 0.001)
      .map(milliseconds)
      .collectFirst { case at if found.contains(at) => found(at) }
      .orElse(found.collectFirst {
        case (at, name) if math.abs(at - time) <= atOnce => name
      })

  /** The notes with each trill the page wrote out (its first note marked
    * "trill", the others "legato", going between the note above and the note)
    * made one note again, the trill stroke of each, and the notes they stop on.
    */
  private def joinedTrills(
      notes: Vector[Played],
      named: mutable.LinkedHashMap[Double, String]
  ): (Vector[Played], Map[Played, String], Set[Played]) = {
    val joined = Vector.newBuilder[Played]
    val trills = Map.newBuilder[Played, String]
    val tails = Set.newBuilder[Played]
    var index = 0
    while (index < notes.size) {
      val note = notes(index)
      val trill =
        if (
          techniqueAt(named, note.start).contains(
            "trill"
          ) && index + 1 < notes.size
        ) {
          val (upper, main) = (note.pitch, notes(index + 1).pitch)
          val step = note.end - note.start
          var last = index + 1
          while (
            last + 1 < notes.size &&
            Set(upper, main).contains(notes(last + 1).pitch) &&
            techniqueAt(named, notes(last + 1).start).contains("legato") &&
            notes(last + 1).start - notes(last).end < 0.02 &&
            notes(last + 1).end - notes(last + 1).start < 1.5 * step
          ) {
            last += 1
          }
          val interval = upper - main
          Option.when(
            (interval == 1 || interval == 2) &&
              techniqueAt(named, notes(index + 1).start).contains("legato")
          ) {
            val whole = Played(note.velocity, main, note.start, notes(last).end)
            trills += whole -> (if (interval == 1) "trill minor"
                                else "trill major")
            joined += whole
            index = last + 1
            // the note held after the trill, which stops before the next note:
            // one with the trill
            if (
              index < notes.size && notes(index).pitch == main &&
              notes(index).start - whole.end < 0.02
            ) {
              tails += notes(index)
            }
          }
        } else None
      if (trill.isEmpty) {
        joined += note
        index += 1
      }
    }
    (joined.result(), trills.result(), tails.result())
  }
}
