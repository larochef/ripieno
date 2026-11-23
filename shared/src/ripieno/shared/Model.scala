package ripieno.shared

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.tapir.Schema

/** A score's sound as the server renders it: on its way, ready, or failed and
  * why. While it is on its way, `progress` is how much of it is made, in
  * percent, once the engine has started on it.
  */
final case class RenderedSound(
    id: String,
    ready: Boolean,
    failure: Option[String],
    progress: Option[Int]
)

object RenderedSound {
  given JsonValueCodec[RenderedSound] = JsonCodecMaker.make
  given Schema[RenderedSound] = Schema.derived
}

/** A note of the part to be played, as the page engraves it: its id in the
  * sheet, when it starts and how long it lasts in milliseconds of the score at
  * its written tempo, its MIDI pitches (several for a chord) and its measure.
  */
final case class ReferenceNote(
    id: String,
    onset: Double,
    duration: Double,
    pitches: List[Int],
    measure: String
)

/** Where a measure starts, in milliseconds of the score; a repeated one is
  * there once for each time through.
  */
final case class ReferenceMeasure(number: String, start: Double)

/** A marking of the part: `kind` is "dynamics" (p, f…), "wedge" (crescendo or
  * diminuendo, until `end`) or "words" (rit., a tempo…), its milliseconds those
  * of the score.
  */
final case class Marking(
    kind: String,
    text: String,
    measure: String,
    onset: Double,
    end: Option[Double]
)

/** A note of the other parts, played along with the recording: milliseconds of
  * the score and a MIDI pitch.
  */
final case class AccompanimentNote(onset: Double, duration: Double, pitch: Int)

/** The other parts, played with the recording from its start: from
  * `startMillis` of the score, at `rate` times the written tempo. `onSpeakers`
  * when the microphone hears them too, and the review takes them out of what it
  * hears.
  */
final case class Accompaniment(
    onSpeakers: Boolean,
    startMillis: Double,
    rate: Double,
    notes: List[AccompanimentNote]
)

/** What a recording of a part is reviewed against: the notes from where the
  * player starts, the measures, the markings, and how long a beat lasts; and,
  * to find recordings of the same again, the score's path, the part's name and
  * the written tempo (`beat` symbol and beats a minute); the `accompaniment`
  * played along, if any.
  */
final case class PerformanceReference(
    notes: List[ReferenceNote],
    measures: List[ReferenceMeasure],
    beatMillis: Double,
    markings: List[Marking],
    score: Option[String] = None,
    part: Option[String] = None,
    beat: Option[String] = None,
    writtenBeatsPerMinute: Option[Double] = None,
    accompaniment: Option[Accompaniment] = None
)

object PerformanceReference {
  given JsonValueCodec[PerformanceReference] = JsonCodecMaker.make
  given Schema[ReferenceNote] = Schema.derived
  given Schema[ReferenceMeasure] = Schema.derived
  given Schema[Marking] = Schema.derived
  given Schema[AccompanimentNote] = Schema.derived
  given Schema[Accompaniment] = Schema.derived
  given Schema[PerformanceReference] = Schema.derived
}

/** A note as it was played: its `verdict` is "right", "intonation", "wrong",
  * "missed" or "uncertain"; `cents` how far it was from the written pitch when
  * that is known, `heard` the MIDI pitch heard instead of a wrong one, `at`
  * where it starts in the recording (milliseconds), and `rhythm` "early" or
  * "late" by `onsetMillis` when it is off the player's own tempo there.
  */
final case class ReviewedNote(
    id: String,
    measure: String,
    verdict: String,
    cents: Option[Int],
    heard: Option[Double],
    at: Double,
    rhythm: Option[String],
    onsetMillis: Option[Int]
)

/** The player's tempo at a point of the score, against their tempo over the
  * whole recording: above 1 is faster.
  */
final case class TempoPoint(at: Double, tempo: Double)

/** A span of the score played faster or slower than the rest, by up to
  * `percent`, where the score asks for no change.
  */
final case class TempoDrift(
    kind: String,
    fromMeasure: String,
    toMeasure: String,
    percent: Int
)

final case class TempoReview(
    overallRatio: Double,
    curve: List[TempoPoint],
    drifts: List[TempoDrift]
)

/** A change of dynamics the score asks for (from p to f, or a wedge) heard as
  * only `heardDb` decibels.
  */
final case class WeakDynamics(
    from: Option[String],
    to: Option[String],
    text: Option[String],
    measure: String,
    heardDb: Double
)

/** `marked` says whether the part has any dynamics to follow at all. */
final case class DynamicsReview(
    weakChanges: List[WeakDynamics],
    weakWedges: List[WeakDynamics],
    marked: Boolean
)

/** A moment of the recording and the moment of the score it plays, both in
  * milliseconds.
  */
final case class AlignmentPoint(recording: Double, score: Double)

/** `tuningCents` is how high or low the player's instrument stands against A =
  * 440 Hz, which intonation is measured against; `clippedShare` the share of
  * the recording's seconds that saturate.
  */
final case class PerformanceReview(
    notes: List[ReviewedNote],
    tempo: TempoReview,
    dynamics: DynamicsReview,
    alignment: List[AlignmentPoint],
    recordingSeconds: Double,
    tuningCents: Option[Int],
    clippedShare: Option[Double]
)

/** A review on its way, ready, or failed and why. */
final case class ReviewStatus(
    id: String,
    ready: Boolean,
    failure: Option[String],
    review: Option[PerformanceReview]
)

object ReviewStatus {
  given JsonValueCodec[ReviewStatus] = JsonCodecMaker.make
  given Schema[ReviewedNote] = Schema.derived
  given Schema[TempoPoint] = Schema.derived
  given Schema[TempoDrift] = Schema.derived
  given Schema[TempoReview] = Schema.derived
  given Schema[WeakDynamics] = Schema.derived
  given Schema[DynamicsReview] = Schema.derived
  given Schema[AlignmentPoint] = Schema.derived
  given Schema[PerformanceReview] = Schema.derived
  given Schema[ReviewStatus] = Schema.derived
}

object PerformanceReview {
  given JsonValueCodec[PerformanceReview] = JsonCodecMaker.make
}

/** A change to a score, made to its note `note` (its id in the score as the
  * page names its notes: see the page's `MusicXml.withIds`). The source is
  * never changed: an edition is a list of these, played over it.
  */
sealed trait Edit {
  def note: String
}

object Edit {

  /** `accidental` is the one shown ("sharp", "flat", "natural"), if any. */
  final case class SetPitch(
      note: String,
      step: String,
      alter: Int,
      octave: Int,
      accidental: Option[String]
  ) extends Edit

  /** `value` is a MusicXML note type ("quarter", "eighth"…). */
  final case class SetDuration(note: String, value: String, dots: Int)
      extends Edit

  /** The note becomes a rest of the same length. */
  final case class MakeRest(note: String) extends Edit

  /** Ties the note to the next one of the voice, or unties it. */
  final case class SetTie(note: String, tied: Boolean) extends Edit

  /** Beams the note with the next one of its voice, or breaks the beam between
    * them.
    */
  final case class SetBeam(note: String, joined: Boolean) extends Edit

  /** A new note `id` after `note`, of its length, or above it in a chord. */
  final case class AddNote(
      note: String,
      id: String,
      step: String,
      alter: Int,
      octave: Int,
      chord: Boolean
  ) extends Edit

  final case class SetFingering(note: String, fingering: Option[String])
      extends Edit

  /** "up-bow" or "down-bow", or none. */
  final case class SetBowing(note: String, bowing: Option[String]) extends Edit

  /** A slur from `note` to `to`. */
  final case class AddSlur(note: String, to: String) extends Edit

  /** The slurs starting or ending on `note` are taken away. */
  final case class RemoveSlurs(note: String) extends Edit

  /** A dynamic marking ("p", "mf"…) at the note, or none of those added. */
  final case class SetDynamics(note: String, dynamics: Option[String])
      extends Edit

  /** Words above the note: "dolce", "rit."… */
  final case class AddText(note: String, text: String) extends Edit
}

/** A layer of changes to a score, by name: corrections, fingerings… Those
  * `visible` are played over the source, in order.
  */
final case class Edition(name: String, visible: Boolean, edits: List[Edit])

/** The editions of a score, the first played first. */
final case class Editions(editions: List[Edition])

object Editions {
  given JsonValueCodec[Editions] = JsonCodecMaker.make
  given Schema[Edit] = Schema.derived
  given Schema[Edition] = Schema.derived
  given Schema[Editions] = Schema.derived
}

/** The tempo a section of a score at a tempo of its own is played at by
  * default, in quarter notes a minute: the section by its rank, from 0, as the
  * page finds them.
  */
final case class TempoSetting(section: Int, quarterNotes: Int)

/** The tempi a score's sections are played at: settings kept beside the score,
  * never written into it.
  */
final case class ScoreTempi(sections: List[TempoSetting])

object ScoreTempi {
  given JsonValueCodec[ScoreTempi] = JsonCodecMaker.make
  given Schema[TempoSetting] = Schema.derived
  given Schema[ScoreTempi] = Schema.derived
}

/** A part of a score at a tempo of its own, from its first note (its id): its
  * name is the words starting it, if any; its tempo, in quarter notes a minute,
  * the one written, if any, and the one it is played at: the one chosen, else
  * the last one written before it, else one guessed from its words and its
  * rhythm.
  */
final case class TempoSection(
    index: Int,
    name: Option[String],
    written: Option[Double],
    played: Double,
    firstNote: String
)

/** A passage the score allows to cut: its words, if any, and the measures it
  * starts and ends in.
  */
final case class ScoreCut(
    index: Int,
    label: Option[String],
    from: String,
    to: String
)

/** Where the score goes slower or faster than the engraver plays it: from the
  * note `note`, less `before` quarter notes, `slower` times slower (0 where a
  * cut made takes no time).
  */
final case class TimeChange(note: String, slower: Double, before: Double)

/** How a score is timed: its sections at a tempo of their own, the cuts it
  * allows and where its time changes, with the tempi chosen and the cuts made.
  */
final case class ScoreTiming(
    sections: List[TempoSection],
    cuts: List[ScoreCut],
    changes: List[TimeChange]
)

object ScoreTiming {
  given JsonValueCodec[ScoreTiming] = JsonCodecMaker.make
  given Schema[TempoSection] = Schema.derived
  given Schema[ScoreCut] = Schema.derived
  given Schema[TimeChange] = Schema.derived
  given Schema[ScoreTiming] = Schema.derived
}

/** A score the page plays: its path, the editions played over it (the visible
  * ones, in order), the tempi chosen for its sections and the cuts made.
  */
final case class ScoreRequest(
    path: String,
    editions: List[Edition],
    chosen: List[TempoSetting],
    cuts: List[Int]
)

object ScoreRequest {
  import Editions.given
  import ScoreTempi.given
  given JsonValueCodec[ScoreRequest] = JsonCodecMaker.make
  given Schema[ScoreRequest] = Schema.derived
}

/** A score ready to engrave: its MusicXML, every note with its id, the editions
  * played over it and its parts' instruments, the edits that found no note to
  * change (made over another state of the source), its timing and how the
  * conductor has it played.
  */
final case class PreparedScore(
    musicXml: String,
    lost: List[Edit],
    timing: ScoreTiming,
    interpretation: Interpretation,
    reading: ScoreReading
)

object PreparedScore {
  import Editions.given
  import Interpretation.given
  import ScoreReading.given
  import ScoreTiming.given
  given JsonValueCodec[PreparedScore] = JsonCodecMaker.make
  given Schema[PreparedScore] = Schema.derived
}

/** A recording reviewed, in a few figures, to be listed and compared with the
  * other recordings of the same part: when it was made (ISO 8601), the measures
  * it played, how many of its notes got each verdict, and the tempo it was
  * played at overall, in the written tempo's beat.
  */
final case class ReviewSummary(
    id: String,
    score: Option[String],
    part: Option[String],
    recordedAt: String,
    fromMeasure: String,
    toMeasure: String,
    notes: Int,
    right: Int,
    wrong: Int,
    intonation: Int,
    rhythm: Int,
    unclear: Int,
    beat: Option[String],
    beatsPerMinute: Option[Double],
    tuningCents: Option[Int],
    clippedShare: Option[Double]
)

/** The recordings of a score, the latest first. */
final case class ReviewSummaries(reviews: List[ReviewSummary])

object ReviewSummaries {
  given JsonValueCodec[ReviewSummaries] = JsonCodecMaker.make
  given Schema[ReviewSummary] = Schema.derived
  given Schema[ReviewSummaries] = Schema.derived
}

/** The input volume of the server machine's microphone, from 0 to 1, and
  * whether the asker may set it: only someone on that machine, who plays into
  * its microphone, may.
  */
final case class MicrophoneLevel(volume: Double, adjustable: Boolean)

object MicrophoneLevel {
  given JsonValueCodec[MicrophoneLevel] = JsonCodecMaker.make
  given Schema[MicrophoneLevel] = Schema.derived
}

/** An ornament played in a note's time, its other notes in semitones from the
  * written one.
  */
enum Ornament {

  /** The note and the one above, from the one above, ending on the note. */
  case Trill(upper: Int)

  /** A short trill, the French "+" on a long note: the note above and the note
    * twice, then the note held.
    */
  case Tremblement(upper: Int)

  /** The note, its neighbour (below for a mordent, above for an inverted one),
    * the note again.
    */
  case Mordent(neighbour: Int)

  /** The note above, the note, the note below, the note. */
  case Turn(upper: Int, lower: Int)

  /** The note repeated every `quarters`. */
  case Tremolo(quarters: Double)
}

/** How a note is played beyond what the engraver's MIDI gives it: its velocity,
  * the share of its written length it sounds, a breath before it (the note
  * before it let go a little early: a louré, or a change of bow on a repeated
  * note), an ornament, and how its instrument's player draws it (a string
  * player's `Bowing.text`), which the engines that know it read in the MIDI;
  * and its string in a chord the bow breaks.
  */
final case class NotePlay(
    velocity: Int,
    length: Double,
    breathBefore: Boolean,
    ornament: Option[Ornament],
    technique: Option[String],
    string: ChordString
)

/** A note's place in a chord the bow breaks (`Violinist.broken`): not broken,
  * struck first and let go, held from the start (the middle of three), or
  * coming late and held.
  */
enum ChordString {
  case Whole, Lower, Middle, Upper
}

/** How the conductor has a note (by its id) played: the MIDI track it is in
  * (the first is the tempo's, then one a staff), the pitch it sounds at (as
  * MIDI counts it, a transposing instrument's transposed) and how it is played.
  */
final case class ConductedNote(
    note: String,
    track: Int,
    pitch: Int,
    play: NotePlay
)

/** That a part, counted in score order, accompanies in a measure: played softer
  * under the parts that carry the music.
  */
final case class AccompanyingMeasure(part: Int, measure: String)

/** What the conductor makes of a score: how each note is played, by its id
  * (`ScoreEditing.identify`, on the server), and where each part accompanies. A
  * list, not a map: a score's thousands of notes are more than a JSON map may
  * hold.
  */
final case class Interpretation(
    notes: List[ConductedNote],
    accompaniments: List[AccompanyingMeasure]
)

object Interpretation {
  val none: Interpretation = Interpretation(Nil, Nil)

  given JsonValueCodec[Interpretation] = JsonCodecMaker.make
  given Schema[Ornament] = Schema.derived
  given Schema[ChordString] = Schema.derived
  given Schema[NotePlay] = Schema.derived
  given Schema[ConductedNote] = Schema.derived
  given Schema[AccompanyingMeasure] = Schema.derived
  given Schema[Interpretation] = Schema.derived
}

/** A part of a score: an instrument or a voice, with the name the score gives
  * it, and the staves it is written on (a piano's two).
  */
final case class ScorePart(id: String, name: String, staves: Int = 1)

/** A time signature. */
final case class Meter(beats: Int, beatType: Int) {

  /** The beats a conductor gives a measure: a compound time is beaten in dotted
    * notes.
    */
  def beaten: Int = if (compound) beats / 3 else beats

  /** How long a beaten beat is, in quarter notes. */
  def beatQuarters: Double = 4.0 / beatType * (if (compound) 3 else 1)

  /** The beats counted in a measure, and how long each is in quarter notes, in
    * the note its tempo is given in (`tempoBeat`, in quarter notes; none given,
    * a quarter, or a dotted one in a compound time): a time in halves with a
    * tempo in quarters is counted in quarters. A beat that does not divide the
    * measure leaves it beaten as a conductor does.
    */
  def counted(tempoBeat: Option[Double]): (Int, Double) = {
    val beat = tempoBeat.getOrElse(if (compound) beatQuarters else 1.0)
    val count = beats * 4.0 / beatType / beat
    if (beat > 0 && count >= 1 && math.abs(count - math.round(count)) < 1e-6)
      (math.round(count).toInt, beat)
    else (beaten, beatQuarters)
  }

  private def compound: Boolean =
    beatType >= 8 && beats > 3 && beats % 3 == 0
}

/** The time signature in force in a measure, by the measure's number. */
final case class MeasureMeter(measure: String, meter: Meter)

/** A rest of the score, by its id: its measure's number, and where it starts in
  * the measure and how long it lasts, in quarter notes.
  */
final case class RestPlace(
    id: String,
    measure: String,
    at: Double,
    quarters: Double
)

/** A marking of a measure of a part (counted in score order): `kind` is
  * "dynamics" (its text p, f…), "wedge" (crescendo, diminuendo or stop) or
  * "words", `quarters` from the start of the measure.
  */
final case class Direction(
    part: Int,
    measure: String,
    quarters: Double,
    kind: String,
    text: String
)

/** A pitch as written: its step (C to B), alteration in semitones, octave. */
final case class WrittenPitch(step: String, alter: Int, octave: Int)

/** A measure of a part that may have been read wrong, by the score's own
  * consistency: `kind` is "too-long" or "too-short" (the beats it holds,
  * `beatsFound`, against those its time signature writes) or "tie" (a tie from
  * `tiedFrom` to notes of other pitches, `tiedTo`, none for a rest); `notes`
  * are the ids of the notes concerned.
  */
final case class MeasureCheck(
    part: Int,
    measure: String,
    kind: String,
    beatsFound: Double,
    beatsWritten: Int,
    tiedFrom: Option[WrittenPitch],
    tiedTo: List[WrittenPitch],
    notes: List[String]
)

/** What the server reads of a score for the page: its parts, the note value its
  * first metronome mark counts (in quarter notes), each measure's time
  * signature, its rests, its parts' markings and the measures to check.
  */
final case class ScoreReading(
    parts: List[ScorePart],
    beatQuarters: Option[Double],
    meters: List[MeasureMeter],
    rests: List[RestPlace],
    directions: List[Direction],
    checks: List[MeasureCheck]
) {

  /** What the score with that part alone reads: its markings as the first
    * part's.
    */
  def alone(part: Int): ScoreReading =
    copy(
      parts = parts.lift(part).toList,
      directions = directions.filter(_.part == part).map(_.copy(part = 0)),
      checks = checks.filter(_.part == part).map(_.copy(part = 0))
    )
}

object ScoreReading {
  val none: ScoreReading = ScoreReading(Nil, None, Nil, Nil, Nil, Nil)

  given JsonValueCodec[ScoreReading] = JsonCodecMaker.make
  given Schema[ScorePart] = Schema.derived
  given Schema[Meter] = Schema.derived
  given Schema[MeasureMeter] = Schema.derived
  given Schema[RestPlace] = Schema.derived
  given Schema[Direction] = Schema.derived
  given Schema[WrittenPitch] = Schema.derived
  given Schema[MeasureCheck] = Schema.derived
  given Schema[ScoreReading] = Schema.derived
}
