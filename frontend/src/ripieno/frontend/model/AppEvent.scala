package ripieno.frontend.model

import ripieno.frontend.score.*
import ripieno.shared.*

import scala.scalajs.js.typedarray.Uint8Array

sealed trait AppEvent

object AppEvent {
  final case class ScoreLoading(path: String) extends AppEvent

  /** A score is ready: its parts in score order, the MIDI that plays all of
    * them, the tempo it is written at, and its engraving with every part.
    */
  final case class ScoreOpened(
      path: String,
      parts: List[ScorePart],
      midi: Uint8Array,
      tempo: Tempo,
      engraving: Engraving
  ) extends AppEvent

  final case class ScoreUnreadable(path: String, reason: String)
      extends AppEvent

  /** What the sheet shows: the whole score, or the part `solo` alone; `revised`
    * when it is the same score as before, an edit made.
    */
  final case class ScoreShown(
      engraving: Engraving,
      solo: Option[Int],
      layoutMillis: Long,
      revised: Boolean = false
  ) extends AppEvent

  // The score's own sound is being made ready: it cannot be played yet.
  case object SoundsLoading extends AppEvent

  /** The score's own sound is ready: playback starts at once. */
  case object SoundsReady extends AppEvent

  /** How each part sounds from now on, and the parts still being rendered (the
    * sound in use plays meanwhile), with how far each is in percent once it has
    * started.
    */
  final case class SoundMixed(mix: SoundMix, rendering: Map[Int, Option[Int]])
      extends AppEvent

  /** The score's own sound plays again, after a recording played back. */
  case object ScoreSoundBack extends AppEvent

  /** The tempo played from now on, in beats a minute of the written tempo's
    * beat.
    */
  final case class TempoSet(beatsPerMinute: Int) extends AppEvent

  /** The section of a score at its own tempo where playback stands, none for a
    * score at one tempo throughout: its tempo is the one set, not `TempoSet`'s.
    */
  final case class SectionShown(section: Option[SectionTempo]) extends AppEvent

  /** Every section of a score at a tempo of its own, in order; none for a score
    * at one tempo throughout.
    */
  final case class SectionsShown(sections: List[SectionTempo]) extends AppEvent

  /** The cuts the score allows, each with whether it is made. */
  final case class CutsShown(cuts: List[(ScoreCut, Boolean)]) extends AppEvent

  /** The score's repeats, each with whether it is played. */
  final case class RepeatsShown(repeats: List[(ScoreRepeat, Boolean)])
      extends AppEvent

  /** The same score timed again, a section's tempo chosen: its MIDI, and its
    * engraving with every part, the page unchanged.
    */
  final case class ScoreRetimed(midi: Uint8Array, engraving: Engraving)
      extends AppEvent

  /** How many measures the metronome beats before playback starts. */
  final case class CountInSet(measures: Int) extends AppEvent

  /** How many measures the metronome beats where a new tempo starts. */
  final case class SectionCountInSet(measures: Int) extends AppEvent

  /** The parts heard from now on, counted in score order. */
  final case class PartsPlayed(parts: Set[Int]) extends AppEvent

  case object PlaybackStarted extends AppEvent

  case object PlaybackPaused extends AppEvent

  /** Playback is back at the start of the score. */
  case object PlaybackStopped extends AppEvent

  /** How far into the score the sound is, the notes sounding there as the sheet
    * names them, and the number of the measure.
    */
  final case class PlaybackMoved(
      seconds: Double,
      sounding: Sounding,
      measure: String
  ) extends AppEvent

  /** Whether a click on a note selects it to be edited, rather than playing
    * from it.
    */
  final case class EditingSet(on: Boolean) extends AppEvent

  /** The note selected to be edited, as it stands, or none. */
  final case class NoteSelected(note: Option[NoteDetails]) extends AppEvent

  /** The editions of the score at `path`, and the one edits go to. */
  final case class EditionsChanged(
      path: String,
      editions: List[Edition],
      active: Option[String]
  ) extends AppEvent

  /** Edits of the editions shown that found no note to change. */
  final case class EditsLost(count: Int) extends AppEvent

  /** The notes selected, or none. */
  final case class RangeSelected(range: Option[ScoreRange]) extends AppEvent

  /** The metronome counts in before the recording starts. */
  case object RecordingCountingIn extends AppEvent

  /** The microphone is recording, from that measure. */
  final case class RecordingStarted(measure: String) extends AppEvent

  /** How loud the microphone is while recording, its loudest sample from 0 to
    * 1: at 1, the sound saturates.
    */
  final case class RecordingLevel(peak: Double) extends AppEvent

  /** The level check listens, for its `attempt`th time. */
  final case class LevelChecking(attempt: Int) extends AppEvent

  /** The level check is over, and what it found or did, in words. */
  final case class LevelChecked(outcome: String) extends AppEvent

  /** Whether the metronome beats. */
  final case class MetronomeSet(on: Boolean) extends AppEvent

  /** How the metronome sounds. */
  final case class MetronomeConfigured(settings: MetronomeSettings)
      extends AppEvent

  /** Whether the tuner listens. */
  final case class TunerSet(on: Boolean) extends AppEvent

  /** The A the tuner tunes to, in hertz. */
  final case class ReferencePitchSet(hertz: Double) extends AppEvent

  /** The instrument the player chose to tune: none for the A alone. */
  final case class TunerInstrumentSet(instrument: Option[String])
      extends AppEvent

  /** The pitch the tuner hears, in hertz, a few times a second: none in
    * silence.
    */
  final case class PitchHeard(hertz: Option[Double]) extends AppEvent

  /** The part the player plays, if any, and whether it is heard too. */
  final case class PartChosen(part: Option[Int], guide: Boolean)
      extends AppEvent

  /** How the player hears the other parts. */
  final case class ListeningSet(listening: Listening) extends AppEvent

  /** The page shown from now on. */
  final case class PageShown(page: Page) extends AppEvent

  /** The library's home, as it stands. */
  final case class ShelvesLoaded(shelves: Shelves) extends AppEvent

  /** Works listed for the page: a collection's, or those a search found. */
  final case class WorksListed(page: Page, works: List[WorkSummary])
      extends AppEvent

  /** A work as it stands: on its page, or above its movements on the sheet. */
  final case class WorkLoaded(work: WorkDetail) extends AppEvent

  /** A PDF is being made of movements of a work, or is made. */
  final case class ExportProgress(working: Boolean) extends AppEvent

  /** Whether the settings of the sound show. */
  final case class SoundsShown(on: Boolean) extends AppEvent

  /** Whether the measures to check are shown. */
  final case class ChecksShown(on: Boolean) extends AppEvent

  /** The drawer of cuts and repeats is shown, or hidden. */
  final case class PassagesShown(on: Boolean) extends AppEvent

  /** The tempo drawer is shown, or hidden. */
  final case class TempiShown(on: Boolean) extends AppEvent

  /** The suspect measure shown on the sheet, if any. */
  final case class SuspectShown(suspect: Option[SuspectMeasure])
      extends AppEvent

  /** The measures of the score as it stands that may have been read wrong, less
    * those the player said are right.
    */
  final case class MeasuresSuspected(suspects: List[SuspectMeasure])
      extends AppEvent

  /** The recording is over and on its way to be reviewed. */
  case object RecordingReviewing extends AppEvent

  /** A recording reviewed, by `id`; its notes are named as in the engraving
    * shown when it was recorded.
    */
  final case class PerformanceReviewed(id: String, review: PerformanceReview)
      extends AppEvent

  /** The recordings reviewed of the score at `score`, the latest first. */
  final case class RecordingsListed(
      score: String,
      recordings: List[ReviewSummary]
  ) extends AppEvent

  /** Whether the list of the score's recordings shows beside the sheet. */
  final case class RecordingsShown(on: Boolean) extends AppEvent

  /** A reviewed recording is what plays from now on. */
  final case class RecordingPlayedBack(review: String) extends AppEvent

  final case class RequestFailed(command: AppCommand, reason: String)
      extends AppEvent
}
