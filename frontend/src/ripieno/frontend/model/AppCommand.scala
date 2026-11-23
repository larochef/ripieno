package ripieno.frontend.model

import ripieno.shared.*

sealed trait AppCommand

object AppCommand {

  final case class OpenScore(path: String) extends AppCommand

  /** One part of the opened score alone, or the whole score. */
  final case class ShowPart(part: Option[Int]) extends AppCommand

  case object TogglePlayback extends AppCommand

  case object StopPlayback extends AppCommand

  /** Plays at that many beats a minute, the beat being the one the score's
    * tempo counts.
    */
  final case class SetTempo(beatsPerMinute: Int) extends AppCommand

  /** Plays a section of the score (`ScoreTiming.sections`, counted from 0) at
    * that many beats a minute of the score's beat, from now on.
    */
  final case class SetSectionTempo(section: Int, beatsPerMinute: Int)
      extends AppCommand

  /** Plays sections of the score at those tempi, by section, in beats a minute
    * of the score's beat, all at once.
    */
  final case class SetSectionTempi(tempi: Map[Int, Int]) extends AppCommand

  /** Makes a cut the score allows (`ScoreTiming.cuts`, counted from 0), or
    * plays the passage again.
    */
  final case class SetCut(cut: Int, made: Boolean) extends AppCommand

  /** Plays a repeat of the score (`Engraving.repeats`, counted from 0), or
    * leaves it out, with its first ending.
    */
  final case class SetRepeat(repeat: Int, played: Boolean) extends AppCommand

  /** Plays those parts of the score alone, counted in score order. */
  final case class PlayParts(parts: Set[Int]) extends AppCommand

  /** Moves playback to the start of a measure, by its number. */
  final case class PlayFromMeasure(number: String) extends AppCommand

  /** Has the metronome beat that many measures before playback starts. */
  final case class SetCountIn(measures: Int) extends AppCommand

  /** Has playback stop where a section of the score at a tempo of its own
    * starts, and the metronome beat that many of its measures before it carries
    * on: none for no stop.
    */
  final case class SetSectionCountIn(measures: Int) extends AppCommand

  /** Selects the notes from one to another, by their ids in the sheet, in
    * whichever order they were clicked.
    */
  final case class SelectRange(from: String, to: String) extends AppCommand

  case object ClearRange extends AppCommand

  /** Plays the notes selected and stops after them: as the score writes them,
    * or as the recording opened played them; with the metronome's beats over
    * them if `beats`.
    */
  final case class PlayRange(asPlayed: Boolean, beats: Boolean)
      extends AppCommand

  /** Moves playback to a note, named as it is in the sheet's SVG. */
  final case class PlayFromNote(note: String) extends AppCommand

  /** Clicks on notes select them to be edited, or play from them again. */
  final case class SetEditing(on: Boolean) extends AppCommand

  /** Selects a note to edit, by its id in the sheet, or none. */
  final case class SelectNote(id: Option[String]) extends AppCommand

  /** Adds an edit to the edition edits go to; there is a first one, made when
    * needed: "Corrections".
    */
  final case class ApplyEdit(edit: Edit) extends AppCommand

  /** Takes back the last edit of the edition edits go to. */
  case object UndoEdit extends AppCommand

  /** A new edition, which edits go to from now on. */
  final case class NewEdition(name: String) extends AppCommand

  /** Shows an edition over the score, or hides it. */
  final case class ShowEdition(name: String, visible: Boolean)
      extends AppCommand

  /** The edition edits go to from now on; it is shown. */
  final case class UseEdition(name: String) extends AppCommand

  /** Records the player from where playback is, after the count-in, to review
    * their playing of the part shown alone.
    */
  case object StartRecording extends AppCommand

  /** Shows the list of the score's recordings beside the sheet, or hides it. */
  final case class ShowRecordings(on: Boolean) extends AppCommand

  /** Opens the review of an earlier recording, by its id. */
  final case class OpenRecording(id: String) extends AppCommand

  /** Throws an earlier recording and its review away. */
  final case class DeleteRecording(id: String) extends AppCommand

  /** Listens to the player's loudest passage for a few seconds and sets the
    * microphone's input volume from it, where the server can.
    */
  case object CheckLevel extends AppCommand

  /** Starts or stops the metronome: with the music while it plays, on its own
    * otherwise, at the tempo chosen and in the time of the measure where the
    * score stands.
    */
  final case class SetMetronome(on: Boolean) extends AppCommand

  /** How the metronome sounds from now on. */
  final case class ConfigureMetronome(settings: MetronomeSettings)
      extends AppCommand

  /** Starts or stops listening for the pitch played, to tune. */
  final case class SetTuner(on: Boolean) extends AppCommand

  /** The A the tuner tunes to, in hertz. */
  final case class SetReferencePitch(hertz: Double) extends AppCommand

  /** The instrument whose open strings the tuner sounds: none for its A alone.
    */
  final case class SetTunerInstrument(instrument: Option[String])
      extends AppCommand

  /** Sounds a note, a MIDI pitch, at the reference pitch's tuning. */
  final case class PlayReferenceNote(pitch: Int) extends AppCommand

  /** Starts the parts heard with the recording, which starts now. */
  case object PlayAlongNow extends AppCommand

  /** Records the part the player plays, the one played last when none is
    * chosen.
    */
  case object RecordMyPart extends AppCommand

  /** The part the player plays, shown alone while the others are heard; none to
    * see and hear the whole score.
    */
  final case class ChoosePart(part: Option[Int]) extends AppCommand

  /** Whether the player's own part is heard too, as a guide. */
  final case class SetGuide(on: Boolean) extends AppCommand

  /** How the player hears the other parts. */
  final case class SetListening(listening: Listening) extends AppCommand

  /** Shows another page: the library, a work, movements on the sheet. With
    * `replace`, the browser's history keeps no trace of the page left.
    */
  final case class Navigate(page: Page, replace: Boolean = false)
      extends AppCommand

  /** Gives a part of a work another name. */
  final case class RenamePart(work: Long, part: Long, name: String)
      extends AppCommand

  /** Makes a work of the player's own of score files. */
  final case class ImportScores(scores: ScoreImport) extends AppCommand

  /** Makes a PDF of those movements of a work, its parts laid out as `layout`
    * says, the visible editions played over them if `edited`, the music a
    * little smaller where that saves a page if `compact`.
    */
  final case class ExportPdf(
      work: WorkDetail,
      movements: List[Long],
      layout: ExportLayout,
      edited: Boolean,
      compact: Boolean
  ) extends AppCommand

  /** Shows the settings of the sound, or hides them. */
  final case class ShowSounds(on: Boolean) extends AppCommand

  /** Shows the score's cuts and repeats, or hides them. */
  final case class ShowPassages(on: Boolean) extends AppCommand

  /** Shows the tempo of the score, or of each of its sections, or hides it. */
  final case class ShowTempi(on: Boolean) extends AppCommand

  /** Shows or hides the measures to check. */
  final case class ShowChecks(on: Boolean) extends AppCommand

  /** Shows a suspect measure on the sheet, or none. */
  final case class ShowSuspect(suspect: Option[SuspectMeasure])
      extends AppCommand

  /** The player looked at a suspect measure: it is right as it stands. */
  final case class DismissSuspect(suspect: SuspectMeasure) extends AppCommand

  /** Ends the recording and has it reviewed. */
  case object StopRecording extends AppCommand

  /** Plays back a reviewed recording, the sheet following it. */
  final case class ListenToRecording(
      review: String,
      alignment: List[AlignmentPoint]
  ) extends AppCommand

  /** Plays the score with that engine from now on, its instruments too in every
    * score; a rendering not made yet is asked for, and used once ready.
    */
  final case class UseSound(engine: SoundEngine) extends AppCommand

  /** Plays every part of that instrument with that engine from now on, in every
    * score; `None` stands for the parts of no known instrument.
    */
  final case class UseInstrumentSound(
      instrument: Option[String],
      engine: SoundEngine
  ) extends AppCommand

  /** One part's level from now on, from 0 to 100. */
  final case class SetPartLevel(part: Int, level: Int) extends AppCommand

  /** Back from a recording played back to the score's own sound. */
  case object BackToScoreSound extends AppCommand
}
