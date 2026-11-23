package ripieno.backend.reading

import ripieno.shared.*

import org.w3c.dom.Document

/** A score made ready for the page, from its source: every note given its id,
  * the editions played over it, its parts given their instruments and its beams
  * closed; timed with the tempi chosen and the cuts made, and how the conductor
  * has it played. The page engraves and plays it as it comes.
  */
object ScorePreparation {

  def prepared(source: String, request: ScoreRequest): PreparedScore = {
    val document = MusicXmlDocument.parsed(source)
    val lost = edited(document, request)
    PreparedScore(
      MusicXmlDocument.serialized(document),
      lost,
      timed(document, request),
      Conductor.interpretation(document),
      ScoreReader.reading(document)
    )
  }

  def timing(source: String, request: ScoreRequest): ScoreTiming = {
    val document = MusicXmlDocument.parsed(source)
    edited(document, request)
    timed(document, request)
  }

  // the score as the page engraves it; the edits that found no note
  private def edited(document: Document, request: ScoreRequest) = {
    ScoreEditing.identify(document)
    val lost = ScoreEditing.edit(document, request.editions)
    ScoreCleaning.instrument(document)
    ScoreCleaning.closeBeams(document)
    lost
  }

  private def timed(document: Document, request: ScoreRequest): ScoreTiming =
    TempoReading.timing(
      document,
      request.chosen.map(set => set.section -> set.quarterNotes.toDouble).toMap,
      request.cuts.toSet
    )
}
