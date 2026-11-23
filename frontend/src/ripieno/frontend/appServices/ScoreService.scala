package ripieno.frontend.appServices

import ripieno.frontend.api.ApiClient
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.*
import ripieno.frontend.utils.*
import ripieno.shared.*

import scala.scalajs.js

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** Opens scores and engraves what the sheet shows of them, as the server
  * prepares them: their editions played over them, timed with the tempi chosen
  * and the cuts made.
  *
  * A score is asked for once its editions and its tempi are there, and again
  * each time its editions change: the sheet stays where it was.
  */
class ScoreService(bus: AppBus, apiClient: ApiClient, verovio: js.Any) {

  // Two engravings, so that going back to the whole score has nothing to lay
  // out again.
  private val wholeScore = Engraving(verovio)
  private val singlePart = Engraving(verovio)

  private var path = ""
  private var editions: Option[List[Edition]] = None
  // the tempo each section is played at, kept beside the score (quarter notes
  // a minute, by section)
  private var tempi: Option[Map[Int, Int]] = None
  private var engraved = false
  // the score as engraved: the source with its editions
  private var musicXml = ""
  // how the server last timed it, and what else it read of it
  private var timing = ScoreTiming(Nil, Nil, Nil)
  private var reading = ScoreReading.none
  // the last score asked for, and the last timing: an answer to an earlier
  // question comes too late
  private var asked = 0
  private var parts: List[ScorePart] = Nil
  private var solo: Option[Int] = None
  private var selected: Option[String] = None
  // the score's suspect measures, the player's verdicts aside
  private var suspects: List[SuspectMeasure] = Nil

  bus.commands.events.foreach {
    case OpenScore(opened)        => open(opened)
    case command @ ShowPart(part) => showPart(command, part)
    case SelectNote(id)           => select(id)
    case DismissSuspect(suspect)  =>
      DismissedSuspects.add(path, suspect.key)
      publishSuspects()
      bus.publish(SuspectShown(None))
    case ShowSuspect(suspect) => bus.publish(SuspectShown(suspect))
    case SetSectionTempo(section, beatsPerMinute) =>
      setSectionTempi(Map(section -> beatsPerMinute))
    case SetSectionTempi(chosenNow) => setSectionTempi(chosenNow)
    case SetCut(cut, made)          => setCut(cut, made)
    case SetRepeat(repeat, played)  => setRepeat(repeat, played)
    case _                          => ()
  }(using unsafeWindowOwner)

  bus.events.events.foreach {
    case EditionsChanged(changed, now, _) if changed == path =>
      editions = Some(now)
      prepare()
    case _ => ()
  }(using unsafeWindowOwner)

  private def open(opened: String): Unit = {
    path = opened
    editions = None
    tempi = None
    engraved = false
    solo = None
    selected = None
    bus.publish(ScoreLoading(opened))
    // none to be had is none chosen: the score plays as written
    AppStreams.runOnce(apiClient.tempi(opened)) { answer =>
      if (opened == path) {
        tempi = Some(
          answer.fold(
            _ => Map.empty,
            _.sections.map(set => set.section -> set.quarterNotes).toMap
          )
        )
        prepare()
      }
    }
  }

  // What the server is asked for the score: with its editions, the tempi
  // chosen and the cuts made.
  private def request(played: List[Edition]): ScoreRequest =
    ScoreRequest(
      path,
      played,
      tempi.getOrElse(Map.empty).toList.sorted.map(TempoSetting(_, _)),
      RememberedTempos.cutsMade(path).toList.sorted
    )

  // Once its editions and its tempi are there, the score as the server
  // prepares it.
  private def prepare(): Unit = {
    for {
      played <- editions
      _ <- tempi
    } {
      asked += 1
      val question = asked
      val opened = path
      AppStreams.runOnce(apiClient.prepareScore(request(played))) {
        case _ if question != asked || opened != path => ()
        case Left(reason)    => bus.publish(ScoreUnreadable(opened, reason))
        case Right(prepared) => engrave(prepared)
      }
    }
  }

  private def engrave(prepared: PreparedScore): Unit = {
    timing = prepared.timing
    reading = prepared.reading
    val text = prepared.musicXml
    val (result, layoutMillis) =
      timed(
        wholeScore.load(
          text,
          prepared.timing,
          prepared.interpretation,
          prepared.reading,
          RememberedTempos.skippedRepeats(path)
        )
      )
    result match {
      case Left(reason) =>
        bus.publish(
          ScoreUnreadable(
            path,
            Tr(
              s"Could not engrave $path: $reason",
              s"Impossible de graver $path : $reason"
            )
          )
        )
      case Right(()) =>
        val revised = engraved
        engraved = true
        musicXml = text
        parts = prepared.reading.parts
        suspects = MeasureChecks.suspects(prepared.reading.checks)
        publishSuspects()
        bus.publish(
          ScoreOpened(
            path,
            parts,
            wholeScore.midi,
            writtenTempo,
            wholeScore
          )
        )
        publishCuts()
        if (prepared.lost.nonEmpty) {
          bus.publish(EditsLost(prepared.lost.size))
        }
        solo.filter(_ => revised) match {
          case Some(index) =>
            showPart(ShowPart(Some(index)), Some(index), revised = true)
          case None =>
            bus.publish(
              ScoreShown(wholeScore, solo = None, layoutMillis, revised)
            )
        }
        selected.foreach(id => select(Some(id)))
    }
  }

  private def showPart(
      command: ShowPart,
      part: Option[Int],
      revised: Boolean = false
  ): Unit = {
    solo = part
    part match {
      case None =>
        bus.publish(
          ScoreShown(wholeScore, solo = None, layoutMillis = 0, revised)
        )
      case Some(index) =>
        parts.lift(index).foreach { part =>
          val (engraved, layoutMillis) =
            timed(
              // laid out alone, timed as the whole score: what this part
              // leaves silent goes on all the same
              singlePart
                .load(
                  MusicXml.only(musicXml, part),
                  reading = reading.alone(index)
                )
                .map(_ => singlePart.retimeAs(wholeScore))
            )
          bus.publish(
            engraved.fold(
              reason =>
                RequestFailed(
                  command,
                  Tr(
                    s"Could not engrave ${part.name}: $reason",
                    s"Impossible de graver ${part.name} : $reason"
                  )
                ),
              _ =>
                ScoreShown(
                  singlePart,
                  solo = Some(index),
                  layoutMillis,
                  revised
                )
            )
          )
        }
    }
  }

  private def publishSuspects(): Unit = {
    val dismissed = DismissedSuspects.of(path)
    bus.publish(
      MeasuresSuspected(suspects.filterNot(s => dismissed.contains(s.key)))
    )
  }

  // The note as it stands now, editions played: the same id whichever
  // engraving shows it, since the ids come from the source.
  private def select(id: Option[String]): Unit = {
    selected = id
    bus.publish(NoteSelected(id.flatMap(ScoreEdits.details(musicXml, _))))
  }

  // Sections played at other tempi, in beats a minute of the score's beat:
  // kept beside the score, and the score timed again, its page as it is.
  private def setSectionTempi(chosenNow: Map[Int, Int]): Unit = {
    if (engraved) {
      val sections = wholeScore.sections
      val now = chosenNow.foldLeft(tempi.getOrElse(Map.empty)) {
        case (sofar, (section, beatsPerMinute)) =>
          sections.find(_.index == section).fold(sofar) { found =>
            val quarters = math.round(beatsPerMinute * beat.quarters).toInt
            // the tempo written, set again, is no tempo kept
            if (
              found.written.exists(written => math.round(written) == quarters)
            )
              sofar - section
            else sofar.updated(section, quarters)
          }
      }
      tempi = Some(now)
      val kept = ScoreTempi(
        now.toList.sorted.map((section, quarters) =>
          TempoSetting(section, quarters)
        )
      )
      AppStreams.runOnce(apiClient.saveTempi(path, kept)) {
        case Left(reason) =>
          bus.publish(
            RequestFailed(
              SetSectionTempi(chosenNow),
              Tr(
                s"The tempi could not be kept: $reason",
                s"Les tempos n'ont pas pu être enregistrés : $reason"
              )
            )
          )
        case Right(_) => ()
      }
      retimed()
    }
  }

  private def setCut(cut: Int, made: Boolean): Unit = {
    if (engraved && wholeScore.cuts.exists(_.index == cut)) {
      RememberedTempos.setCut(path, cut, made)
      retimed()
    }
  }

  private def setRepeat(repeat: Int, played: Boolean): Unit = {
    if (engraved && wholeScore.repeats.exists(_.index == repeat)) {
      RememberedTempos.setRepeat(path, repeat, played)
      retimedAs(timing)
    }
  }

  // The score timed again by the server, other tempi chosen or other cuts
  // made, its page as it is.
  private def retimed(): Unit = {
    asked += 1
    val question = asked
    val opened = path
    AppStreams.runOnce(
      apiClient.timeScore(request(editions.getOrElse(Nil)))
    ) {
      case _ if question != asked || opened != path => ()
      case Left(reason)                             =>
        bus.publish(
          RequestFailed(
            SetSectionTempi(Map.empty),
            Tr(
              s"The score could not be timed again: $reason",
              s"La partition n'a pas pu être minutée à nouveau : $reason"
            )
          )
        )
      case Right(now) => retimedAs(now)
    }
  }

  // The score timed as the server last said, the repeats left out not played.
  private def retimedAs(now: ScoreTiming): Unit = {
    timing = now
    wholeScore.retime(now, RememberedTempos.skippedRepeats(path))
    if (solo.isDefined) singlePart.retimeAs(wholeScore)
    bus.publish(ScoreRetimed(wholeScore.midi, wholeScore))
    publishCuts()
  }

  private def publishCuts(): Unit = {
    bus.publish(
      CutsShown(
        wholeScore.cuts.map(cut => cut -> RememberedTempos.cut(path, cut.index))
      )
    )
    val skipped = RememberedTempos.skippedRepeats(path)
    bus.publish(
      RepeatsShown(
        wholeScore.repeats.map(repeat =>
          repeat -> !skipped.contains(repeat.index)
        )
      )
    )
  }

  // The note the score's metronome mark counts, a quarter if it has none.
  private def beat: Beat =
    reading.beatQuarters
      .flatMap(quarters =>
        Beat.values.find(each => math.abs(each.quarters - quarters) < 1e-9)
      )
      .getOrElse(Beat.Quarter)

  // In the beat of the score's metronome mark: Verovio counts quarter notes.
  private def writtenTempo: Tempo =
    Tempo(
      beat,
      math.max(1, math.round(wholeScore.tempo / beat.quarters).toInt)
    )

  private def timed[A](work: => A): (A, Long) = {
    val started = dom.window.performance.now()
    val result = work
    (result, math.round(dom.window.performance.now() - started))
  }
}
