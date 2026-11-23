package ripieno.frontend.score

import ripieno.frontend.Libraries.VerovioToolkit
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.AppResult
import ripieno.shared.*

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.*

import org.scalajs.dom

/** What sounds at a moment of a score: the page it is on and its notes, by the
  * identifiers they have in that page's SVG, and the rests being kept then.
  */
final case class Sounding(
    page: Int,
    notes: Seq[String],
    rests: Seq[String] = Seq.empty
) {

  /** The notes and the rests, all that is played then. */
  def played: Seq[String] = notes ++ rests
}

object Sounding {
  val silence: Sounding = Sounding(page = 0, notes = Seq.empty)
}

/** A section of a score at its own tempo (`ScoreTiming.sections`): its tempo as
  * written, if it is, and as played, in quarter notes a minute, from its start
  * in milliseconds as played.
  */
final case class PlayedSection(
    index: Int,
    name: Option[String],
    written: Option[Double],
    chosen: Double,
    start: Double
)

/** A measure as it is played, in milliseconds: a repeated one is played once
  * for each time through. Its time signature has `beats` beats a measure, a
  * conductor's, of `beatMillis` each: a pickup is shorter than its beats. `id`
  * is its element's in the engraving's pages.
  */
final case class Bar(
    number: String,
    start: Double,
    end: Double,
    beats: Int,
    beatMillis: Double,
    quarterMillis: Double,
    id: String
)

/** A repeat of the score, as Verovio plays it: the measures played again, by
  * their numbers, and the first ending's, if it has one, which is left out when
  * the repeat is.
  */
final case class ScoreRepeat(
    index: Int,
    from: String,
    to: String,
    firstEnding: Option[(String, String)]
)

/** A score laid out by Verovio: its pages as SVG, the MIDI that plays it and
  * which notes sound when. All three come from the one layout, so what is heard
  * and what is shown cannot disagree; repeats are unfolded in both.
  */
final class Engraving(verovio: js.Any) {
  private val toolkit = new VerovioToolkit(verovio).asInstanceOf[js.Dynamic]
  toolkit.setOptions(
    js.Dynamic.literal(
      // the server hands out nothing else, whatever the file was
      inputFrom = "musicxml",
      pageWidth = Engraving.pageWidth,
      pageHeight = Engraving.pageHeight,
      scale = 40,
      breaks = "auto",
      // every measure numbered, so that any can be found and asked for
      mnumInterval = 1,
      adjustPageHeight = true,
      svgViewBox = true,
      footer = "none"
    )
  )

  // when each note first sounds, in milliseconds
  private var onsets: Map[String, Double] = Map.empty
  // every time a note sounds, in the order played: a repeated one more than once
  private var played: Vector[(String, Double)] = Vector.empty
  private var markings: List[Marking] = Nil
  // every time a note sounds, in quarter notes from the start, as the MIDI counts
  private var playedQuarters: Vector[(String, Double)] = Vector.empty
  private var interpretation = Interpretation.none
  // how the conductor has each note played, by its id
  private var conductedNotes: Map[String, ConductedNote] = Map.empty
  private var startTempo = Engraving.defaultTempo
  // where the score's words stretch Verovio's time (`TempoChanges`): from a time of
  // Verovio's in milliseconds, and from a quarter note, how many times slower
  private var stretches: Vector[(Double, Double)] = Vector.empty
  private var quarterStretches: Vector[(Double, Double)] = Vector.empty
  private var bars: Vector[Bar] = Vector.empty
  // each rest's first onset and end, in milliseconds as played
  private var restSpans: Map[String, (Double, Double)] = Map.empty
  private var sectionsPlayed: Vector[PlayedSection] = Vector.empty
  // the first note of each section after the first, as the server reads them
  private var sectionStarts: Vector[String] = Vector.empty
  private var cutsFound: List[ScoreCut] = Nil
  private var repeatsFound: List[ScoreRepeat] = Nil
  // what leaving out each repeat takes away, in Verovio's quarter notes: its
  // first ending and the measures played again
  private var repeatSpans: Map[Int, (Double, Double)] = Map.empty
  // the measures as Verovio plays them, written tempo, cuts made
  private var writtenBars: Vector[Bar] = Vector.empty
  // what the server read of the score loaded, and Verovio's times for it, to
  // time it again
  private var read = ScoreReading.none
  private var verovioTimes: String = "[]"

  /** Replaces what was engraved, timed as the server reads the score (its
    * sections at their tempi, the cuts made: `timing`), played as its conductor
    * has it (`conducted`), what else the server read of it (`reading`), and the
    * repeats `skipped` left out; a score Verovio cannot read leaves nothing.
    */
  def load(
      musicXml: String,
      timing: ScoreTiming = ScoreTiming(Nil, Nil, Nil),
      conducted: Interpretation = Interpretation.none,
      reading: ScoreReading = ScoreReading.none,
      skipped: Set[Int] = Set.empty
  ): AppResult[Unit] = {
    onsets = Map.empty
    played = Vector.empty
    playedQuarters = Vector.empty
    interpretation = Interpretation.none
    conductedNotes = Map.empty
    markings = Nil
    startTempo = Engraving.defaultTempo
    stretches = Vector.empty
    quarterStretches = Vector.empty
    bars = Vector.empty
    sectionsPlayed = Vector.empty
    sectionStarts = Vector.empty
    cutsFound = Nil
    repeatsFound = Nil
    repeatSpans = Map.empty
    writtenBars = Vector.empty
    restSpans = Map.empty
    read = ScoreReading.none
    verovioTimes = "[]"
    Try(js.DynamicImplicits.truthValue(toolkit.loadData(musicXml))) match {
      case Success(true) =>
        // Verovio only answers questions of time once it has worked the times out
        verovioTimes = js.JSON.stringify(
          toolkit.renderToTimemap(js.Dynamic.literal(includeMeasures = true))
        )
        read = reading
        interpretation = conducted
        conductedNotes = conducted.notes.map(note => note.note -> note).toMap
        findRepeats()
        retime(timing, skipped)
        Right(())
      case Success(false) =>
        Left(
          Tr(
            "the engraver could not read it",
            "le moteur de gravure n'a pas pu la lire"
          )
        )
      case Failure(error) => Left(error.toString)
    }
  }

  /** The score loaded timed again, as the server reads it with other tempi
    * chosen or other cuts made (`timing`), the repeats `skipped` not played:
    * the page stays as it is.
    */
  def retime(timing: ScoreTiming, skipped: Set[Int] = Set.empty): Unit = {
    cutsFound = timing.cuts
    timed(
      Engraving.passedOver(
        changesAt(verovioMoments, timing.changes),
        skipped.toList.flatMap(repeatSpans.get)
      )
    )
    sectionStarts = timing.sections.drop(1).map(_.firstNote).toVector
    sectionsPlayed = timing.sections.toVector.flatMap { section =>
      onsets
        .get(section.firstNote)
        .map(start =>
          PlayedSection(
            section.index,
            section.name,
            section.written,
            section.played,
            start
          )
        )
    }
  }

  /** Timed as `whole`, the whole score this engraving shows a part of: one
    * score, one time, whatever this part plays or leaves silent.
    */
  def retimeAs(whole: Engraving): Unit = {
    timed(whole.quarterStretches)
    sectionsPlayed = whole.sectionsPlayed
    sectionStarts = whole.sectionStarts
    cutsFound = whole.cutsFound
    repeatsFound = whole.repeatsFound
  }

  // Each time Verovio plays measures again (their ids the source's with
  // `-rend2` after them), a repeat; the measures before, played once only,
  // are its first ending.
  private def findRepeats(): Unit = {
    val timemap = verovioMoments
    val starts = timemap.toVector.flatMap(moment =>
      moment.measureOn
        .asInstanceOf[js.UndefOr[String]]
        .toOption
        .map(_ -> moment.qstamp.asInstanceOf[Double])
    )
    val end = timemap.lastOption.fold(0.0)(_.qstamp.asInstanceOf[Double])
    def again(index: Int) =
      Engraving.source(starts(index)._1) != starts(index)._1
    def startAt(index: Int) = starts.lift(index).fold(end)(_._2)
    def number(index: Int) = toolkit
      .getElementAttr(Engraving.source(starts(index)._1))
      .n
      .asInstanceOf[js.UndefOr[String]]
      .getOrElse("")
    val runs = starts.indices
      .filter(index => again(index) && (index == 0 || !again(index - 1)))
      .map(first =>
        first -> starts.indices
          .drop(first)
          .takeWhile(again)
          .lastOption
          .fold(first)(_ + 1)
      )
    val found = runs.zipWithIndex.map { case ((first, after), index) =>
      val replayed =
        (first until after).map(at => Engraving.source(starts(at)._1)).toSet
      val ending = Iterator
        .iterate(first)(_ - 1)
        .drop(1)
        .takeWhile(at =>
          at >= 0 && !again(at) && !replayed.contains(starts(at)._1)
        )
        .toList
        .reverse
      val repeat = ScoreRepeat(
        index,
        number(first),
        number(after - 1),
        ending.headOption.map(start => number(start) -> number(ending.last))
      )
      (
        repeat,
        index -> (startAt(ending.headOption.getOrElse(first)), startAt(after))
      )
    }
    repeatsFound = found.map(_._1).toList
    repeatSpans = found.map(_._2).toMap
  }

  // Verovio's moments, before they are stretched
  private def verovioMoments: js.Array[js.Dynamic] =
    js.JSON.parse(verovioTimes).asInstanceOf[js.Array[js.Dynamic]]

  // Every time of the score from Verovio's, stretched from each quarter note
  // where the music goes slower or faster (`quarterChanges`): its notes, its
  // measures and their beats.
  private def timed(quarterChanges: Vector[(Double, Double)]): Unit = {
    val timemap = verovioMoments
    // the measures as Verovio plays them, before they are stretched
    val written = playedBars(
      verovioMoments,
      read.meters.map(each => each.measure -> each.meter).toMap
    )
    quarterStretches = quarterChanges
    stretches = quarterChanges.map((quarters, times) =>
      millisAt(timemap, quarters) -> times
    )
    if (stretches.nonEmpty) {
      // the moments of a cut made are not played: their notes sound nowhere
      timemap.foreach { moment =>
        if (slowerAt(moment.tstamp.asInstanceOf[Double]) == 0) {
          moment.updateDynamic("on")(js.Array[String]())
        }
      }
      timemap.foreach(moment =>
        moment.updateDynamic("tstamp")(
          TempoChanges.stretched(stretches)(moment.tstamp.asInstanceOf[Double])
        )
      )
    }
    startTempo = timemap.headOption
      .flatMap(_.tempo.asInstanceOf[js.UndefOr[Double]].toOption)
      .getOrElse(Engraving.defaultTempo)
    onsets = timemap.foldRight(Map.empty[String, Double]) {
      (moment, laterOnsets) =>
        val starting = moment.on
          .asInstanceOf[js.UndefOr[js.Array[String]]]
          .getOrElse(js.Array())
        laterOnsets ++ starting.map(_ -> moment.tstamp.asInstanceOf[Double])
    }
    played = timemap.toVector.flatMap { moment =>
      moment.on
        .asInstanceOf[js.UndefOr[js.Array[String]]]
        .fold(Vector.empty[String])(_.toVector)
        .map(_ -> moment.tstamp.asInstanceOf[Double])
    }
    playedQuarters = timemap.toVector.flatMap { moment =>
      moment.on
        .asInstanceOf[js.UndefOr[js.Array[String]]]
        .fold(Vector.empty[String])(_.toVector)
        .map(_ -> moment.qstamp.asInstanceOf[Double])
    }
    // each measure where it is played, at the tempo it starts at; one cut is
    // not played at all
    writtenBars = written
    bars = written.flatMap { bar =>
      // a measure a cut starts in is played at the tempo it ends at
      val slower = Some(slowerAt(bar.start))
        .filter(_ > 0)
        .getOrElse(slowerAt(bar.end - 1))
      Option.when(slower > 0 || stretched(bar.end) > stretched(bar.start))(
        bar.copy(
          start = stretched(bar.start),
          end = stretched(bar.end),
          beatMillis = bar.beatMillis * math.max(slower, 1e-3),
          quarterMillis = bar.quarterMillis * math.max(slower, 1e-3)
        )
      )
    }
    markings = placed(read.directions.filter(_.part == 0))
    // the rests, timed as the notes: Verovio times notes only
    val restsPlayed =
      read.rests.flatMap { case RestPlace(id, measure, at, quarters) =>
        written.filter(_.number == measure).flatMap { bar =>
          val from = bar.start + at * bar.quarterMillis
          Option.when(stretches.isEmpty || slowerAt(from) > 0)(
            (
              id,
              stretched(from),
              stretched(from + quarters * bar.quarterMillis)
            )
          )
        }
      }
    restSpans = restsPlayed
      .groupBy(_._1)
      .view
      .mapValues(spans => spans.minBy(_._2))
      .map((id, span) => id -> (span._2, span._3))
      .toMap
    onsets = onsets ++ restSpans.view.mapValues(_._1)
    played = (played ++ restsPlayed.map((id, onset, _) => id -> onset))
      .sortBy(_._2)
  }

  /** The sections of the score at their own tempo, in the order played: a score
    * timed by them, even by one alone (its tempo may not be the engraver's);
    * none for a score whose words start no part.
    */
  def sections: Vector[PlayedSection] = sectionsPlayed

  /** The passages the score allows to cut, in order. */
  def cuts: List[ScoreCut] = cutsFound

  /** The score's repeats, in the order played. */
  def repeats: List[ScoreRepeat] = repeatsFound

  /** The section played at that time. */
  def sectionAt(milliseconds: Double): Option[PlayedSection] =
    sections.findLast(_.start <= milliseconds).orElse(sections.headOption)

  // Verovio's times stretched where the score's words change the tempo, which
  // Verovio does not play within a measure; the MIDI is stretched alike
  // a time of Verovio's as played, and how many times slower than written the
  // score goes there
  private def stretched(millis: Double): Double =
    if (stretches.isEmpty) millis else TempoChanges.stretched(stretches)(millis)

  private def slowerAt(millis: Double): Double =
    stretches.filter(_._1 <= millis + 1e-6).lastOption.fold(1.0)(_._2)

  // The quarter notes where the music goes slower or faster, by how many
  // times than written, from TempoChanges' changes: each at the quarter its
  // note is played at (once for each time a repeat plays it), less how far
  // before that note the change is.
  private def changesAt(
      timemap: js.Array[js.Dynamic],
      changes: List[TimeChange]
  ): Vector[(Double, Double)] = {
    val byNote =
      changes.groupMap(_.note)(change => (change.slower, change.before))
    timemap.toVector
      .flatMap { moment =>
        moment.on
          .asInstanceOf[js.UndefOr[js.Array[String]]]
          .fold(Vector.empty[String])(_.toVector)
          .flatMap(id => byNote.getOrElse(Engraving.source(id), Nil))
          .distinct
          .map((times, before) =>
            (moment.qstamp.asInstanceOf[Double] - before) -> times
          )
      }
      .distinct
      .sortBy(_._1)
  }

  // Verovio's time at a quarter note, between its moments either side
  private def millisAt(
      timemap: js.Array[js.Dynamic],
      quarters: Double
  ): Double = {
    val moments = timemap.toVector.map(moment =>
      (
        moment.qstamp.asInstanceOf[Double],
        moment.tstamp.asInstanceOf[Double]
      )
    )
    moments.lastIndexWhere(_._1 <= quarters + 1e-6) match {
      case -1    => moments.headOption.fold(0.0)(_._2)
      case index =>
        val (fromQuarters, fromMillis) = moments(index)
        moments.lift(index + 1).fold(fromMillis) { (toQuarters, toMillis) =>
          if (toQuarters <= fromQuarters) fromMillis
          else
            fromMillis + (toMillis - fromMillis) *
              (quarters - fromQuarters) / (toQuarters - fromQuarters)
        }
    }
  }

  // how long a note lasts as played, in milliseconds, from its onset as played
  private def lasting(id: String, onset: Double): Double = {
    val written = toolkit
      .getMIDIValuesForElement(id)
      .duration
      .asInstanceOf[js.UndefOr[Double]]
      .getOrElse(0.0)
    if (stretches.isEmpty) written
    else
      TempoChanges.stretched(stretches)(
        TempoChanges.unstretched(stretches)(onset) + written
      ) - onset
  }

  private def playedBars(
      timemap: js.Array[js.Dynamic],
      meters: Map[String, Meter]
  ): Vector[Bar] = {
    // the note the score's tempo is given in, in quarter notes, its beats
    // counted in it
    val tempoBeat = read.beatQuarters
    // where each measure starts, in milliseconds and in quarter notes
    val starts = timemap.toVector.flatMap { moment =>
      moment.measureOn
        .asInstanceOf[js.UndefOr[String]]
        .toOption
        .map(id =>
          (
            id,
            moment.tstamp.asInstanceOf[Double],
            moment.qstamp.asInstanceOf[Double]
          )
        )
    }
    val end = timemap.lastOption.fold((0.0, 0.0))(moment =>
      (moment.tstamp.asInstanceOf[Double], moment.qstamp.asInstanceOf[Double])
    )
    val nexts = starts.drop(1).map((_, millis, quarters) => (millis, quarters))
    starts.zip(nexts :+ end).map {
      case ((id, start, startQuarter), (next, nextQuarter)) =>
        val number = toolkit
          .getElementAttr(Engraving.source(id))
          .n
          .asInstanceOf[js.UndefOr[String]]
          .getOrElse("")
        val meter = meters.getOrElse(number, Meter(4, 4))
        val (count, beatQuarters) = meter.counted(tempoBeat)
        val quarterMillis =
          if (nextQuarter > startQuarter)
            (next - start) / (nextQuarter - startQuarter)
          else 60000 / startTempo
        Bar(
          number,
          start,
          next,
          count,
          beatQuarters * quarterMillis,
          quarterMillis,
          Engraving.source(id)
        )
    }
  }

  // Markings at their times, once for each time their measure is played; a
  // wedge lasts until the next wedge's end.
  private def placed(directions: List[Direction]): List[Marking] = {
    val timed = directions
      .flatMap(direction =>
        bars
          .filter(_.number == direction.measure)
          .map(bar =>
            (direction, bar.start + direction.quarters * bar.quarterMillis)
          )
      )
      .sortBy(_._2)
    timed.flatMap {
      case (Direction(_, _, _, "wedge", "stop"), _)         => None
      case (Direction(_, measure, _, "wedge", text), onset) =>
        val end = timed.collectFirst {
          case (Direction(_, _, _, "wedge", "stop"), at) if at > onset => at
        }
        Some(Marking("wedge", text, measure, onset, end.orElse(Some(onset))))
      case (Direction(_, measure, _, kind, text), onset) =>
        Some(Marking(kind, text, measure, onset, None))
    }
  }

  /** The part as it is to be played from that time on, to review a recording of
    * it: notes, measures and markings, in milliseconds of this engraving.
    */
  def reference(fromMillis: Double): PerformanceReference = {
    val from = barAt(fromMillis).fold(fromMillis)(_.start)
    val notes = played.collect {
      case (id, onset) if onset >= from =>
        val values = toolkit.getMIDIValuesForElement(id)
        ReferenceNote(
          Engraving.source(id),
          onset,
          lasting(id, onset),
          values.pitch.asInstanceOf[js.UndefOr[Int]].toList,
          barAt(onset).fold("")(_.number)
        )
    }
    PerformanceReference(
      notes.filter(_.pitches.nonEmpty).toList,
      bars
        .filter(_.end > from)
        .map(bar => ReferenceMeasure(bar.number, bar.start))
        .toList,
      barAt(from).fold(60000 / startTempo)(_.beatMillis),
      markings.filter(_.onset >= from)
    )
  }

  /** The notes of those parts from that time on, as they are played along with
    * a recording, repeats unfolded. A note's part is the first number of its id
    * (`ScoreEditing.identify`, on the server).
    */
  def accompaniment(
      parts: Set[Int],
      fromMillis: Double
  ): List[AccompanimentNote] =
    played.toList.flatMap {
      case (id, onset)
          if Engraving
            .partOf(id)
            .exists(parts.contains) && onset >= fromMillis =>
        val values = toolkit.getMIDIValuesForElement(id)
        values.pitch
          .asInstanceOf[js.UndefOr[Int]]
          .toOption
          .map(
            AccompanimentNote(
              onset,
              lasting(id, onset),
              _
            )
          )
      case _ => None
    }

  /** Where a measure is first played, in milliseconds. */
  def measureStart(number: String): Option[Double] =
    bars.find(_.number == number).map(_.start)

  /** The first measure starting at or after that time, in milliseconds. */
  def barStartingFrom(milliseconds: Double): Option[Bar] =
    bars.find(_.start >= milliseconds - 1)

  /** The measure being played at that time. */
  /** How long the score lasts as played, repeats unfolded, in milliseconds. */
  def durationMillis: Double = bars.lastOption.fold(0.0)(_.end)

  /** The share of the score's measures in which a part, counted in score order,
    * accompanies the others (`Conductor`).
    */
  def accompanying(part: Int): Double = {
    val measures = bars.map(_.number).distinct.size
    if (measures == 0) 0.0
    else
      math.min(
        1.0,
        interpretation.accompaniments
          .count(_.part == part)
          .toDouble / measures
      )
  }

  def barAt(milliseconds: Double): Option[Bar] =
    bars.findLast(_.start <= milliseconds).orElse(bars.headOption)

  /** The tempo the score starts at, in quarter notes a minute: the one its MIDI
    * and its times are worked out at.
    */
  def tempo: Double = startTempo

  def pageCount: Int = toolkit.getPageCount().asInstanceOf[Int]

  /** The page an element of the engraving is on, counted from 1; 0 when it is
    * on none.
    */
  def pageOf(id: String): Int =
    toolkit.getPageWithElement(id).asInstanceOf[Int]

  /** The SVG of a page, counted from 1. */
  def page(number: Int): String =
    toolkit.renderToSVG(number).asInstanceOf[String]

  /** The MIDI that plays the score as the conductor has it played
    * (`Conductor`); as the engraver writes it if that cannot be done.
    */
  def midi: Uint8Array = {
    val binary = dom.window.atob(toolkit.renderToMIDI().asInstanceOf[String])
    val written = Vector.tabulate(binary.length)(binary.charAt(_).toInt)
    val bytes = Try(performed(written)) match {
      case Success(played) => played
      case Failure(error)  =>
        dom.console.warn(s"the conductor could not play the score: $error")
        written
    }
    val result = new Uint8Array(bytes.length)
    for (index <- bytes.indices) {
      result(index) = bytes(index).toShort
    }
    result
  }

  private def performed(written: Vector[Int]): Vector[Int] = {
    val sequence = MidiSequence.read(written)
    // each note where it is played, by the pitch the server read: asking the
    // engraver for every note's took seconds
    val plays = playedQuarters.flatMap { (id, quarters) =>
      conductedNotes
        .get(Engraving.source(id))
        .map(conducted =>
          (
            conducted.track,
            math.round(quarters * sequence.ppq).toInt,
            conducted.pitch,
            conducted.play
          )
        )
    }
    // a quarter as played: at the engraver's tempo, slowed where the score's
    // words and the chosen tempi stretch it
    val secondsAQuarter = (quarter: Double) =>
      60.0 / startTempo * quarterStretches
        .takeWhile(_._1 <= quarter)
        .lastOption
        .fold(1.0)(_._2)
    // where each section after the first is first played
    val sectionTicks = sectionStarts.flatMap(note =>
      playedQuarters
        .find((id, _) => Engraving.source(id) == note)
        .map((_, quarters) => math.round(quarters * sequence.ppq).toInt)
    )
    val played = Performance.sectioned(
      Performance.played(sequence, plays, secondsAQuarter),
      sectionTicks
    )
    (if (quarterStretches.isEmpty) played
     else
       TempoChanges.stretched(
         played,
         quarterStretches.map((quarters, times) =>
           math.round(quarters * sequence.ppq).toInt -> times
         )
       )).bytes
  }

  def soundingAt(milliseconds: Double): Sounding = {
    val elements = toolkit.getElementsAtTime(
      if (stretches.isEmpty) milliseconds
      else TempoChanges.unstretched(stretches)(milliseconds)
    )
    def found(kind: js.Dynamic) = kind
      .asInstanceOf[js.UndefOr[js.Array[String]]]
      .map(_.toSeq.map(Engraving.source))
      .getOrElse(Seq.empty)
    Sounding(
      elements.page.asInstanceOf[js.UndefOr[Int]].getOrElse(0),
      found(elements.notes),
      found(elements.rests)
    )
  }

  /** When a note ends, in milliseconds: its first onset and its length. */
  def end(note: String): Option[Double] =
    restSpans
      .get(note)
      .map(_._2)
      .orElse(onsets.get(note).map(onset => onset + lasting(note, onset)))

  /** A note's pitch, in MIDI. */
  def pitch(note: String): Option[Int] =
    onsets
      .get(note)
      .flatMap(_ =>
        toolkit
          .getMIDIValuesForElement(note)
          .pitch
          .asInstanceOf[js.UndefOr[Int]]
          .toOption
      )

  /** The notes that start from `from` until `to`, in milliseconds. */
  def notesBetween(from: Double, to: Double): Set[String] =
    played.collect {
      case (id, onset) if onset >= from && onset < to => id
    }.toSet

  /** The beats from `from` until `to`, in milliseconds, each saying whether it
    * starts a measure.
    */
  def beatsBetween(from: Double, to: Double): Vector[(Double, Boolean)] =
    beats.filter((at, _) => at >= from - 1 && at < to)

  /** The beat on or before a time: where a count starts for music entering
    * between two beats.
    */
  def beatBefore(milliseconds: Double): Option[Double] =
    beats.map(_._1).filter(_ <= milliseconds + 1).lastOption

  /** How long the score's pickup lasts, if it starts with one: a first measure
    * shorter than its time signature, which ends on a downbeat.
    */
  def pickup(bar: Bar): Option[Double] =
    Option
      .when(bars.headOption.contains(bar) && isShort(bar))(bar.end - bar.start)

  private def isShort(bar: Bar): Boolean =
    bar.end - bar.start < bar.beats * bar.beatMillis - 1

  // Every beat, each saying whether it starts a measure. A pickup's are counted
  // back from the downbeat it leads to: its notes fall late in a measure whose
  // start, and first beats, come before the score does.
  // Counted in Verovio's times, then as played: the beats of a cut are not.
  private def beats: Vector[(Double, Boolean)] =
    writtenBars.zipWithIndex
      .flatMap { (bar, index) =>
        if (index == 0 && isShort(bar)) {
          (1 to bar.beats).reverse
            .map(back => (bar.end - back * bar.beatMillis, back == bar.beats))
        } else {
          Iterator
            .iterate(bar.start)(_ + bar.beatMillis)
            .takeWhile(_ < bar.end - 1)
            .zipWithIndex
            .map((at, index) => (at, index == 0))
            .toVector
        }
      }
      .collect {
        case (at, downbeat) if stretches.isEmpty || slowerAt(at) > 0 =>
          (stretched(at), downbeat)
      }

  /** When a note first sounds, in milliseconds. */
  def onset(note: String): Option[Double] = onsets.get(note)
}

object Engraving {

  /** The changes of tempo, by quarter note, with each span passed over at once
    * (infinitely faster, as a cut made), the music going on after it as it
    * would have.
    */
  def passedOver(
      changes: Vector[(Double, Double)],
      spans: List[(Double, Double)]
  ): Vector[(Double, Double)] =
    spans.foldLeft(changes) { case (sofar, (start, end)) =>
      val after = sofar.filter(_._1 <= end + 1e-6).lastOption.fold(1.0)(_._2)
      (sofar.filter((at, _) => at < start - 1e-6 || at > end + 1e-6) ++
        Vector(start -> 0.0, end -> after)).sortBy(_._1)
    }

  /** What `work` finds, else `otherwise`, the failure told in the console: the
    * score plays without it.
    */
  def warned[A](what: String)(work: => A)(otherwise: A): A =
    Try(work) match {
      case Success(found) => found
      case Failure(error) =>
        dom.console.warn(s"The score could not be read for $what: $error")
        otherwise
    }
  // an upright A4 page, in the tenths of a millimetre Verovio counts in
  val pageWidth = 2100
  val pageHeight = 2970

  // what Verovio plays a score with no tempo marked at
  val defaultTempo = 120.0

  // a note's id from the source: part, measure, rank
  private val noteId = """n(\d+)-\d+-\d+""".r

  /** The id an element has in the source: Verovio plays a repeat with copies of
    * its measures and notes, their ids the source's with `-rend2` (and so on)
    * after them.
    */
  def source(id: String): String = id.replaceFirst("-rend\\d+$", "")

  // The part a note belongs to, by the first number of its id.
  private def partOf(id: String): Option[Int] =
    source(id) match {
      case noteId(part) => part.toIntOption
      case _            => None
    }
}
