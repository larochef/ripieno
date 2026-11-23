package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*
import ripieno.backend.reading.ScoreTimes.*
import ripieno.shared.*

import org.w3c.dom.{Document, Element}

/** Where the words of a score change its tempo, which the engraver plays as
  * written: a ritardando ("rit.", "rall.") slows down until the next tempo, an
  * "a tempo" or the last note struck; "moins vite" (meno mosso) goes on a
  * little slower; a fermata holds its note. The engraver holds one tempo a
  * measure, so these are played by stretching its time: each change as how many
  * times slower than written the music goes from a note on.
  *
  * A part of the score starts at each tempo it writes and each title starting
  * one (an "Allegro", a "Trio"); one with no tempo written, nor any before it,
  * is played at a tempo guessed from its words and its rhythm (`TempoGuess`).
  */
object TempoReading {

  // the tempo the engraver plays a score at when it writes none
  private val engraverTempo = 120.0
  // the share of its tempo a ritardando ends at
  private val slowedTo = 0.5
  // the share of its tempo "moins vite" goes on at
  private val lessQuick = 0.85
  // how much longer than written a note under a fermata lasts
  private val held = 1.3
  // a note under a fermata is waited for: the quarters before it go this much
  // slower
  private val awaited = 1.4
  private val awaitedQuarters = 0.5
  // how often a ritardando's tempo changes, in quarters
  private val stepQuarters = 0.5

  private val slowing = "^(rit|ritard|ritardando|rall|rallentando)\\b.*".r
  private val calmer = "^(moins vite|meno mosso|pi[uù] lento)\\b.*".r
  private val restoring = "^(a tempo|tempo i|tempo primo)\\b.*".r
  // the words of tempo and of dance a part of a score is titled with
  private val titled = List(
    "grave",
    "largo",
    "larghetto",
    "lento",
    "adagi",
    "andant",
    "moderato",
    "allegr",
    "vivace",
    "presto",
    "prestissimo",
    "tempo di",
    "menuet",
    "minuet",
    "trio",
    "rondo",
    "prélude",
    "prelude",
    "sarabande",
    "gavotte",
    "gigue",
    "bourrée",
    "courante",
    "allemande",
    "aria",
    "finale",
    "scherzo",
    "romance",
    "romanze",
    "polonaise",
    "valse",
    "sicilien",
    "lent",
    "modéré",
    "vif",
    "animé"
  )

  sealed private trait Change
  private case object Slowing extends Change
  private case object Calmer extends Change
  private case object Restoring extends Change
  final private case class SetTo(tempo: Option[Double], name: Option[String])
      extends Change
  final private case class Sign(label: Option[String]) extends Change
  final private case class Hold(end: Double) extends Change

  /** How the score (its notes given their ids) is timed with the tempo `chosen`
    * for some of its sections (in quarter notes a minute) and the cuts `cut`
    * made.
    */
  def timing(
      document: Document,
      chosen: Map[Int, Double],
      cut: Set[Int]
  ): ScoreTiming = {
    val (marks, notes) = read(document)
    val guesses = guessed(document, marks)
    val sectionCount = marks.count(_._2.isInstanceOf[SetTo])
    val rechosen =
      chosen.filter((index, tempo) => index < sectionCount && tempo > 0)
    ScoreTiming(
      sections(marks, notes, guesses, rechosen),
      cuts(document, marks),
      changes(marks, notes, guesses, rechosen, cut)
    )
  }

  // the parts of the score at a tempo of their own, from their first note,
  // each at the tempo chosen for it, else the one written before it, else the
  // one guessed
  private def sections(
      marks: List[(Double, Change)],
      notes: Vector[(Double, String)],
      guesses: Map[Int, Double],
      chosen: Map[Int, Double]
  ): List[TempoSection] = {
    var written = engraverTempo
    marks
      .collect { case (at, SetTo(tempo, name)) => (at, tempo, name) }
      .zipWithIndex
      .map { case ((at, tempo, name), index) =>
        tempo.foreach(written = _)
        val played =
          chosen.getOrElse(index, guesses.getOrElse(index, written))
        (at, tempo, played, name)
      }
      .flatMap((at, tempo, played, name) =>
        notes
          .find(_._1 >= at - 1e-6)
          .map(note => (tempo, played, name, note._2))
      )
      .zipWithIndex
      .map { case ((tempo, played, name, note), index) =>
        TempoSection(index, name, tempo, played, note)
      }
  }

  // How many times slower than written the score goes from each time that
  // changes it: none for a score played as written. A time is told by the
  // first note struck at or after it, in any part (its id), and how many
  // quarter notes before that note it is: the engraver times notes, not rests.
  private def changes(
      marks: List[(Double, Change)],
      notes: Vector[(Double, String)],
      guesses: Map[Int, Double],
      rechosen: Map[Int, Double],
      cut: Set[Int]
  ): List[TimeChange] = {
    val made = spans(marks).zipWithIndex.collect {
      case (span, index) if cut.contains(index) => span
    }
    val holds = marks.collect { case (at, Hold(end)) => at -> end }
    if (
      rechosen.isEmpty && made.isEmpty && holds.isEmpty && guesses.isEmpty &&
      !marks.exists(mark => mark._2 == Slowing || mark._2 == Calmer)
    ) Nil
    else {
      // a ritardando to the end slows down until the last note struck
      val end = notes.lastOption.fold(0.0)(_._1)
      var written = engraverTempo
      var section = engraverTempo
      var played = engraverTempo
      var index = -1
      val changes = marks.zipWithIndex.flatMap {
        case ((at, SetTo(tempo, _)), _) =>
          index += 1
          // a part with no tempo written goes on at the engraver's, played
          // at the one written before it, else at the one guessed
          tempo.foreach(written = _)
          section = rechosen.getOrElse(index, guesses.getOrElse(index, written))
          played = section
          List(at -> written / played)
        case ((at, Restoring), _) =>
          played = section
          List(at -> written / played)
        case ((at, Calmer), _) =>
          played *= lessQuick
          List(at -> written / played)
        case ((start, Slowing), index) =>
          val stop = marks.drop(index + 1).find(_._2 != Slowing).fold(end)(_._1)
          val from = played
          val steps =
            math.max(1, math.ceil((stop - start) / stepQuarters).toInt)
          (0 until steps).toList.map { step =>
            played = from * (1 - (1 - slowedTo) * (step + 0.5) / steps)
            start + step * (stop - start) / steps -> written / played
          }
        case _ => Nil
      }
      // a cut made takes no time: from its start to its end, the score goes
      // infinitely slower, then on as it would have
      val withCuts = made.foldLeft(changes.sortBy(_._1)) {
        case (sofar, (start, end)) =>
          val after =
            sofar.filter(_._1 <= end + 1e-6).lastOption.fold(1.0)(_._2)
          (sofar.filter((at, _) => at < start - 1e-6 || at > end + 1e-6) ++
            List(start -> 0.0, end -> after)).sortBy(_._1)
      }
      // a fermata's note is waited for: the music goes slower before it
      val withWaits = holds.foldLeft(withCuts) { case (sofar, (start, _)) =>
        val from = math.max(0.0, start - awaitedQuarters)
        def slowerAt(at: Double) =
          sofar.filter(_._1 <= at + 1e-6).lastOption.fold(1.0)(_._2)
        val inside = sofar.collect {
          case (at, slower) if at > from + 1e-6 && at < start - 1e-6 =>
            at -> slower * awaited
        }
        (sofar.filter((at, _) => at < from - 1e-6 || at > start - 1e-6) ++
          List(from -> slowerAt(from) * awaited) ++ inside ++
          Option
            .when(!sofar.exists(change => math.abs(change._1 - start) < 1e-6))(
              start -> slowerAt(start)
            )
            .toList).sortBy(_._1)
      }
      // a fermata holds its note: the music goes slower while it lasts
      val withHolds = holds.foldLeft(withWaits) { case (sofar, (start, end)) =>
        def slowerAt(at: Double) =
          sofar.filter(_._1 <= at + 1e-6).lastOption.fold(1.0)(_._2)
        val inside = sofar.collect {
          case (at, slower) if at > start + 1e-6 && at < end - 1e-6 =>
            at -> slower * held
        }
        (sofar.filter((at, _) => at < start - 1e-6 || at > end + 1e-6) ++
          List(start -> slowerAt(start) * held) ++ inside ++
          List(end -> slowerAt(end))).sortBy(_._1)
      }
      // each change from the first note struck at or after it
      withHolds.flatMap((at, slower) =>
        notes
          .find(_._1 >= at - 1e-6)
          .map((time, id) => TimeChange(id, slower, math.max(0.0, time - at)))
      )
    }
  }

  // The cuts the score allows (a passage between two coda signs, ⊕, as an
  // editor marks a "coupure à volonté"), in order: its words, and the measures
  // it starts and ends in.
  private def cuts(
      document: Document,
      marks: List[(Double, Change)]
  ): List[ScoreCut] = {
    val labels = marks.collect { case (at, Sign(label)) => at -> label }
    val measures = measuresOf(document)
    def measureAt(at: Double) =
      measures.filter(_._1 <= at + 1e-6).lastOption.fold("")(_._2)
    spans(marks).zipWithIndex.map { case ((start, end), index) =>
      ScoreCut(
        index,
        labels.find(_._1 == start).flatMap(_._2),
        measureAt(start),
        measureAt(end - 1e-3)
      )
    }
  }

  // the passages between two coda signs, by time
  private def spans(marks: List[(Double, Change)]): List[(Double, Double)] =
    marks
      .collect { case (at, Sign(_)) => at }
      .grouped(2)
      .collect { case List(start, end) if end > start => start -> end }
      .toList

  // the tempi and the words changing them, in order, and the notes struck in
  // every part, by time
  private def read(
      document: Document
  ): (List[(Double, Change)], Vector[(Double, String)]) = {
    val all = parts(document)
    // a tempo marked in several parts is one
    val marks = all
      .flatMap(marksOf)
      // a tempo written before a part's title alone
      .sortBy {
        case (at, SetTo(tempo, _)) => (at, if (tempo.isDefined) 0 else 1)
        case (at, _)               => (at, 2)
      }
      .distinctBy {
        case (at, SetTo(_, _)) => (at, "tempo")
        case (at, Sign(_))     => (at, "sign")
        case (at, Hold(_))     => (at, "hold")
        case (at, change)      => (at, change.toString)
      }
      .sortBy(_._1)
    // a change is made from the first note struck at or after it, in any part:
    // the first part may be silent where the others play
    (marks, all.toVector.flatMap(struck).sortBy(_._1))
  }

  // The tempo of each part of the score (counted among its `SetTo`) that
  // writes none, with none written before it either: guessed from its words
  // and the rhythm it runs in, the measures a ritardando slows left out.
  private def guessed(
      document: Document,
      marks: List[(Double, Change)]
  ): Map[Int, Double] = {
    val starts = marks.collect { case (at, SetTo(tempo, name)) =>
      (at, tempo, name)
    }
    if (starts.forall(_._2.isDefined)) Map.empty
    else {
      val all = parts(document)
      val played = all.flatMap(playedIn)
      // from when the beat is a half note (2/2) or not, by the first part's
      // time signatures
      val halfBeats = all.headOption.toList
        .flatMap(timed)
        .flatMap((element, at) =>
          child(element, "time")
            .map(time => at -> text(time, "beat-type").contains("2"))
        )
      val slowed = marks.zipWithIndex.collect {
        case ((start, Slowing), index) =>
          start -> marks
            .drop(index + 1)
            .find(_._2 != Slowing)
            .fold(Double.MaxValue)(_._1)
      }
      val ends = starts.drop(1).map(_._1) :+ Double.MaxValue
      val written = starts.scanLeft(false)(_ || _._2.isDefined)
      starts
        .zip(ends)
        .zipWithIndex
        .foldLeft(Map.empty[Int, Double]) {
          case (found, (((start, None, name), end), index))
              if !written(index) =>
            val inPart = played.collect {
              case (at, measureStart, note)
                  if at >= start - 1e-6 && at < end - 1e-6 &&
                    !slowed.exists((from, until) =>
                      measureStart >= from - 1e-6 && measureStart < until
                    ) =>
                note
            }
            found + (index -> TempoGuess.tempo(
              name,
              inPart,
              halfBeats.findLast(_._1 <= start + 1e-6).exists(_._2),
              found.get(index - 1)
            ))
          case (found, _) => found
        }
    }
  }

  // what a part plays, each note and rest (a chord once, a grace note not)
  // with its time and its measure's
  private def playedIn(part: Element): List[(Double, Double, PlayedNote)] = {
    val measures = children(part, "measure")
    val index = measures.zipWithIndex.toMap
    var divisions = 1.0
    val divisionsOf = measures.map { measure =>
      at(measure, "attributes", "divisions")
        .flatMap(_.getTextContent.trim.toDoubleOption)
        .foreach(divisions = _)
      measure -> divisions
    }.toMap
    val times = timed(part)
    val measureStarts = times
      .groupBy(_._1.getParentNode)
      .view
      .mapValues(_.map(_._2).min)
      .toMap
    times.collect {
      case (element, at)
          if element.getTagName == "note" &&
            !has(element, "chord") && !has(element, "grace") =>
        val measure = element.getParentNode.asInstanceOf[Element]
        val quarters = text(element, "duration")
          .flatMap(_.toDoubleOption)
          .getOrElse(0.0) / divisionsOf.getOrElse(measure, 1.0)
        (
          at,
          measureStarts.getOrElse(measure, at),
          PlayedNote(
            index.getOrElse(measure, -1),
            text(element, "voice").getOrElse("1"),
            quarters,
            child(element, "pitch").map(
              _.getTextContent.filterNot(_.isWhitespace)
            )
          )
        )
    }
  }

  // the notes struck in a part, by time, with their ids
  private def struck(part: Element): Vector[(Double, String)] =
    timed(part)
      .collect {
        case (element, at)
            if element.getTagName == "note" && !has(element, "rest") &&
              element.hasAttribute("id") =>
          at -> element.getAttribute("id")
      }
      .sortBy(_._1)
      .toVector

  // a part's tempi and the words that change them, at their time, and its
  // fermatas, until what follows them
  private def marksOf(part: Element): List[(Double, Change)] = {
    val timedChildren = timed(part).toVector
    val fermatas = timedChildren.indices.flatMap { index =>
      val (element, at) = timedChildren(index)
      Option
        .when(
          element.getTagName == "note" && !has(element, "chord") &&
            MusicXmlDocument.at(element, "notations", "fermata").isDefined
        )(timedChildren.drop(index + 1).find(_._2 > at + 1e-6))
        .flatten
        .map((_, end) => at -> Hold(end))
    }
    fermatas.toList ++ markings(timedChildren.toList) ++ titles(part)
  }

  // the titles starting a part of the score, as a part with no tempo written:
  // words over a measure's first beat, in the first measure, or with a new key
  // or time, or after a double bar, or a word of tempo or dance
  private def titles(part: Element): List[(Double, Change)] = {
    val measures = children(part, "measure")
    val times = timed(part)
    val starts = times
      .groupBy(_._1.getParentNode)
      .view
      .mapValues(_.map(_._2).min)
      .toMap
    val timeOf = times.toMap
    // the notes a part's title leads in from: after the last rest of the
    // measure before (its first voice), its upbeat
    def upbeat(before: Element): Option[Double] = {
      val voice = children(before).takeWhile(_.getTagName != "backup")
      val lastRest = voice.lastIndexWhere(element =>
        element.getTagName == "note" && has(element, "rest")
      )
      Option
        .when(lastRest >= 0)(voice.drop(lastRest + 1))
        .toList
        .flatten
        .filter(element =>
          element.getTagName == "note" &&
            !has(element, "chord") && !has(element, "grace")
        )
        .flatMap(timeOf.get)
        .minOption
    }
    measures.zipWithIndex.flatMap { (measure, index) =>
      val start = starts.getOrElse(measure, 0.0)
      def doubled(barline: Element) =
        text(barline, "bar-style").exists(
          Set("light-light", "light-heavy", "heavy-light", "heavy-heavy")
        )
      val opening = index == 0 ||
        at(measure, "attributes", "key").isDefined ||
        at(measure, "attributes", "time").isDefined ||
        measures
          .lift(index - 1)
          .toList
          .flatMap(children(_, "barline"))
          .exists(doubled)
      val words = children(measure)
        .takeWhile(_.getTagName != "note")
        .filter(element =>
          element.getTagName == "direction" &&
            element.getAttribute("placement") != "below"
        )
        .flatMap(all(_, "direction-type", "words"))
        .map(_.getTextContent.trim)
        .filter(_.nonEmpty)
      words
        .find(text => opening || titled.exists(text.toLowerCase.startsWith))
        .map(title =>
          measures
            .lift(index - 1)
            .filter(_ => index > 0)
            .flatMap(upbeat)
            .getOrElse(start) -> SetTo(None, Some(title))
        )
        .toList
    }
  }

  private def markings(
      timedChildren: List[(Element, Double)]
  ): List[(Double, Change)] =
    timedChildren.flatMap {
      case (element, at) if element.getTagName == "direction" =>
        val written = all(element, "direction-type", "words")
          .map(_.getTextContent.trim)
          .filter(_.nonEmpty)
        val tempo = child(element, "sound")
          .map(_.getAttribute("tempo"))
          .flatMap(_.toDoubleOption)
          .map(tempo => at -> SetTo(Some(tempo), written.headOption))
        val words = written
          .map(_.toLowerCase)
          .collectFirst {
            case slowing(_)   => at -> Slowing
            case calmer(_)    => at -> Calmer
            case restoring(_) => at -> Restoring
          }
        val sign = Option
          .when(
            MusicXmlDocument.at(element, "direction-type", "coda").isDefined
          )(
            at -> Sign(written.headOption)
          )
        // a tempo set with its words ("a tempo", with its speed) sets it
        sign.orElse(tempo).orElse(words).toList
      case _ => Nil
    }
}
