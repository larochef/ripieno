package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*
import ripieno.backend.reading.ScoreTimes.*

import org.w3c.dom.{Document, Element}

/** A note of a line as phrasing reads it: its time and length in quarter notes
  * from the part's start, its pitch (a chord's highest) and its measure.
  */
final case class LineNote(
    at: Double,
    quarters: Double,
    pitch: Int,
    measure: String
)

/** How a player shapes the dynamics a line is written at, as much of the
  * classical repertoire leaves implied:
  *
  *   - a line grows louder as it rises and softer as it falls;
  *   - a dynamic reached at the end of a fall (or of a rise) is prepared from
  *     the top (or the bottom) of it, not reached at once;
  *   - a phrase repeated at once is played a dynamic softer the second time, an
  *     echo.
  */
object Phrasing {

  // how many of a measure's notes may differ from the measure it repeats
  private val repeatedShare = 0.8

  /** Where the first part sets off on a new phrase, by time: a measure that
    * plays the one before it again an octave lower or higher (most of its
    * notes), a theme taken up again elsewhere.
    */
  def phraseStarts(document: Document): List[Double] = {
    val line = parts(document).headOption.toList
      .flatMap(part =>
        timed(part).collect {
          case (element, at)
              if element.getTagName == "note" &&
                !has(element, "rest") && !has(element, "grace") =>
            at -> pitchOf(element)
        }
      )
      .collect { case (at, Some(pitch)) => at -> pitch }
      .groupMapReduce(_._1)(_._2)(math.max)
      .toVector
      .sortBy(_._1)
    val starts = measuresOf(document).map(_._1).toVector
    def notesIn(from: Double, to: Double) =
      line.filter((at, _) => at >= from - 1e-6 && at < to - 1e-6)
    starts.indices.drop(1).dropRight(1).toList.flatMap { index =>
      val (before, at, after) =
        (starts(index - 1), starts(index), starts(index + 1))
      val played = notesIn(before, at)
      val again = notesIn(at, after)
      val sameRhythm = played.size >= 4 && played.size == again.size &&
        played
          .zip(again)
          .forall((first, second) =>
            math.abs((first._1 - before) - (second._1 - at)) < 1e-6
          )
      val octave = played
        .zip(again)
        .count((first, second) => math.abs(second._2 - first._2) == 12)
      Option.when(sameRhythm && octave >= repeatedShare * played.size)(at)
    }
  }

  private def pitchOf(note: Element): Option[Int] =
    child(note, "pitch").map { pitch =>
      val step = text(pitch, "step").fold(0)(name =>
        Vector(0, 2, 4, 5, 7, 9, 11)("CDEFGAB".indexOf(name.head))
      )
      val alter =
        text(pitch, "alter").flatMap(_.toDoubleOption).fold(0)(_.round.toInt)
      val octave = text(pitch, "octave").flatMap(_.toIntOption).getOrElse(4)
      12 * (octave + 1) + step + alter
    }

  // velocity a semitone above or below the line's pitch around a note, at most
  private val risingVelocity = 1.0
  private val contourReach = 10
  // quarter notes on each side of a note the line's pitch is taken over
  private val contourQuarters = 4.0
  // how far back a change of dynamic is prepared from, in quarter notes, and
  // how short a preparation is no preparation
  private val preparedQuarters = 12.0
  private val shortestPreparation = 1.0
  // what an echo takes off
  private val echoVelocity = 12

  /** The velocity a note of the line at `at` is played at: from `written`, the
    * dynamics as marked (`dynamics`, each at its time, with its velocity;
    * `hairpinEnds`, where a written hairpin leads to one), shaped. `melodic`
    * for a part playing one line (a violin), whose contour is followed.
    */
  def shaped(
      written: Double => Int,
      dynamics: List[(Double, Int)],
      hairpinEnds: List[Double],
      line: Vector[LineNote],
      melodic: Boolean
  ): Double => Int = {
    val prepared = preparations(written, dynamics, hairpinEnds, line)
    val echoed = echoes(line, dynamics.map(_._1))
    at => {
      val level = prepared
        .collectFirst {
          case (from, to, start, end) if at >= from - 1e-6 && at < to - 1e-6 =>
            math.round(start + (end - start) * (at - from) / (to - from)).toInt
        }
        .getOrElse(written(at))
      val contour = if (melodic) contourAt(line, at) else 0
      val echo =
        if (melodic && echoed.contains(measureAt(line, at))) echoVelocity
        else 0
      level + contour - echo
    }
  }

  // each change of dynamic reached at the end of a fall or a rise, made from
  // its top or its bottom: from, to, and the velocities at both
  private def preparations(
      written: Double => Int,
      dynamics: List[(Double, Int)],
      hairpinEnds: List[Double],
      line: Vector[LineNote]
  ): List[(Double, Double, Int, Int)] =
    dynamics.zipWithIndex.flatMap { case ((at, reached), index) =>
      val before = written(at - 1e-3)
      // a written hairpin leads to it already
      val led = hairpinEnds.exists(end => end <= at + 1e-6 && end > at - 1)
      val since = dynamics.lift(index - 1).fold(0.0)(_._1)
      val leading = line.filter(note =>
        note.at > math.max(since, at - preparedQuarters) - 1e-6 &&
          note.at < at - shortestPreparation + 1e-6
      )
      val last = line.filter(_.at < at - 1e-6).lastOption
      Option
        .when(!led && reached != before && leading.nonEmpty)(
          if (reached < before) leading.maxBy(_.pitch)
          else leading.minBy(_.pitch)
        )
        .filter(turn =>
          // a fall to a softer dynamic, a rise to a louder one: else it comes
          // at once, as written
          last.exists(end =>
            if (reached < before) end.pitch < turn.pitch
            else end.pitch > turn.pitch
          )
        )
        .map(turn => (turn.at, at, written(turn.at), reached))
    }

  // how much louder than its surroundings the line is at a time, by how much
  // higher it is
  private def contourAt(line: Vector[LineNote], at: Double): Int =
    line
      .find(note => note.at <= at + 1e-6 && note.at + note.quarters > at)
      .orElse(line.findLast(_.at <= at + 1e-6))
      .fold(0) { note =>
        val around = line.filter(other =>
          other.at + other.quarters > at - contourQuarters &&
            other.at < at + contourQuarters
        )
        val weight = around.map(_.quarters).sum
        if (weight <= 0) 0
        else {
          val mean =
            around.map(other => other.pitch * other.quarters).sum / weight
          math.max(
            -contourReach,
            math.min(
              contourReach,
              math.round((note.pitch - mean) * risingVelocity).toInt
            )
          )
        }
      }

  private def measureAt(line: Vector[LineNote], at: Double): String =
    line.findLast(_.at <= at + 1e-6).fold("")(_.measure)

  // the measures played as an echo: the second of two the same in a row, or
  // the second pair of two pairs the same, no dynamic marked in them
  private def echoes(
      line: Vector[LineNote],
      marked: List[Double]
  ): Set[String] = {
    val measures = line.map(_.measure).distinct
    val notes = line.groupBy(_.measure)
    def shape(numbers: Seq[String]): Vector[(Double, Double, Int)] = {
      val start = numbers.headOption
        .flatMap(notes.get)
        .flatMap(_.headOption)
        .fold(0.0)(_.at)
      numbers
        .flatMap(notes.getOrElse(_, Vector.empty))
        .map(note => (note.at - start, note.quarters, note.pitch))
        .toVector
    }
    def unmarked(numbers: Seq[String]) =
      numbers.flatMap(notes.getOrElse(_, Vector.empty)).forall { note =>
        !marked
          .exists(at => at >= note.at - 1e-6 && at < note.at + note.quarters)
      }
    def repeated(size: Int): Set[String] =
      measures
        .sliding(2 * size)
        .collect {
          case window if window.size == 2 * size && {
                val (first, second) = window.splitAt(size)
                val firstShape = shape(first)
                firstShape.size >= 3 && firstShape == shape(second) &&
                unmarked(second)
              } =>
            window.drop(size)
        }
        .flatten
        .toSet
    repeated(1) ++ repeated(2)
  }
}
