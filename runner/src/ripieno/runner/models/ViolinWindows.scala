package ripieno.runner.models

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** A note of a violin line as VIOLET reads it (note_seq, from its MIDI file),
  * its stroke's keyswitch among them: a pitch under the violin's. Its velocity
  * is how strongly it is played (the page Conductor's: its dynamic, accents and
  * balance), VIOLET's dynamics from its start to the next note's.
  */
final case class ViolinNote(
    pitch: Int,
    velocity: Int,
    start: Double,
    end: Double
)

object ViolinNote {

  /** A line's notes, one a line: `pitch velocity start end`, in seconds. */
  def read(file: Path): Vector[ViolinNote] =
    Files
      .readAllLines(file)
      .asScala
      .toVector
      .filter(_.trim.nonEmpty)
      .map { line =>
        val Array(pitch, velocity, start, end) = line.trim.split(" ")
        ViolinNote(pitch.toInt, velocity.toInt, start.toDouble, end.toDouble)
      }
}

/** A window of a line: where it starts, in frames, and its conditions: a roll
  * of the notes (`[pitches, rollFrames]`, one where a pitch sounds) and one of
  * their strokes (`[techniques, rollFrames]`), every 10 ms, and its dynamics,
  * one a latent frame.
  */
final case class ViolinWindow(
    startFrame: Int,
    notes: Array[Float],
    techniques: Array[Float],
    dynamics: Array[Float]
)

/** A line cut into VIOLET's windows, as its reference implementation does
  * (`prepare_eval_aligned_windows`): ten seconds every five, from the start to
  * a second after the last note, the last window ending at the end.
  */
object ViolinWindows {

  /** Latent frames a second. */
  val FrameRate = 25.0
  val WindowFrames = 250
  private val overlap = 0.5
  val StepFrames: Int = (WindowFrames * (1 - overlap)).toInt
  val WindowSeconds: Double = WindowFrames / FrameRate
  private val tailSeconds = 1.0
  // the rolls' step, in seconds, and their frames
  private val rollStep = 0.01
  val RollFrames: Int = math.ceil(WindowSeconds / rollStep).toInt
  val LowestPitch = 55
  val HighestPitch = 105
  val Pitches: Int = HighestPitch - LowestPitch + 1
  val Techniques = 13
  // seconds a stroke's keyswitch may come before its note
  private val lead = 0.01
  // the stroke of a note no keyswitch marks, in a line that has none
  private val defaultTechnique = 1
  private val maximumVelocity = 127

  /** The keyswitches' pitches and the technique each stands for
    * (configs/ks_config.yaml, in the order of `load_technique_map`).
    */
  val Keyswitches: Map[Int, Int] = Map(
    36 -> 1,
    37 -> 2,
    38 -> 3,
    39 -> 4,
    40 -> 5,
    41 -> 6,
    42 -> 7,
    43 -> 8,
    44 -> 9,
    48 -> 10,
    49 -> 11,
    50 -> 12
  )

  /** When the line ends, as note_seq tells it: its last note's end. */
  def totalTime(notes: Vector[ViolinNote]): Double =
    notes.map(_.end).maxOption.getOrElse(0.0)

  /** The seconds rendered: the line and a second of tail, a window at least. */
  def duration(notes: Vector[ViolinNote]): Double =
    math.max(totalTime(notes) + tailSeconds, WindowSeconds)

  def totalFrames(notes: Vector[ViolinNote]): Int =
    math.max(math.ceil(duration(notes) * FrameRate).toInt, WindowFrames)

  /** Where each window starts, in frames. */
  def starts(notes: Vector[ViolinNote]): Vector[Int] = {
    val total = totalFrames(notes)
    val regular = Iterator
      .iterate(0)(_ + StepFrames)
      .takeWhile(_ + WindowFrames <= total)
      .toVector
    if (regular.isEmpty || regular.last + WindowFrames < total)
      regular :+ math.max(0, total - WindowFrames)
    else regular
  }

  def windows(notes: Vector[ViolinNote]): Vector[ViolinWindow] =
    starts(notes).map { start =>
      val startTime = start / FrameRate
      ViolinWindow(
        start,
        notesRoll(notes, startTime),
        techniqueRoll(notes, startTime),
        dynamics(notes, startTime)
      )
    }

  /** The dynamics of each latent frame of the window starting at `startTime`,
    * as VIOLET learnt them (MIDI's CC1 over 127, held until it changes): the
    * velocity of the last note started by then, the first note's before it.
    */
  def dynamics(notes: Vector[ViolinNote], startTime: Double): Array[Float] = {
    val played = notes
      .filter(note => !Keyswitches.contains(note.pitch))
      .filter(_.pitch >= LowestPitch)
      .sortBy(_.start)
    val first = played.headOption.fold(maximumVelocity)(_.velocity)
    Array.tabulate(WindowFrames) { frame =>
      val time = startTime + frame / FrameRate
      val velocity = played
        .takeWhile(_.start <= time + 1e-9)
        .lastOption
        .fold(first)(_.velocity)
      math.max(0, math.min(velocity, maximumVelocity)).toFloat / maximumVelocity
    }
  }

  // the frames of the roll a span covers, from its window's start
  private def frames(
      onset: Double,
      offset: Double,
      startTime: Double
  ): (Int, Int) = {
    val first = math.floor((onset - startTime) / rollStep).toInt
    val last = math.ceil((offset - startTime) / rollStep).toInt
    (
      math.max(0, math.min(first, RollFrames)),
      math.max(0, math.min(last, RollFrames))
    )
  }

  private def notesRoll(
      notes: Vector[ViolinNote],
      startTime: Double
  ): Array[Float] = {
    val roll = new Array[Float](Pitches * RollFrames)
    val endTime = startTime + WindowSeconds
    for (note <- notes) {
      if (
        !Keyswitches.contains(note.pitch) && note.pitch >= LowestPitch &&
        note.pitch <= HighestPitch
      ) {
        val onset = math.max(note.start, startTime)
        val offset = math.min(note.end, endTime)
        if (offset > onset) {
          val (first, last) = frames(onset, offset, startTime)
          val row = (note.pitch - LowestPitch) * RollFrames
          for (frame <- first until last) roll(row + frame) = 1.0f
        }
      }
    }
    roll
  }

  private def techniqueRoll(
      notes: Vector[ViolinNote],
      startTime: Double
  ): Array[Float] = {
    val roll = new Array[Float](Techniques * RollFrames)
    val endTime = startTime + WindowSeconds
    val matched = techniquesOf(notes, startTime)
    for (note <- notes) {
      if (!Keyswitches.contains(note.pitch) && note.pitch >= LowestPitch) {
        val onset = math.max(note.start, startTime)
        val offset = math.min(note.end, endTime)
        if (offset > onset) {
          val (first, last) = frames(onset, offset, startTime)
          if (last > first) {
            val technique = math.max(
              0,
              math.min(
                matched.getOrElse((note.start, note.pitch), 0),
                Techniques - 1
              )
            )
            for (frame <- first until last)
              roll(technique * RollFrames + frame) = 1.0f
          }
        }
      }
    }
    roll
  }

  /** The technique of each note sounding in the window, by its start and pitch:
    * the first keyswitch from a lead and a half before it to a lead after it
    * (`match_techniques`).
    */
  private def techniquesOf(
      notes: Vector[ViolinNote],
      startTime: Double
  ): Map[(Double, Int), Int] = {
    val endTime = startTime + WindowSeconds
    val annotated = notes.exists(_.pitch < LowestPitch)
    val played = notes
      .filter(_.pitch >= LowestPitch)
      .filter(note => note.start < endTime && note.end > startTime)
    if (!annotated) {
      played.map(note => (note.start, note.pitch) -> defaultTechnique).toMap
    } else {
      val switches = notes
        .filter(note => Keyswitches.contains(note.pitch))
        .filter(note => startTime - lead <= note.start && note.start <= endTime)
        .map(note => (note.start, Keyswitches(note.pitch)))
        .sortBy(_._1)
      played.foldLeft(Map.empty[(Double, Int), Int]) { (found, note) =>
        val earliest = math.max(startTime, note.start - 2 * lead)
        val latest = note.start + lead
        switches
          .dropWhile(_._1 < earliest)
          .headOption
          .filter(_._1 <= latest)
          .fold(found)((_, technique) =>
            found + ((note.start, note.pitch) -> technique)
          )
      }
    }
  }
}
