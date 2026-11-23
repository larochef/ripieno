package ripieno.frontend.score

import ripieno.shared.*

import scala.collection.mutable

/** A message of a MIDI track at its tick: its bytes, status first (running
  * status spelled out), a meta event as FF, type, length, data.
  */
final case class MidiEvent(tick: Int, data: Vector[Int]) {
  def isNoteOn: Boolean = (data.head & 0xf0) == 0x90 && data(2) > 0
  def isNoteOff: Boolean =
    (data.head & 0xf0) == 0x80 || ((data.head & 0xf0) == 0x90 && data(2) == 0)
  def isEndOfTrack: Boolean = data.head == 0xff && data(1) == 0x2f
}

/** A note of a track, from its note-on to its note-off. */
final case class MidiNote(
    start: Int,
    end: Int,
    channel: Int,
    pitch: Int,
    velocity: Int
)

/** A standard MIDI file read into its tracks' events, at absolute ticks. */
final case class MidiSequence(
    header: Vector[Int],
    tracks: Vector[Vector[MidiEvent]]
) {

  /** Ticks a quarter note. */
  def ppq: Int = (header(12) << 8) | header(13)

  def bytes: Vector[Int] =
    header ++ tracks.flatMap(MidiSequence.chunk)
}

object MidiSequence {

  private val headerLength = 14

  /** The file's events; a file this cannot read throws. */
  def read(bytes: IndexedSeq[Int]): MidiSequence = {
    val header = bytes.take(headerLength).toVector
    val tracks = Vector.newBuilder[Vector[MidiEvent]]
    var offset = headerLength
    while (offset + 8 <= bytes.length) {
      val length =
        (4 until 8).foldLeft(0)((sum, at) => (sum << 8) | bytes(offset + at))
      if (bytes.slice(offset, offset + 4) == Vector(0x4d, 0x54, 0x72, 0x6b)) {
        tracks += events(bytes, offset + 8, offset + 8 + length)
      }
      offset += 8 + length
    }
    MidiSequence(header, tracks.result())
  }

  private def events(bytes: IndexedSeq[Int], from: Int, to: Int) = {
    val found = Vector.newBuilder[MidiEvent]
    var at = from
    var tick = 0
    var status = 0
    def number(): Int = {
      var value = 0
      var more = true
      while (more) {
        value = (value << 7) | (bytes(at) & 0x7f)
        more = (bytes(at) & 0x80) != 0
        at += 1
      }
      value
    }
    while (at < to) {
      tick += number()
      if (bytes(at) >= 0x80) {
        status = bytes(at)
        at += 1
      }
      if (status == 0xff) {
        val kind = bytes(at)
        at += 1
        val start = at
        val length = number()
        val lengthBytes = bytes.slice(start, at)
        found += MidiEvent(
          tick,
          Vector(0xff, kind) ++ lengthBytes ++ bytes.slice(at, at + length)
        )
        at += length
      } else if (status == 0xf0 || status == 0xf7) {
        val start = at
        val length = number()
        found += MidiEvent(
          tick,
          Vector(status) ++ bytes.slice(start, at) ++ bytes
            .slice(at, at + length)
        )
        at += length
      } else {
        val size =
          if ((status & 0xf0) == 0xc0 || (status & 0xf0) == 0xd0) 1 else 2
        found += MidiEvent(tick, status +: bytes.slice(at, at + size).toVector)
        at += size
      }
    }
    found.result()
  }

  // a track as its chunk: events in order of tick, at a tick the notes ending
  // before those starting, and the end of the track last
  private def chunk(events: Vector[MidiEvent]): Vector[Int] = {
    def order(event: MidiEvent): (Int, Int) =
      if (event.isEndOfTrack) (Int.MaxValue, 0)
      else (event.tick, if (event.isNoteOn) 1 else 0)
    val sorted = events.zipWithIndex
      .sortBy((event, index) => (order(event), index))
      .map(_._1)
    val body = sorted
      .foldLeft((Vector.empty[Int], 0)) { case ((written, last), event) =>
        val tick = math.max(event.tick, last)
        (written ++ variable(tick - last) ++ event.data, tick)
      }
      ._1
    Vector(0x4d, 0x54, 0x72, 0x6b) ++ (0 until 4).reverse.map(byte =>
      (body.length >> (8 * byte)) & 0xff
    ) ++ body
  }

  private def variable(value: Int): Vector[Int] = {
    val groups = Iterator
      .iterate(value)(_ >> 7)
      .takeWhile(_ > 0)
      .map(_ & 0x7f)
      .toVector
      .reverse
    if (groups.isEmpty) Vector(0)
    else groups.init.map(_ | 0x80) :+ groups.last
  }
}

/** The engraver's MIDI played the way a score is interpreted: no note starts at
  * another time than the engraver's, so the sheet and the sound stay together;
  * notes are let go earlier, played louder or softer, or ornamented within
  * their own time.
  */
object Performance {

  // the shortest step of an ornament, a thirty-second note
  private val stepsAQuarter = 8
  // a breath before a note: this share of the note before it, at most a step
  private val breathShare = 0.25
  // the share of a long trilled note that is trilled
  private val trilledShare = 0.75
  // how late a broken chord's upper strings come, at most a share of the
  // chord's length: a short chord is not given over to its lower strings
  private val breakSeconds = 0.15
  private val breakShare = 0.4
  // the shortest note of an ornament: a quicker one is no longer heard as a
  // note (and the engines take notes this close for a chord)
  private val ornamentSeconds = 0.05

  // a time in seconds as ticks, at the tempo played from a tick on
  private def ticksOf(
      secondsAQuarter: Double => Double,
      ppq: Int,
      tick: Int
  ): Double => Int = {
    val quarter = secondsAQuarter(tick.toDouble / ppq)
    seconds => math.round(seconds / quarter * ppq).toInt
  }

  /** The sequence with each note found at its track, tick and pitch played as
    * said; a note not found (a tied one, which has no note-on) is left.
    * `secondsAQuarter` gives how long a quarter lasts, as played, from a
    * quarter note: a broken chord's strings are timed in seconds.
    */
  def played(
      sequence: MidiSequence,
      plays: Seq[(Int, Int, Int, NotePlay)],
      secondsAQuarter: Double => Double
  ): MidiSequence = {
    val step = math.max(1, sequence.ppq / stepsAQuarter)
    val byTrack = plays.groupBy(_._1)
    sequence.copy(tracks = sequence.tracks.zipWithIndex.map { (events, track) =>
      byTrack.get(track).fold(events) { here =>
        val (notes, others) = paired(events)
        val wanted =
          here.map((_, tick, pitch, play) => (tick, pitch) -> play).toMap
        // each note with how it is played, found at its written start
        val lengthened = notes.map { note =>
          val play = wanted.get((note.start, note.pitch))
          play.fold(note) { play =>
            val length =
              math
                .max(1, math.round((note.end - note.start) * play.length).toInt)
            val ticks = ticksOf(secondsAQuarter, sequence.ppq, note.start)
            // the bow's way from the lower strings to the upper
            val break = math.min(
              ticks(breakSeconds),
              math.round((note.end - note.start) * breakShare).toInt
            )
            play.string match {
              case ChordString.Upper =>
                note.copy(
                  velocity = play.velocity,
                  start = note.start + break,
                  end = note.start + length
                )
              // the lower strings sound on while the upper ones set in
              case ChordString.Lower =>
                note.copy(
                  velocity = play.velocity,
                  end = note.start + math.max(1, math.min(length, 2 * break))
                )
              case _ =>
                note.copy(velocity = play.velocity, end = note.start + length)
            }
          } -> play
        }
        val breaths = wanted.collect {
          case ((tick, _), play) if play.breathBefore => tick
        }.toSet
        val breathed = lengthened.map((note, play) =>
          // the engraver lets a note go a tick or so before the next one starts
          (if (
             breaths.exists(at =>
               note.start < at && note.end > at - step && note.end <= at
             )
           ) {
             val length = note.end - note.start
             val breath = math.min(step, math.round(length * breathShare).toInt)
             note
               .copy(end = math.max(note.start + length / 2, note.end - breath))
           } else note)
          -> play
        )
        // each note with how its player draws it: an ornament's notes after
        // its first are slurred to it, a tremolo's each drawn alike
        val withOrnaments = breathed.flatMap { (note, play) =>
          // a broken chord's upper strings come in the bow of its lower ones
          // (the engines play them on the same lines, the backend's `VioletLines`)
          val technique = play.flatMap(each =>
            each.technique.map(text =>
              if (each.string == ChordString.Upper) inTheBow(text) else text
            )
          )
          play.flatMap(_.ornament).fold(Vector(note -> technique)) { ornament =>
            ornamented(
              note,
              ornament,
              step,
              sequence.ppq,
              ticksOf(secondsAQuarter, sequence.ppq, note.start)(
                ornamentSeconds
              )
            ).zipWithIndex.map { (played, index) =>
              val joined =
                index > 0 && !ornament.isInstanceOf[Ornament.Tremolo]
              played -> technique.map(text =>
                if (joined) text.split(" ").updated(1, "legato").mkString(" ")
                else text
              )
            }
          }
        }
        val techniques = withOrnaments
          .collect { case (note, Some(text)) => (note.start, text) }
          .distinct
          .map((tick, text) => MidiEvent(tick, textEvent(text)))
        others ++ techniques ++ withOrnaments.flatMap((note, _) =>
          Vector(
            MidiEvent(
              note.start,
              Vector(0x90 | note.channel, note.pitch, note.velocity)
            ),
            MidiEvent(note.end, Vector(0x90 | note.channel, note.pitch, 0))
          )
        )
      }
    })
  }

  /** The sequence with a "section" text event at each of `ticks`, where a part
    * of the score played at its own tempo starts (a movement, a new tempo): the
    * sound engines render each part on its own (the backend's `VioletLines`).
    */
  def sectioned(sequence: MidiSequence, ticks: Seq[Int]): MidiSequence =
    if (ticks.isEmpty || sequence.tracks.isEmpty) sequence
    else
      sequence.copy(tracks =
        sequence.tracks.updated(
          0,
          sequence.tracks.head ++ ticks.distinct
            .map(MidiEvent(_, textEvent("section")))
        )
      )

  // A bowing ("technique detache down") drawn in the bow of the note before,
  // its own stroke, a trill's or a pizzicato's, kept.
  private def inTheBow(text: String): String = {
    val words = text.split(" ")
    if (
      words
        .lift(1)
        .exists(Set("detache", "staccato", "spiccato", "volant", "loure"))
    )
      words.updated(1, "legato").mkString(" ")
    else text
  }

  // a text meta event, ASCII
  private def textEvent(text: String): Vector[Int] = {
    val bytes = text.map(_.toInt & 0x7f).toVector
    Vector(0xff, 0x01, bytes.size) ++ bytes
  }

  // a track's notes, each note-on with the next note-off of its pitch, and its
  // other events
  private def paired(
      events: Vector[MidiEvent]
  ): (Vector[MidiNote], Vector[MidiEvent]) = {
    val open = mutable.Map.empty[(Int, Int), List[MidiEvent]]
    val notes = Vector.newBuilder[MidiNote]
    val others = Vector.newBuilder[MidiEvent]
    for (event <- events) {
      val key = (event.data.head & 0x0f, event.data.lift(1).getOrElse(0))
      if (event.isNoteOn) {
        open(key) = open.getOrElse(key, Nil) :+ event
      } else if (event.isNoteOff && open.get(key).exists(_.nonEmpty)) {
        val start = open(key).head
        open(key) = open(key).tail
        notes += MidiNote(start.tick, event.tick, key._1, key._2, start.data(2))
      } else {
        others += event
      }
    }
    // a note-on never let go stays as it was
    others ++= open.values.flatten
    (notes.result(), others.result())
  }

  private def ornamented(
      note: MidiNote,
      ornament: Ornament,
      step: Int,
      ppq: Int,
      shortest: Int
  ): Vector[MidiNote] = {
    val length = note.end - note.start
    // the notes `pitches` `wanted` ticks each, never quicker than `shortest`:
    // a note too short for all of them keeps the first that fit, ending on the
    // written note
    def run(pitches: Seq[Int], wanted: Int): Vector[MidiNote] = {
      val each = math.max(1, math.max(wanted, shortest))
      val fitting = math.max(1, math.min(pitches.size, length / each))
      val played =
        if (fitting == pitches.size) pitches
        else pitches.take(fitting - 1) :+ 0
      val starts = played.indices.map(note.start + _ * each)
      played
        .zip(starts)
        .zipWithIndex
        .map { case ((pitch, start), index) =>
          val end = if (index == played.size - 1) note.end else start + each
          note.copy(start = start, end = end, pitch = note.pitch + pitch)
        }
        .toVector
    }
    ornament match {
      case Ornament.Trill(upper) if length >= 4 * step =>
        // from the note above, an even number of steps, then the note held
        // the rest of its time: the trill stops before the next note, heard
        // apart from it; a short note is trilled through, at least twice
        val count = math.max(4, (length * trilledShare / step).toInt / 2 * 2)
        run(
          (0 until count - 1).map(index => if (index % 2 == 0) upper else 0)
            :+ 0,
          step
        )
      case Ornament.Trill(upper) =>
        run(Seq(upper, 0, 0), math.max(1, length / 4))
      case Ornament.Tremblement(upper) =>
        run(Seq(upper, 0, upper, 0), math.max(1, math.min(step, length / 5)))
      case Ornament.Mordent(neighbour) =>
        run(Seq(0, neighbour, 0), math.max(1, math.min(step, length / 4)))
      case Ornament.Turn(upper, lower) =>
        run(Seq(upper, 0, lower, 0), math.max(1, math.min(step, length / 4)))
      case Ornament.Tremolo(quarters) =>
        val each = math.max(1, math.round(quarters * ppq).toInt)
        run(Seq.fill(math.max(1, length / each))(0), each)
    }
  }
}
