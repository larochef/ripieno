package ripieno.backend.sound

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

/** A note, in seconds. */
final case class MidiNote(velocity: Int, pitch: Int, start: Double, end: Double)

/** A text event ("technique legato up", "section"), in seconds. */
final case class MidiText(text: String, time: Double)

final case class MidiControl(number: Int, value: Int, time: Double)

final case class MidiBend(pitch: Int, time: Double)

final case class MidiTimeSignature(
    numerator: Int,
    denominator: Int,
    time: Double
)

/** The notes one program plays on one channel of one track. */
final case class MidiInstrument(
    program: Int,
    isDrum: Boolean,
    name: String,
    notes: Vector[MidiNote],
    controls: Vector[MidiControl] = Vector.empty,
    bends: Vector[MidiBend] = Vector.empty
)

/** How ticks are seconds: from each tick on, the seconds a tick lasts; and the
  * last tick a file read had (`MidiFile.read`), up to which a time is written
  * back at the nearest tick.
  */
final case class MidiTiming(
    resolution: Int,
    scales: Vector[(Int, Double)],
    lastTick: Int
) {

  // the time at each tick where a scale starts
  private val starts: Vector[Double] =
    scales.indices
      .foldLeft(Vector.empty[Double]) { (found, index) =>
        if (index == 0) Vector(0.0)
        else {
          val (from, scale) = scales(index - 1)
          found :+ (found.last + scale * (scales(index)._1 - from))
        }
      }

  def seconds(tick: Int): Double = {
    val index = math.max(0, scales.lastIndexWhere(_._1 <= tick))
    starts(index) + scales(index)._2 * (tick - scales(index)._1)
  }

  /** The tick nearest a time: on the ticks the file had, the later of two as
    * near; past them, at the last tempo.
    */
  def tick(time: Double): Int = {
    val last = math.max(lastTick, scales.map(_._1).max)
    if (time > seconds(last)) {
      val ticks = last + (time - seconds(last)) / scales.last._2
      math.rint(ticks).toInt
    } else {
      // the first tick at or after the time
      var low = 0
      var high = last
      while (low < high) {
        val middle = (low + high) / 2
        if (seconds(middle) < time) low = middle + 1 else high = middle
      }
      if (
        low > 0 &&
        math.abs(time - seconds(low - 1)) < math.abs(time - seconds(low))
      ) low - 1
      else low
    }
  }
}

object MidiTiming {

  /** A file made from nothing: 220 ticks a quarter, at 120 a minute. */
  val fresh: MidiTiming =
    MidiTiming(220, Vector(0 -> 60.0 / (120.0 * 220)), 0)
}

/** A standard MIDI file as pretty_midi reads and writes it (the engines' code
  * was written against it, and its tests checked against it): its instruments,
  * by program, channel and track; the text events of all its tracks; times in
  * seconds by the tempi of its first track. A file written and read again has
  * its times at its ticks, as the engines read it.
  */
final case class MidiFile(
    instruments: Vector[MidiInstrument],
    texts: Vector[MidiText],
    timing: MidiTiming = MidiTiming.fresh,
    timeSignatures: Vector[MidiTimeSignature] = Vector.empty
) {

  def bytes: Array[Byte] = MidiFile.written(this)

  /** When its last event is, as pretty_midi tells it. */
  def endTime: Double =
    (instruments.flatMap(instrument =>
      instrument.notes.map(_.end) ++ instrument.bends.map(_.time) ++
        instrument.controls.map(_.time)
    ) ++ timeSignatures.map(_.time) ++ texts.map(_.time) ++
      timing.scales.map((tick, _) => timing.seconds(tick))).maxOption
      .getOrElse(0.0)
}

object MidiFile {

  private val drumChannel = 9

  final private case class Event(tick: Int, data: Vector[Int])

  def read(bytes: Array[Byte]): MidiFile = {
    val data = bytes.map(_ & 0xff)
    def word(at: Int, size: Int) =
      (0 until size).foldLeft(0)((sum, index) => (sum << 8) | data(at + index))
    val resolution = word(12, 2)
    val tracks = Vector.newBuilder[Vector[Event]]
    var offset = 8 + word(4, 4)
    while (offset + 8 <= data.length) {
      val length = word(offset + 4, 4)
      if (new String(bytes, offset, 4, StandardCharsets.ISO_8859_1) == "MTrk") {
        tracks += events(data, offset + 8, offset + 8 + length)
      }
      offset += 8 + length
    }
    loaded(resolution, tracks.result())
  }

  private def events(data: Array[Int], from: Int, to: Int): Vector[Event] = {
    val found = Vector.newBuilder[Event]
    var at = from
    var tick = 0
    var status = 0
    def number(): Int = {
      var value = 0
      var more = true
      while (more) {
        value = (value << 7) | (data(at) & 0x7f)
        more = (data(at) & 0x80) != 0
        at += 1
      }
      value
    }
    while (at < to) {
      tick += number()
      if (data(at) >= 0x80) {
        status = data(at)
        at += 1
      }
      if (status == 0xff) {
        val kind = data(at)
        at += 1
        val length = number()
        found += Event(tick, Vector(0xff, kind) ++ data.slice(at, at + length))
        at += length
      } else if (status == 0xf0 || status == 0xf7) {
        val length = number()
        at += length
      } else {
        val size =
          if ((status & 0xf0) == 0xc0 || (status & 0xf0) == 0xd0) 1 else 2
        found += Event(tick, status +: data.slice(at, at + size).toVector)
        at += size
      }
    }
    found.result()
  }

  private def isMeta(event: Event, kind: Int) =
    event.data.head == 0xff && event.data(1) == kind

  private def text(event: Event) =
    new String(
      event.data.drop(2).map(_.toByte).toArray,
      StandardCharsets.ISO_8859_1
    )

  private def loaded(
      resolution: Int,
      tracks: Vector[Vector[Event]]
  ): MidiFile = {
    val scales = tracks.headOption.toVector.flatten
      .filter(isMeta(_, 0x51))
      .foldLeft(Vector(0 -> 60.0 / (120.0 * resolution))) { (found, event) =>
        val micros = event.data.drop(2).foldLeft(0)((sum, b) => (sum << 8) | b)
        val scale = 60.0 / ((6e7 / micros) * resolution)
        if (event.tick == 0) Vector(0 -> scale)
        else if (scale != found.last._2) found :+ (event.tick -> scale)
        else found
      }
    val lastTick = tracks.flatMap(_.map(_.tick)).maxOption.getOrElse(0) + 1
    val timing = MidiTiming(resolution, scales, lastTick)
    val signatures = tracks.headOption.toVector.flatten
      .filter(isMeta(_, 0x58))
      .map(event =>
        MidiTimeSignature(
          event.data(2),
          1 << event.data(3),
          timing.seconds(event.tick)
        )
      )
    // the texts of all tracks, by time, a track's before the next one's
    val texts = tracks.zipWithIndex
      .flatMap((events, track) =>
        events.filter(isMeta(_, 0x01)).map(event => (event, track))
      )
      .sortBy((event, _) => timing.seconds(event.tick))
      .map((event, _) => MidiText(text(event), timing.seconds(event.tick)))
    MidiFile(instrumentsOf(tracks, timing), texts, timing, signatures)
  }

  // pretty_midi's instruments: one per program, channel and track, made when
  // its first note ends, given the controls and bends its channel had before
  private def instrumentsOf(
      tracks: Vector[Vector[Event]],
      timing: MidiTiming
  ): Vector[MidiInstrument] = {
    final class Building(
        val program: Int,
        val isDrum: Boolean,
        val name: String
    ) {
      val notes = Vector.newBuilder[MidiNote]
      var controls = Vector.empty[MidiControl]
      var bends = Vector.empty[MidiBend]
    }
    val made = scala.collection.mutable.LinkedHashMap
      .empty[(Int, Int, Int), Building]
    val stragglers = scala.collection.mutable.Map.empty[(Int, Int), Building]
    var names = Map.empty[Int, String]
    def instrument(program: Int, channel: Int, track: Int, create: Boolean) =
      made.get((program, channel, track)) match {
        case Some(found)                                              => found
        case None if !create && stragglers.contains((channel, track)) =>
          stragglers((channel, track))
        case None if create =>
          val building = Building(
            program,
            channel == drumChannel,
            names.getOrElse(track, "")
          )
          stragglers.get((channel, track)).foreach { straggler =>
            building.controls = straggler.controls
            building.bends = straggler.bends
          }
          made((program, channel, track)) = building
          building
        case None =>
          val straggler = Building(program, false, names.getOrElse(track, ""))
          stragglers((channel, track)) = straggler
          straggler
      }
    for ((events, track) <- tracks.zipWithIndex) {
      val open =
        scala.collection.mutable.Map.empty[(Int, Int), Vector[(Int, Int)]]
      val programs = Array.fill(16)(0)
      for (event <- events) {
        val status = event.data.head
        val kind = status & 0xf0
        val channel = status & 0x0f
        if (isMeta(event, 0x03)) {
          names += track -> text(event)
        } else if (status < 0xf0 && kind == 0xc0) {
          programs(channel) = event.data(1)
        } else if (status < 0xf0 && kind == 0x90 && event.data(2) > 0) {
          val key = (channel, event.data(1))
          open(key) = open.getOrElse(key, Vector.empty) :+
            (event.tick, event.data(2))
        } else if (status < 0xf0 && (kind == 0x80 || kind == 0x90)) {
          val key = (channel, event.data(1))
          open.get(key).foreach { notes =>
            val (closed, kept) = notes.partition(_._1 != event.tick)
            for ((start, velocity) <- closed) {
              instrument(
                programs(channel),
                channel,
                track,
                create = true
              ).notes +=
                MidiNote(
                  velocity,
                  event.data(1),
                  timing.seconds(start),
                  timing.seconds(event.tick)
                )
            }
            if (closed.nonEmpty && kept.nonEmpty) open(key) = kept
            else open.remove(key)
          }
        } else if (status < 0xf0 && kind == 0xe0) {
          val pitch = ((event.data(2) << 7) | event.data(1)) - 8192
          val building =
            instrument(programs(channel), channel, track, create = false)
          building.bends :+= MidiBend(pitch, timing.seconds(event.tick))
        } else if (status < 0xf0 && kind == 0xb0) {
          val building =
            instrument(programs(channel), channel, track, create = false)
          building.controls :+= MidiControl(
            event.data(1),
            event.data(2),
            timing.seconds(event.tick)
          )
        }
      }
    }
    made.values.toVector.map(building =>
      MidiInstrument(
        building.program,
        building.isDrum,
        building.name,
        building.notes.result(),
        building.controls,
        building.bends
      )
    )
  }

  // pretty_midi's writing: a first track of tempi, time signatures and texts,
  // then a track an instrument, its events in order of tick and, at a tick, of
  // kind, pitch and velocity (a note's end before a note's start of its pitch)
  private def written(file: MidiFile): Array[Byte] = {
    val timing = file.timing
    def tick(time: Double) = timing.tick(time)
    val defaultSignature =
      file.timeSignatures.isEmpty || file.timeSignatures.map(_.time).min > 0
    val first = (
      (if (defaultSignature) Vector((0, 2, timeSignature(4, 4)))
       else Vector()) ++
        timing.scales.map((at, scale) =>
          (at, 1, tempo((6e7 / (60.0 / (scale * timing.resolution))).toInt))
        ) ++
        file.timeSignatures.map(signature =>
          (
            tick(signature.time),
            2,
            timeSignature(signature.numerator, signature.denominator)
          )
        ) ++
        file.texts.map(event => (tick(event.time), 5, meta(0x01, event.text)))
    ).zipWithIndex
      .sortBy { case ((at, rank, _), index) => (at, rank, index) }
      .map { case ((at, _, data), _) => Event(at, data) }
    val channels = (0 until 16).filterNot(_ == drumChannel)
    val instrumentTracks = file.instruments.zipWithIndex.map {
      (instrument, index) =>
        val channel =
          if (instrument.isDrum) drumChannel
          else channels(index % channels.size)
        val named =
          if (instrument.name.nonEmpty)
            Vector((0, -1, meta(0x03, instrument.name)))
          else Vector()
        val events = (
          Vector((0, 6 << 16, Vector(0xc0 | channel, instrument.program))) ++
            instrument.notes.flatMap(note =>
              Vector(
                (
                  tick(note.start),
                  (10 << 16) + (note.pitch << 8) + note.velocity,
                  Vector(0x90 | channel, note.pitch, note.velocity)
                ),
                (
                  tick(note.end),
                  (10 << 16) + (note.pitch << 8),
                  Vector(0x90 | channel, note.pitch, 0)
                )
              )
            ) ++
            instrument.bends.map(bend =>
              (
                tick(bend.time),
                (7 << 16) + bend.pitch,
                Vector(
                  0xe0 | channel,
                  (bend.pitch + 8192) & 0x7f,
                  ((bend.pitch + 8192) >> 7) & 0x7f
                )
              )
            ) ++
            instrument.controls.map(control =>
              (
                tick(control.time),
                (8 << 16) + (control.number << 8) + control.value,
                Vector(0xb0 | channel, control.number, control.value)
              )
            )
        ).zipWithIndex
          .sortBy { case ((at, rank, _), order) => (at, rank, order) }
          .map { case ((at, _, data), _) => Event(at, data) }
        named.map((at, _, data) => Event(at, data)) ++ zeroLengthsSwapped(
          events
        )
    }
    val output = new ByteArrayOutputStream()
    val tracks = (first +: instrumentTracks).map(withEnd)
    output.write(
      Array(0x4d, 0x54, 0x68, 0x64, 0, 0, 0, 6, 0, 1).map(_.toByte)
    )
    output.write(Array(tracks.size >> 8, tracks.size & 0xff).map(_.toByte))
    output.write(
      Array(timing.resolution >> 8, timing.resolution & 0xff).map(_.toByte)
    )
    tracks.foreach(track => output.write(chunk(track)))
    output.toByteArray
  }

  // a note starting and ending at one tick: its start first
  private def zeroLengthsSwapped(events: Vector[Event]): Vector[Event] = {
    val swapped = events.toArray
    for (index <- 0 until swapped.length - 1) {
      val (one, next) = (swapped(index), swapped(index + 1))
      if (
        one.tick == next.tick && isNote(one) && isNote(next) &&
        one.data(1) == next.data(1) && one.data(2) != 0 && next.data(2) == 0
      ) {
        swapped(index) = next
        swapped(index + 1) = one
      }
    }
    swapped.toVector
  }

  private def isNote(event: Event) = (event.data.head & 0xf0) == 0x90

  private def withEnd(events: Vector[Event]): Vector[Event] =
    events :+ Event(events.lastOption.fold(0)(_.tick) + 1, Vector(0xff, 0x2f))

  private def meta(kind: Int, text: String): Vector[Int] =
    Vector(0xff, kind) ++ text
      .getBytes(StandardCharsets.ISO_8859_1)
      .map(_ & 0xff)

  private def tempo(micros: Int): Vector[Int] =
    Vector(
      0xff,
      0x51,
      (micros >> 16) & 0xff,
      (micros >> 8) & 0xff,
      micros & 0xff
    )

  private def timeSignature(numerator: Int, denominator: Int): Vector[Int] =
    Vector(
      0xff,
      0x58,
      numerator,
      Integer.numberOfTrailingZeros(denominator),
      24,
      8
    )

  private def chunk(events: Vector[Event]): Array[Byte] = {
    val body = new ByteArrayOutputStream()
    var last = 0
    for (event <- events) {
      variable(event.tick - last).foreach(body.write)
      last = event.tick
      if (event.data.head == 0xff) {
        body.write(0xff)
        body.write(event.data(1))
        val content = event.data.drop(2)
        variable(content.size).foreach(body.write)
        content.foreach(body.write)
      } else {
        event.data.foreach(body.write)
      }
    }
    val bytes = body.toByteArray
    Array(0x4d, 0x54, 0x72, 0x6b).map(_.toByte) ++
      (0 until 4).reverse.map(byte =>
        ((bytes.length >> (8 * byte)) & 0xff).toByte
      ) ++
      bytes
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
