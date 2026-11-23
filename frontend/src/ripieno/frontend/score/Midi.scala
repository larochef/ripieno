package ripieno.frontend.score

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.AppResult

import scala.scalajs.js.typedarray.Uint8Array

/** What the page rewrites of the MIDI an engraving plays. */
object Midi {

  private val headerLength = 14
  private val chunkHeaderLength = 8

  /** The MIDI with only those parts, counted in score order, of `staves` staves
    * each. Verovio writes a first track with the tempo, then one track per
    * staff (two for a piano): leaving tracks out changes no time.
    */
  def onlyParts(
      midi: Uint8Array,
      parts: Set[Int],
      staves: List[Int]
  ): AppResult[Uint8Array] = {
    val tracks = chunks(midi)
    if (tracks.sizeIs != staves.sum + 1) {
      Left(
        Tr(
          s"the MIDI has ${tracks.size} tracks for ${staves.sum} staves",
          s"le MIDI a ${tracks.size} pistes pour ${staves.sum} portées"
        )
      )
    } else {
      // the part each track after the first belongs to
      val owners =
        staves.zipWithIndex.flatMap((count, part) => List.fill(count)(part))
      val kept = tracks.head +: tracks.tail.zip(owners).collect {
        case (track, part) if parts.contains(part) => track
      }
      val result = new Uint8Array(headerLength + kept.map(_.length).sum)
      result.set(midi.subarray(0, headerLength))
      result(10) = (kept.size >> 8).toShort
      result(11) = (kept.size & 0xff).toShort
      kept.foldLeft(headerLength) { (offset, track) =>
        result.set(track, offset)
        offset + track.length
      }
      Right(result)
    }
  }

  /** The MIDI at `rate` times its tempo: every tempo of its first track, where
    * Verovio writes them, made that much faster. A rendering made from it plays
    * at that tempo with no stretching of its sound.
    */
  def atRate(midi: Uint8Array, rate: Double): Uint8Array = {
    val result = new Uint8Array(midi.length)
    result.set(midi)
    chunks(midi).headOption.foreach { tempoTrack =>
      val start = tempoTrack.byteOffset - midi.byteOffset
      // a tempo event: FF 51 03 and three bytes of microseconds a quarter note
      for {
        at <- start + chunkHeaderLength until start + tempoTrack.length - 5
        if midi(at) == 0xff && midi(at + 1) == 0x51 && midi(at + 2) == 0x03
      } {
        val micros =
          (midi(at + 3) << 16) | (midi(at + 4) << 8) | midi(at + 5)
        val faster = math.max(1, math.min(0xffffff, math.round(micros / rate)))
        result(at + 3) = ((faster >> 16) & 0xff).toShort
        result(at + 4) = ((faster >> 8) & 0xff).toShort
        result(at + 5) = (faster & 0xff).toShort
      }
    }
    result
  }

  // each track with its chunk header
  private def chunks(midi: Uint8Array): List[Uint8Array] = {
    def lengthAt(offset: Int): Int =
      (0 until 4).foldLeft(0)((length, index) =>
        (length << 8) | midi(offset + 4 + index)
      )
    Iterator
      .unfold(headerLength) { offset =>
        Option.when(offset + chunkHeaderLength <= midi.length) {
          val end = offset + chunkHeaderLength + lengthAt(offset)
          (midi.subarray(offset, end), end)
        }
      }
      .toList
  }
}
