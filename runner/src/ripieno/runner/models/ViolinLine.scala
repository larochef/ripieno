package ripieno.runner.models

import java.util.Arrays
import scala.collection.mutable

/** A violin line's audio from its windows, as VIOLET's long-audio rendering
  * assembles them (`long_audio_test_step`), with the rendering's own rules:
  *   - each window is sampled again, eight times at most, while it comes out
  *     silent: as a whole (VIOLET's own check), or on some of its notes (a run
  *     of sixteenths or a held note left out, against the line's notes heard so
  *     far); the loudest attempt is kept when none is right;
  *   - a window with no note is silence, not sampled, once some window was;
  *   - each window is silenced at its edges, the windows are cross-faded by
  *     Hann windows, the line silenced at its edges again.
  *
  * `sample(window, attempt)` gives a window's audio, 48 kHz, one channel, the
  * attempts counted from 0; `windowDone` is told of each window assembled. One
  * instance renders the lines of one rendering.
  */
final class ViolinLine(
    sample: (ViolinWindow, Int) => Array[Float],
    windowDone: () => Unit = () => ()
) {
  import ViolinLine.*

  // the length of a window's audio, once a window was sampled
  private var windowLength = Option.empty[Int]

  def rendered(notes: Vector[ViolinNote]): Array[Float] = {
    val duration = ViolinWindows.duration(notes)
    val heardBefore = mutable.ArrayBuffer.empty[Double]
    val windows = ViolinWindows.windows(notes)
    val placed = windows.map { window =>
      val startTime = window.startFrame / ViolinWindows.FrameRate
      val chosen = attempted(window, notes, startTime, heardBefore)
      windowDone()
      (
        math.rint(startTime * SampleRate).toInt,
        masked(chosen, notes, startTime, ViolinWindows.WindowSeconds)
      )
    }
    val (lastStart, lastAudio) = placed.last
    val total = math.max(
      math.rint(duration * SampleRate).toInt,
      lastStart + lastAudio.length
    )
    masked(blended(placed, total), notes, 0.0, duration)
  }

  // the window's attempts, until one is not silent; the loudest when none is
  private def attempted(
      window: ViolinWindow,
      notes: Vector[ViolinNote],
      startTime: Double,
      heardBefore: mutable.ArrayBuffer[Double]
  ): Array[Float] = {
    var best = Double.NegativeInfinity
    var chosen = Array.emptyFloatArray
    var attempt = 0
    var done = false
    while (!done && attempt < Attempts) {
      val audio = windowLength
        .filter(_ => !window.notes.exists(_ != 0))
        .fold {
          val sampled = sample(window, attempt)
          windowLength = Some(sampled.length)
          sampled
        }(length => new Array[Float](length))
      val diagnosis = diagnosed(
        audio,
        notes,
        startTime,
        ViolinWindows.WindowSeconds,
        heardBefore
      )
      val score =
        if (diagnosis.expectedActivity) diagnosis.peakRms
        else Double.PositiveInfinity
      if (score > best) {
        best = score
        chosen = audio
      }
      if (!diagnosis.nearSilence) {
        chosen = audio
        done = true
      }
      attempt += 1
    }
    chosen
  }
}

object ViolinLine {

  val SampleRate = 48000
  private val Attempts = 8
  // seconds at a window's edges silenced (VIOLET's DEFAULT_EDGE_SILENCE_SECONDS)
  private val edgeSilence = 0.04
  // VIOLET's check of a silent window: its loudest 100 ms every 25 ms under
  // about -40 dBFS
  private val analysisWindow = 0.1
  private val analysisHop = 0.025
  private val silentPeak = 1e-2
  // a note this many dB under the window's typical note was left out
  private val silentBelow = 14.0
  // seconds VIOLET's sound is late by, and at a window's edges where its notes
  // are not judged
  private val late = 0.08
  private val notJudged = 0.3

  final private case class Diagnosis(
      expectedActivity: Boolean,
      peakRms: Double,
      nearSilence: Boolean
  )

  private def playable(notes: Vector[ViolinNote]): Vector[ViolinNote] =
    notes.filter(_.pitch >= ViolinWindows.LowestPitch)

  /** When the first note sounding in a span starts, from the span's start (0
    * for one already sounding); none when no note sounds.
    */
  private def firstOnset(
      notes: Vector[ViolinNote],
      start: Double,
      seconds: Double
  ): Option[Double] = {
    val end = start + seconds
    val sounding =
      playable(notes).filter(note => note.end > start && note.start < end)
    if (sounding.exists(_.start <= start)) Some(0.0)
    else sounding.map(_.start - start).minOption
  }

  private def meanSquare(audio: Array[Float], from: Int, until: Int): Double = {
    var sum = 0.0
    for (index <- from until until) sum += audio(index).toDouble * audio(index)
    sum / (until - from)
  }

  // VIOLET's diagnosis of a window's audio, then the rendering's own on its notes
  private def diagnosed(
      audio: Array[Float],
      notes: Vector[ViolinNote],
      startTime: Double,
      seconds: Double,
      heardBefore: mutable.ArrayBuffer[Double]
  ): Diagnosis = {
    val end = startTime + seconds
    val expectedActivity =
      playable(notes).exists(note => note.end > startTime && note.start < end)
    val onset = firstOnset(notes, startTime, seconds)
    val edge = math.rint(edgeSilence * SampleRate).toInt
    val startTrim = if (onset.forall(_ >= edgeSilence)) edge else 0
    val (from, until) =
      if (audio.length > startTrim + edge) (startTrim, audio.length - edge)
      else (0, audio.length)
    val frame = math.max(1, math.rint(analysisWindow * SampleRate).toInt)
    val hop = math.max(1, math.rint(analysisHop * SampleRate).toInt)
    val peakRms =
      if (until - from == 0) 0.0
      else if (until - from <= frame) math.sqrt(meanSquare(audio, from, until))
      else
        (from to until - frame by hop)
          .map(at => math.sqrt(meanSquare(audio, at, at + frame)))
          .max
    val levels = playable(notes).flatMap { note =>
      val (begin, finish) = (note.start - startTime, note.end - startTime)
      Option
        .when(begin >= notJudged && finish <= seconds - notJudged) {
          val first =
            math.min(((begin + late + 0.03) * SampleRate).toInt, audio.length)
          val last = math.min(
            (math.max(
              finish + late - 0.02,
              begin + late + 0.05
            ) * SampleRate).toInt,
            audio.length
          )
          Option.when(last > first)(
            10 * math.log10(meanSquare(audio, first, last) + 1e-12)
          )
        }
        .flatten
    }
    val silent =
      if (levels.size < 3) 0
      else {
        val typical =
          math.max(median(levels), median(heardBefore.toVector ++ levels))
        val count = levels.count(_ < typical - silentBelow)
        if (count == 0) heardBefore ++= levels
        count
      }
    val wholeSilent = expectedActivity && peakRms < silentPeak
    Diagnosis(
      expectedActivity,
      peakRms / (1 + silent),
      wholeSilent || silent > 0
    )
  }

  private def median(values: Vector[Double]): Double = {
    val sorted = values.sorted
    if (sorted.size % 2 == 1) sorted(sorted.size / 2)
    else (sorted(sorted.size / 2 - 1) + sorted(sorted.size / 2)) / 2
  }

  /** The audio silenced at its end, and at its start unless a note sounds from
    * it (`apply_eval_output_silence_mask`).
    */
  private def masked(
      audio: Array[Float],
      notes: Vector[ViolinNote],
      start: Double,
      seconds: Double
  ): Array[Float] = {
    val out = audio.clone()
    val edge = math.min(math.rint(edgeSilence * SampleRate).toInt, out.length)
    if (edge > 0) {
      Arrays.fill(out, out.length - edge, out.length, 0.0f)
      if (firstOnset(notes, start, seconds).forall(_ >= edgeSilence)) {
        Arrays.fill(out, 0, edge, 0.0f)
      }
    }
    out
  }

  /** The windows cross-faded: each weighted by a Hann window, but for the
    * first's start and the last's end, which have no neighbour.
    */
  private def blended(
      placed: Vector[(Int, Array[Float])],
      total: Int
  ): Array[Float] = {
    val size = placed.head._2.length
    val starts = placed.map(_._1)
    val hann = Array.tabulate(size)(n =>
      (0.5 - 0.5 * math.cos(2 * math.Pi * n / (size - 1))).toFloat
    )
    def weight(window: Int, n: Int): Float =
      if (placed.size == 1) 1.0f
      else if (
        window == 0 && n < math.max(0, math.min(size, starts(1) - starts(0)))
      ) 1.0f
      else if (
        window == placed.size - 1 &&
        n >= math.max(
          0,
          math.min(size, starts(starts.size - 2) + size - starts.last)
        )
      ) 1.0f
      else hann(n)
    val sum = new Array[Float](total)
    val weights = new Array[Float](total)
    for {
      ((start, audio), window) <- placed.zipWithIndex
      n <- 0 until size
    } {
      val w = weight(window, n)
      sum(start + n) += audio(n) * w
      weights(start + n) += w
    }
    Array.tabulate(total)(index => sum(index) / math.max(weights(index), 1e-8f))
  }
}
