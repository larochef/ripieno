package ripieno.backend.review

import ripieno.backend.models.Downloaded
import ripieno.backend.sound.*
import ripieno.shared.*

import java.io.File
import java.nio.file.{Files, Path}
import scala.util.Random

import utest.*

/** The review of performances made on purpose: a cello part of 40 seconds
  * (arpeggios in sixteenths, p then f) played by FluidSynth with the page's
  * sound bank (downloaded first into Ripieno's cache when it is not there), as
  * written and with one fault of each kind put in. As written, nothing is said
  * of its pitches, tempo or dynamics; with the faults, the note 30 cents high,
  * the wrong note, the 15 % acceleration and the flat dynamics are found. Not
  * found: the note left out, a note late by a fraction of a sixteenth; and a
  * note of the accelerated measures comes out 25 cents high.
  *
  * Then with a piano accompaniment heard from speakers across a room (later,
  * with an echo, from 6 dB under the cello to 6 dB over), the review told what
  * it plays: tempo and dynamics are found as without it. Its pitch verdicts are
  * printed, not checked: under such a piano they are not the cello's alone,
  * faults lost and others said. Needs FluidSynth.
  */
object PerformanceReviewTests extends TestSuite {

  private val Seconds = 40.0
  private val Rate = ReviewFeatures.Rate
  private val Sixteenth = 0.125
  private val Bar = 16 * Sixteenth
  private val Cello = 42

  // a bar's arpeggio, eight notes twice, one a bar in turn
  private val Arpeggios = Vector(
    Vector(43, 50, 59, 57, 59, 50, 59, 50),
    Vector(43, 52, 60, 59, 60, 52, 60, 52),
    Vector(43, 54, 60, 59, 60, 54, 60, 54),
    Vector(43, 55, 59, 57, 59, 55, 59, 54)
  )
  // the piano's chords under them, a bar each
  private val Chords =
    Vector(
      Vector(67, 71, 74),
      Vector(67, 72, 76),
      Vector(66, 69, 74),
      Vector(67, 71, 74)
    )

  private val part: Vector[MidiNote] =
    (0 until (Seconds / Sixteenth).toInt).toVector.map { k =>
      val bar = k / 16
      MidiNote(
        64,
        Arpeggios(bar % 4)(k % 8),
        k * Sixteenth,
        (k + 1) * Sixteenth
      )
    }

  private val accompaniment: Vector[MidiNote] =
    (0 until (Seconds / Bar).toInt).toVector.flatMap(bar =>
      Chords(bar % 4).map(pitch =>
        MidiNote(70, pitch, bar * Bar, (bar + 1) * Bar)
      )
    )

  private val half = Seconds / 2

  private val reference = PerformanceReference(
    notes = part.zipWithIndex.toList.map((note, k) =>
      ReferenceNote(
        s"n$k",
        math.rint(note.start * 1000),
        math.rint((note.end - note.start) * 1000),
        List(note.pitch),
        s"${(note.start / Bar).toInt + 1}"
      )
    ),
    measures = (0 until (Seconds / Bar).toInt + 2).toList.map(k =>
      ReferenceMeasure(s"${k + 1}", math.rint(k * Bar * 1000))
    ),
    beatMillis = 500,
    markings = List(
      Marking("dynamics", "p", "1", 0, None),
      Marking("dynamics", "f", s"${(half / Bar).toInt + 1}", half * 1000, None)
    )
  )

  private def soundBank: Path = Downloaded.soundBank

  private val hasFluidSynth =
    sys.env
      .getOrElse("PATH", "")
      .split(File.pathSeparator)
      .exists(folder => Files.isExecutable(Path.of(folder, "fluidsynth")))

  /** The instruments played by FluidSynth, one channel at the review's rate. */
  private def played(instruments: MidiInstrument*): Array[Float] = {
    val folder = Files.createTempDirectory("performance")
    try {
      val (midi, wav) =
        (folder.resolve("played.mid"), folder.resolve("played.wav"))
      Files.write(midi, MidiFile(instruments.toVector, Vector.empty).bytes)
      val fluidsynth = new ProcessBuilder(
        "fluidsynth",
        "-ni",
        "-g",
        "0.8",
        "-r",
        s"$Rate",
        "-F",
        wav.toString,
        soundBank.toString,
        midi.toString
      ).redirectErrorStream(true).start()
      fluidsynth.getInputStream.readAllBytes()
      assert(fluidsynth.waitFor() == 0)
      Wav.read(Files.readAllBytes(wav)).mono
    } finally {
      Files.list(folder).forEach(Files.delete)
      Files.delete(folder)
    }
  }

  private def reviewed(
      audio: Array[Float],
      told: Option[Accompaniment] = None
  ): PerformanceReview = {
    val notes = reference.notes.toVector.sortBy(_.onset)
    val review = PerformanceAnalysis.review(
      RecordingFeatures.of(audio, notes, told),
      reference.copy(accompaniment = told)
    )
    assert(review.isRight)
    review.toOption.get
  }

  // the notes given each verdict other than right, and each rhythm
  private def flagged(review: PerformanceReview): Map[String, Set[String]] =
    (review.notes.filter(_.verdict != "right").map(n => n.verdict -> n.id) ++
      review.notes.flatMap(n => n.rhythm.map(_ -> n.id)))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap

  private def described(name: String, review: PerformanceReview): Unit = {
    println(
      s"  $name: ${flagged(review).toSeq.sortBy(_._1).map((k, v) => s"$k ${v.toSeq.sortBy(_.drop(1).toInt).mkString(",")}").mkString("; ")}"
    )
    println(
      s"    drifts ${review.tempo.drifts}; weak changes ${review.dynamics.weakChanges
          .map(w => (w.from, w.to, w.heardDb))}"
    )
  }

  // the faults put in, by note
  private val firstHalf = part.count(_.start < half)
  private val Seq(wrong, around, late, missing) =
    (1 to 4).map(firstHalf * _ / 5)
  // the note played out of tune: one of the upper voice, long enough for its
  // pitch to be measured (a bass note of a sixteenth is not)
  private val sharp = part.indexWhere(_.pitch >= 57, around)
  private val rampFrom = half + 2 * Bar
  private val rampTo = rampFrom + 3 * Bar

  private def clean: Array[Float] =
    played(
      MidiInstrument(
        Cello,
        false,
        "",
        part.map(n => n.copy(velocity = if (n.start < half) 45 else 115))
      )
    )

  private def faulty: Array[Float] = {
    // 15 % faster from rampFrom to rampTo, then back to tempo
    def warp(t: Double) =
      if (t <= rampFrom) t
      else
        rampFrom + (math.min(t, rampTo) - rampFrom) / 1.15 + math.max(
          0,
          t - rampTo
        )
    // late by under half its length: a note out of its place entirely is a missed one
    val shift = math.min(0.12, 0.45 * (part(late).end - part(late).start))
    val notes = part.zipWithIndex.flatMap { (note, k) =>
      val changed =
        if (k == wrong) note.copy(pitch = note.pitch + 2)
        else if (k == late) note.copy(start = note.start + shift)
        else if (k == late - 1) note.copy(end = note.end + shift)
        else note
      Option.when(k != missing)(
        changed.copy(
          velocity = 80,
          start = warp(changed.start),
          end = warp(changed.end)
        )
      )
    }
    // one note 30 cents high, on a channel of its own: a bend is the whole
    // channel's, and would put the end of the note before out of tune too
    // (the bend range two semitones)
    val outOfTune = notes(sharp - (if (sharp > missing) 1 else 0))
    played(
      MidiInstrument(Cello, false, "", notes.filterNot(_ eq outOfTune)),
      MidiInstrument(
        Cello,
        false,
        "",
        Vector(outOfTune),
        bends = Vector(MidiBend((8191 * 0.30 / 2).toInt, 0))
      )
    )
  }

  private val pitchVerdicts = Set("intonation", "wrong", "missed", "uncertain")

  // the faulty performance's tempo and dynamics, found
  private def tempoAndDynamicsFound(review: PerformanceReview): Unit = {
    assert(
      review.tempo.drifts.exists(drift =>
        drift.kind == "faster" && drift.fromMeasure == s"${(rampFrom / Bar).toInt + 1}" &&
          drift.toMeasure == s"${(rampTo / Bar).toInt}" && drift.percent >= 10 && drift.percent <= 20
      )
    )
    assert(
      review.dynamics.weakChanges.exists(weak =>
        weak.from.contains("p") && weak.to.contains("f") && weak.heardDb < 3
      )
    )
  }

  // the clean performance's: nothing to say
  private def tempoAndDynamicsClean(review: PerformanceReview): Unit =
    assert(review.tempo.drifts.isEmpty, review.dynamics.weakChanges.isEmpty)

  val tests: Tests = Tests {
    test("the faults put in a performance, found where they are") {
      if (!hasFluidSynth) println("  skipped: no FluidSynth")
      else {
        val (asWritten, withFaults) = (reviewed(clean), reviewed(faulty))
        described("clean", asWritten)
        described("faulty", withFaults)
        // as written: no pitch faulted, a rhythm one at most at its ends
        assert(flagged(asWritten).keySet.intersect(pitchVerdicts).isEmpty)
        tempoAndDynamicsClean(asWritten)
        // the note out of tune and the wrong note are found; the note left
        // out and the one late by a fraction of a sixteenth are not
        val found = flagged(withFaults)
        assert(found.getOrElse("intonation", Set.empty).contains(s"n$sharp"))
        assert(
          pitchVerdicts.exists(verdict =>
            found.getOrElse(verdict, Set.empty).contains(s"n$wrong")
          )
        )
        tempoAndDynamicsFound(withFaults)
      }
    }
    test("with the accompaniment on speakers, tempo and dynamics as without") {
      if (!hasFluidSynth) println("  skipped: no FluidSynth")
      else {
        val random = Random(3)
        val piano = played(MidiInstrument(0, false, "", accompaniment))
        // heard across the room: later, with an echo
        val delay = (0.08 * Rate).toInt
        val taps: Seq[(Int, Double)] = (0 until 40).map { _ =>
          val at = random.between(0.01, 0.5)
          (at * Rate).toInt -> random.nextGaussian() * math.exp(-6.9 * at / 0.5)
        }
        val norm = math.sqrt(taps.map(t => t._2 * t._2).sum)
        val room = new Array[Float](piano.length + delay + (0.5 * Rate).toInt)
        for (n <- piano.indices) {
          room(n + delay) += piano(n)
          for ((at, gain) <- taps)
            room(n + delay + at) += (piano(n) * 0.35 * gain / norm).toFloat
        }
        val told = Accompaniment(
          true,
          0,
          1.0,
          accompaniment.toList.map(n =>
            AccompanimentNote(
              math.rint(n.start * 1000),
              math.rint((n.end - n.start) * 1000),
              n.pitch
            )
          )
        )
        def rms(x: Array[Float]) = {
          val loud = x.filter(v => math.abs(v) > 1e-4);
          math.sqrt(loud.map(v => v.toDouble * v).sum / loud.length)
        }
        for (
          (name, solo, checked) <- Seq(
            ("clean", clean, tempoAndDynamicsClean),
            ("faulty", faulty, tempoAndDynamicsFound)
          )
        ) {
          described(s"$name alone", reviewed(solo))
          for (decibels <- Seq(-6, 0, 6)) {
            val gain = rms(solo) / rms(room) * math.pow(10, decibels / 20.0)
            val mix = Array.tabulate(math.max(solo.length, room.length))(n =>
              (if (n < solo.length) solo(n)
               else 0f) + (if (n < room.length) room(n) * gain else 0.0).toFloat
            )
            val peak = mix.map(math.abs).max
            val heard = reviewed(
              mix.map(v => (v / math.max(1.0, peak / 0.9)).toFloat),
              Some(told)
            )
            // the pitch verdicts, said only: under a piano as loud as the
            // cello they are not those of the cello alone
            described(f"$name, piano $decibels%+d dB, told", heard)
            checked(heard)
          }
        }
      }
    }
  }
}
