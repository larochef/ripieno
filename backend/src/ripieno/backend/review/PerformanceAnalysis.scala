package ripieno.backend.review

import ripieno.backend.review.ReviewNumerics.*
import ripieno.shared.*

/** A recording of one part reviewed against its score, from what is heard in it
  * (`ReviewFeatures`). The recording is aligned with the score
  * (`ScoreAlignment`), and then each verdict is given on its own, so that one
  * fault is not reported four times:
  *   - pitch: how far, in cents, each note is from the one written. Far enough
  *     to be another note, it is a wrong note; less, a matter of intonation;
  *   - missed notes: nothing sounds where they are written;
  *   - rhythm: a note starting early or late against the player's own tempo
  *     there, not the written one;
  *   - tempo: the player's tempo drifting away from their tempo over the whole
  *     piece, where the score asks for no change (rit., accel. and the like);
  *   - dynamics: markings asking for louder or softer that are not heard as
  *     such. Levels are relative: a microphone's gain is not known.
  *
  * The alignment itself is given too, so that the page can follow the recording
  * on the score.
  */
object PerformanceAnalysis {
  import ReviewFeatures.*

  // this far from the note written, the note heard is another one; from
  // `intonationCents` to it, a matter of intonation
  private val wrongNoteCents = 50
  private val intonationCents = 25
  // a tuning of the player's own is only reckoned from this many notes
  private val tuningLeastNotes = 8
  private val silenceBelowDb = 15
  // a note is heard if its salience reaches this share of the most salient
  // pitch, unless a pitch with no business there is `outweighed` times more
  // salient, and is wrong if, instead, such a pitch reaches `wrongPresent`
  private val presentForVerdict = 0.15
  private val outweighed = 0.4
  private val wrongPresent = 0.5
  // a note whose salience is at least this, against the most salient pitch, is heard
  private val present = 0.5
  // an onset off by more than this share of a beat, against the local tempo,
  // and by more than this, is a rhythm error
  private val rhythmShareOfBeat = 0.15
  private val rhythmLeastMillis = 70
  // a note has begun once its pitch holds this many frames
  private val pitchHoldFrames = 3
  // seconds an aligned onset may move to the attack heard, or this share of
  // its length, for a note repeating the one before
  private val onsetReach = 0.07
  private val repeatedReachShare = 0.2
  // the local tempo this far from the overall one is a drift, when it lasts
  // that many beats
  private val tempoDrift = 0.10
  private val tempoDriftLeastBeats = 4
  // the local tempo is the player's over that many beats, or over this many
  // notes where that many beats hold fewer
  private val tempoWindowBeats = 4
  private val tempoLeastNotes = 8
  // less than this between markings asking for a change is not enough
  private val dynamicContrastDb = 3
  private val dynamics = Vector("ppp", "pp", "p", "mp", "mf", "f", "ff", "fff")
  // what the pitch tracker mistakes a note for: an octave or two off, an
  // octave and a fifth below
  private val trackerSlips = Vector(0, 12, -12, 24, -24, 19, -19)
  private val tempoChange =
    "(?i)\\b(rit|rall|allarg|accel|string|stretto|calando|morendo|smorz|ten)".r
  private val tempoBack = "(?i)\\b(a tempo|tempo i|tempo primo)\\b".r

  // a note's review as it is being made
  final private case class Reviewed(
      verdict: String,
      cents: Option[Double] = None,
      heard: Option[Double] = None,
      rhythm: Option[String] = None,
      onsetMillis: Option[Int] = None
  )

  def review(
      features: ReviewFeatures,
      reference: PerformanceReference
  ): Either[String, PerformanceReview] = {
    val all = reference.notes.toVector.sortBy(_.onset)
    if (all.isEmpty) return Left("the part has no notes to review")
    val beat = reference.beatMillis / 1000
    val (scoreSeconds, recordingSeconds) =
      ScoreAlignment.aligned(features, all, entry(reference, all))
    // the notes after where the player stopped are not reviewed
    val notes = all.filter(_.onset / 1000 <= scoreSeconds.last)
    if (notes.size < 4)
      return Left("too little of the part was heard to review it")
    val aligned = (seconds: Double) =>
      interpolated(seconds, scoreSeconds, recordingSeconds)
    val startsAligned = notes.map(note => aligned(note.onset / 1000)).toArray
    val endsAligned =
      notes.map(note => aligned((note.onset + note.duration) / 1000)).toArray
    // every note gets at least half its written length, at the player's overall tempo
    val overall = (startsAligned.last - startsAligned.head) /
      math.max(1e-6, (notes.last.onset - notes.head.onset) / 1000)
    val least = notes.map(note => 0.5 * note.duration / 1000 * overall)
    val longEnough = endsAligned.indices
      .map(k => math.max(endsAligned(k), startsAligned(k) + least(k)))
      .toArray
    val (starts, reach) = repeatedNotes(notes, startsAligned, longEnough)
    val ends = longEnough.indices
      .map(k => math.max(longEnough(k), starts(k) + least(k)))
      .toArray
    val pitched = reviewPitch(notes, features, starts, ends, beat)
    val (tuning, intoned) = reviewIntonation(pitched)
    val onsets = refinedOnsets(notes, features, starts, reach)
    val usable = intoned.map(note =>
      note.verdict == "right" || note.verdict == "intonation"
    )
    val at = timing(notes, onsets, usable, beat)
    val rhythmic = reviewRhythm(notes, intoned, at, onsets, beat)
    // one line through the whole piece
    val whole = timing(notes, onsets, usable, 1e6)(
      notes(notes.size / 2).onset / 1000,
      None
    )
    val tempo = reviewTempo(
      notes,
      reference.measures.toVector,
      reference.markings.toVector,
      at,
      whole.fold(overall)(_._2),
      beat
    )
    val dynamicsReview = reviewDynamics(
      notes,
      reference.markings.toVector,
      features.level,
      starts,
      ends
    )
    Right(
      PerformanceReview(
        notes.indices.toList.map { k =>
          val note = rhythmic(k)
          ReviewedNote(
            notes(k).id,
            notes(k).measure,
            note.verdict,
            note.cents.map(cents => rounded(cents).toInt),
            note.heard,
            rounded(onsets(k) * 1000).toDouble,
            note.rhythm,
            note.onsetMillis
          )
        },
        tempo,
        dynamicsReview,
        // recording milliseconds against score milliseconds, every 50 ms of the score played
        arange(scoreSeconds.head, scoreSeconds.last, 0.05).toList.map(t =>
          AlignmentPoint(
            rounded(aligned(t) * 1000).toDouble,
            rounded(t * 1000).toDouble
          )
        ),
        rounded(features.seconds, 2),
        Some(rounded(tuning).toInt),
        Some(rounded(features.clipped, 2))
      )
    )
  }

  /** Where the player comes in, in recording seconds, when an accompaniment
    * played with the recording: it started with it, from `startMillis` of the
    * score, at `rate` times the written tempo.
    */
  private def entry(
      reference: PerformanceReference,
      notes: Vector[ReferenceNote]
  ): Option[Double] =
    reference.accompaniment.map { accompaniment =>
      val rate = if (accompaniment.rate == 0) 1.0 else accompaniment.rate
      math.max(
        0.0,
        (notes.head.onset - accompaniment.startMillis) / 1000 / rate
      )
    }

  // a span of seconds as frames, as the reference slices them
  private def frames(length: Int, start: Double, end: Double): (Int, Int) =
    span(length, (start / FrameSeconds).toInt, (end / FrameSeconds).toInt + 1)

  /** How present the most present of `pitches` is over frames a to b (its
    * median).
    */
  private def presence(
      values: Array[Array[Double]],
      lowest: Int,
      pitches: Seq[Int],
      a: Int,
      b: Int
  ): Double = {
    val columns = pitches
      .map(_ - lowest)
      .filter(column => column >= 0 && column < values.head.length)
    if (columns.isEmpty || b <= a) 0.0
    else {
      val (from, until) = span(values.length, a, b)
      columns
        .map(column => median((from until until).map(values(_)(column))))
        .max
    }
  }

  /** The deviation once the tracker's usual slips are taken out. */
  private def slipped(cents: Double): Double = {
    val semitones = cents / 100
    100 * (semitones - trackerSlips(
      argmin(trackerSlips.map(slip => math.abs(semitones - slip)))
    ))
  }

  /** The pitch held longest in a span, and the share of the span that has a
    * pitch at all. Frames are grouped by pitch, a quarter tone apart at most,
    * and the largest group's median is the pitch: a note before or after
    * spilling into the span is a smaller group, not a shift of the median.
    */
  private def heardPitch(
      heard: Array[Double],
      start: Double,
      end: Double
  ): Option[Double] = {
    val (a, b) = frames(heard.length, start, end)
    val voiced = (a until b).map(heard).filterNot(_.isNaN).sorted
    if (voiced.size < 2) None
    else {
      val groups =
        voiced.indices.drop(1).foldLeft(Vector(Vector(voiced.head))) {
          (found, index) =>
            if (voiced(index) - voiced(index - 1) > 0.25)
              found :+ Vector(voiced(index))
            else found.init :+ (found.last :+ voiced(index))
        }
      Some(median(groups.maxBy(_.size)))
    }
  }

  /** The pitches the tracker may hear in a note's place without it being wrong:
    * its own, slipped, and those of the notes just before and after it, come
    * early or held on.
    */
  private def trackerAllowed(notes: Vector[ReferenceNote], k: Int): Set[Int] = {
    val near = Seq(k - 1, k, k + 1)
      .filter(notes.indices.contains)
      .flatMap(notes(_).pitches)
    val own = for {
      pitch <- notes(k).pitches
      slip <- trackerSlips
    } yield pitch + slip
    own.toSet ++ near
  }

  /** Each note heard, wrong, a matter of intonation, missed, or uncertain.
    *
    * A note is heard when the pitch tracker hears it, or when its pitch is
    * present in the spectrum and not far outweighed by a pitch that has no
    * business there (its harmonics' and those of the notes played within the
    * beat before it and of the note after, which still ring or come in). It is
    * wrong only when both say another pitch: the spectrum alone mistakes a note
    * for the one a fifth or a fourth away. A note neither heard nor wrong is
    * missed if nothing sounds, and uncertain else.
    */
  private def reviewPitch(
      notes: Vector[ReferenceNote],
      features: ReviewFeatures,
      starts: Array[Double],
      ends: Array[Double],
      beat: Double
  ): Vector[Reviewed] = {
    val level = features.level
    val loudness = notes.indices.map { k =>
      val (a, b) = frames(level.length, starts(k), ends(k))
      ((a until b).map(level(_).toDouble) :+ -120.0).max
    }
    val loud = median(loudness)
    val (lowest, values) = (features.lowest, features.linear)
    val written = notes.map(_.onset / 1000)
    notes.indices.toVector.map { k =>
      val note = notes(k)
      val span = ends(k) - starts(k)
      // the alignment may be off by up to half a note: the best of three windows
      val windows = Seq(0.0, -0.4, 0.4).map(shift =>
        (
          ((starts(k) + (shift + 0.2) * span) / FrameSeconds).toInt,
          ((starts(k) + (shift + 0.9) * span) / FrameSeconds).toInt + 1
        )
      )
      val presences =
        windows.map((a, b) => presence(values, lowest, note.pitches, a, b))
      val (a, b) = windows(argmax(presences))
      val here = presence(values, lowest, note.pitches, a, b)
      if (loudness(k) < loud - silenceBelowDb && here < presentForVerdict)
        Reviewed("missed")
      else {
        val ringing = notes.indices
          .filter(j =>
            (written(k) - beat <= written(j) && written(j) < written(
              k
            )) || j == k + 1
          )
          .flatMap(notes(_).pitches)
          .toSet
        val allowed = for {
          pitch <- note.pitches.toSet ++ ringing
          slip <- trackerSlips
        } yield pitch + slip
        val median =
          if (b > a) {
            val (from, until) = ReviewNumerics.span(values.length, a, b)
            values.head.indices.map(column =>
              ReviewNumerics.median((from until until).map(values(_)(column)))
            )
          } else values.head.indices.map(_ => 0.0)
        // the most salient pitch that has no business there
        val rival = median.indices
          .filterNot(i => allowed.contains(i + lowest))
          .map(median)
          .maxOption
          .getOrElse(0.0)
        val pitch =
          heardPitch(features.heard, a * FrameSeconds, b * FrameSeconds)
        val deviation = pitch.map(heard =>
          note.pitches.map(p => slipped(100 * (heard - p))).minBy(math.abs)
        )
        val tracked = deviation.exists(math.abs(_) < wrongNoteCents)
        if (
          tracked || (here >= presentForVerdict && here >= outweighed * rival)
        ) {
          Reviewed("right", cents = deviation.filter(_ => tracked))
        } else if (
          rival >= wrongPresent && pitch.exists(heard =>
            !trackerAllowed(notes, k).contains(rounded(heard).toInt)
          )
        ) {
          // the tracker hears another note than the one written, unrelated to it and to its neighbours
          Reviewed("wrong", heard = pitch.map(rounded(_, 2)))
        } else {
          val neighbours = Seq(k - 1, k + 1)
            .filter(notes.indices.contains)
            .flatMap(notes(_).pitches)
            .toSet
          val strongest = Option.when(b > a)(argmax(median) + lowest)
          Reviewed(
            if (strongest.exists(neighbours.contains)) "missed" else "uncertain"
          )
        }
      }
    }
  }

  /** Each note's intonation against the player's own tuning: the median
    * deviation of all the notes heard, which says how high or low the
    * instrument stands, and is not held against each note. With that tuning, in
    * cents from A = 440 Hz.
    */
  private def reviewIntonation(
      reviewed: Vector[Reviewed]
  ): (Double, Vector[Reviewed]) = {
    val deviations = reviewed.flatMap(_.cents)
    val tuning =
      if (deviations.size >= tuningLeastNotes) median(deviations) else 0.0
    (
      tuning,
      reviewed.map(note =>
        note.cents.fold(note) { cents =>
          val against = cents - tuning
          note.copy(
            cents = Some(rounded(against).toDouble),
            verdict =
              if (math.abs(against) >= intonationCents) "intonation"
              else note.verdict
          )
        }
      )
    )
  }

  /** Starts of notes repeating the pitch before them, spread in proportion to
    * the score: the pitch heard does not say where a repeated note begins, and
    * the alignment smears a run of them. With how far each may still be moved
    * to an attack (seconds).
    */
  private def repeatedNotes(
      notes: Vector[ReferenceNote],
      aligned: Array[Double],
      ends: Array[Double]
  ): (Array[Double], Array[Double]) = {
    val starts = aligned.clone()
    val reach = Array.fill(notes.size)(onsetReach)
    var k = 0
    while (k < notes.size) {
      var j = k
      while (j + 1 < notes.size && notes(j + 1).pitches == notes(k).pitches)
        j += 1
      if (j > k) {
        val first = notes(k).onset
        // the run ends where the next pitch starts, or where the last note ends
        val (after, heardAfter) =
          if (j + 1 < notes.size) (notes(j + 1).onset, starts(j + 1))
          else (notes(j).onset + notes(j).duration, ends(j))
        for (i <- k + 1 to j) {
          val share = (notes(i).onset - first) / (after - first)
          starts(i) = starts(k) + share * (heardAfter - starts(k))
          reach(i) = math.max(
            onsetReach,
            repeatedReachShare * (heardAfter - starts(k)) * notes(
              i
            ).duration / (after - first)
          )
        }
      }
      k = j + 1
    }
    (starts, reach)
  }

  /** Each aligned onset moved to where the note is heard to begin, within its
    * reach. A note of another pitch than the one before begins where its pitch
    * becomes present: notes played legato have no attack to find, but they do
    * change pitch. A note repeating the pitch before it begins at the strongest
    * attack.
    */
  private def refinedOnsets(
      notes: Vector[ReferenceNote],
      features: ReviewFeatures,
      starts: Array[Double],
      reach: Array[Double]
  ): Array[Double] = {
    val values = features.values
    notes.indices.map { k =>
      val centre = (starts(k) / FrameSeconds).toInt
      val spread = (reach(k) / FrameSeconds).toInt
      val (a, b) = (
        math.max(0, centre - spread),
        math.min(values.length, centre + spread + 1)
      )
      val byPitch =
        if (k > 0 && notes(k).pitches != notes(k - 1).pitches && b > a) {
          val columns = notes(k).pitches
            .map(_ - features.lowest)
            .filter(c => c >= 0 && c < values.head.length)
          val own = (a until b).map(row =>
            columns.nonEmpty && columns.map(values(row)(_)).max >= present
          )
          // the first frame from which the note's pitch holds for a few frames
          val holding = (0 to own.size - pitchHoldFrames).filter(i =>
            (i until i + pitchHoldFrames).forall(own)
          )
          Option.when(holding.nonEmpty) {
            (a + holding(
              argmin(holding.map(h => math.abs(h + a - centre).toDouble))
            )) * FrameSeconds
          }
        } else None
      byPitch
        .orElse(
          Option.when(b > a)(
            (a + argmax(
              (a until b).map(features.onsetStrength(_).toDouble)
            )) * FrameSeconds
          )
        )
        .getOrElse(starts(k))
    }.toArray
  }

  /** The player's own timing around a point of the score, from the onsets of
    * the notes near it: a straight line through them (score seconds to
    * recording seconds), refitted once without its outliers. `at(t, without)`
    * gives the recording seconds expected at `t` and the recording seconds per
    * score second, or none where too few notes are near.
    */
  private def timing(
      notes: Vector[ReferenceNote],
      onsets: Array[Double],
      usable: Vector[Boolean],
      beat: Double
  ): (Double, Option[Int]) => Option[(Double, Double)] = {
    val written = notes.map(_.onset / 1000)
    val half = tempoWindowBeats * beat / 2
    val candidates = usable.indices.filter(usable)
    def fit(chosen: Seq[Int]): (Double, Double) = {
      val (xs, ys) = (chosen.map(written), chosen.map(onsets))
      val (slope, intercept) = line(xs, ys)
      val residual =
        chosen.indices.map(i => math.abs(slope * xs(i) + intercept - ys(i)))
      val limit = math.max(0.03, 3 * median(residual))
      // a point on the limit is kept, whatever the last bits of the fit say
      val kept = chosen.indices.filter(residual(_) <= limit + 1e-9)
      if (kept.size >= 3) line(kept.map(xs), kept.map(ys))
      else (slope, intercept)
    }
    (t, without) => {
      val within = usable.indices.filter(i =>
        usable(i) && math.abs(written(i) - t) <= half
      )
      val near =
        if (within.size < tempoLeastNotes + 1) {
          // slow music: the nearest notes, however far
          candidates
            .sortBy(i => math.abs(written(i) - t))
            .take(tempoLeastNotes + 1)
            .sorted
        } else within
      val chosen = near.filterNot(without.contains)
      if (
        chosen.size < 4 || chosen.map(written).max - chosen
          .map(written)
          .min < beat
      ) None
      else {
        val (slope, intercept) = fit(chosen)
        Some((slope * t + intercept, slope))
      }
    }
  }

  private def reviewRhythm(
      notes: Vector[ReferenceNote],
      reviewed: Vector[Reviewed],
      at: (Double, Option[Int]) => Option[(Double, Double)],
      onsets: Array[Double],
      beat: Double
  ): Vector[Reviewed] =
    reviewed.indices.toVector.map { k =>
      val note = reviewed(k)
      if (note.verdict == "missed") note
      else
        // where the onset falls if the player keeps their own timing of the moment, the note left out
        at(notes(k).onset / 1000, Some(k)).fold(note) { (expected, ratio) =>
          val early = onsets(k) - expected
          val threshold = math.max(
            rhythmLeastMillis / 1000.0,
            rhythmShareOfBeat * beat * ratio
          )
          val off = Option.when(
            math.abs(early) > threshold && note.verdict != "wrong"
          )(if (early > 0) "late" else "early")
          note.copy(
            onsetMillis = Some(rounded(early * 1000).toInt),
            rhythm = off.orElse(note.rhythm)
          )
        }
    }

  /** Score seconds spans in which the score itself asks for the tempo to
    * change.
    */
  private def tempoMarked(
      markings: Vector[Marking]
  ): Vector[(Double, Double)] = {
    val words = markings.filter(_.kind == "words").sortBy(_.onset)
    val (spans, open) =
      words.foldLeft((Vector.empty[(Double, Double)], Option.empty[Double])) {
        case ((found, Some(start)), marking)
            if tempoBack.findFirstIn(marking.text).isDefined =>
          (found :+ (start -> marking.onset / 1000), None)
        case ((found, None), marking)
            if tempoChange.findFirstIn(marking.text).isDefined =>
          (found, Some(marking.onset / 1000))
        case (state, _) => state
      }
    spans ++ open.map(_ -> Double.PositiveInfinity)
  }

  private def measureAt(
      measures: Vector[ReferenceMeasure],
      seconds: Double
  ): String =
    measures
      .filter(_.start / 1000 <= seconds + 1e-6)
      .lastOption
      .getOrElse(measures.head)
      .number

  private def reviewTempo(
      notes: Vector[ReferenceNote],
      measures: Vector[ReferenceMeasure],
      markings: Vector[Marking],
      at: (Double, Option[Int]) => Option[(Double, Double)],
      overall: Double,
      beat: Double
  ): TempoReview = {
    val written = notes.map(_.onset / 1000)
    val grid = arange(written.head, written.last, beat)
    val curve = grid.map(t => at(t, None).map(_._2))
    val marked = tempoMarked(markings)
    // a drift: (kind, from, to, largest change)
    val drifts = grid
      .zip(curve)
      .foldLeft(
        (Vector.empty[(String, Double, Double, Double)], Option.empty[Int])
      ) { case ((found, current), (t, value)) =>
        val inside = marked.exists((a, b) => a <= t && t < b)
        // > 0: faster than overall
        val change = value.filter(_ != 0).fold(0.0)(v => overall / v - 1)
        val drifting = math.abs(change) > tempoDrift && !inside
        val kind = if (change > 0) "faster" else "slower"
        current.map(found) match {
          case Some((runKind, from, _, largest))
              if drifting && runKind == kind =>
            (
              found.updated(
                current.get,
                (kind, from, t + beat, math.max(largest, math.abs(change)))
              ),
              current
            )
          case _ if drifting =>
            (found :+ (kind, t, t + beat, math.abs(change)), Some(found.size))
          case _ => (found, None)
        }
      }
      ._1
    val lasting = drifts.filter((_, from, to, _) =>
      to - from >= tempoDriftLeastBeats * beat
    )
    TempoReview(
      rounded(overall, 3),
      grid
        .zip(curve)
        .collect { case (t, Some(v)) =>
          TempoPoint(rounded(t * 1000).toDouble, rounded(overall / v, 3))
        }
        .toList,
      lasting.toList.map((kind, from, to, largest) =>
        TempoDrift(
          kind,
          measureAt(measures, from),
          measureAt(measures, to - 1e-3),
          rounded(100 * largest).toInt
        )
      )
    )
  }

  /** The level of the notes under each dynamic marking, and whether each change
    * asked for is heard.
    */
  private def reviewDynamics(
      notes: Vector[ReferenceNote],
      markings: Vector[Marking],
      level: Array[Float],
      starts: Array[Double],
      ends: Array[Double]
  ): DynamicsReview = {
    val noteLevel = notes.indices.map { k =>
      val (a, b) = frames(level.length, starts(k), ends(k))
      if (b > a) percentile((a until b).map(level(_).toDouble), 90)
      else Double.NaN
    }
    val onsets = notes.map(_.onset)
    val marks = markings
      .filter(m => m.kind == "dynamics" && dynamics.contains(m.text))
      .sortBy(_.onset)
    val regions = marks.indices.flatMap { i =>
      val end = marks.lift(i + 1).fold(Double.PositiveInfinity)(_.onset)
      val inside = notes.indices.filter(k =>
        onsets(k) >= marks(i).onset && onsets(k) < end && !noteLevel(k).isNaN
      )
      Option.when(inside.nonEmpty)(
        (marks(i), rounded(median(inside.map(noteLevel)), 1))
      )
    }
    val changes =
      regions.zip(regions.drop(1)).flatMap { case ((a, levelA), (b, levelB)) =>
        val asked =
          math.signum(dynamics.indexOf(b.text) - dynamics.indexOf(a.text))
        val heard = levelB - levelA
        Option.when(asked != 0 && asked * heard < dynamicContrastDb)(
          WeakDynamics(
            Some(a.text),
            Some(b.text),
            None,
            b.measure,
            rounded(heard, 1)
          )
        )
      }
    val wedges = markings.filter(_.kind == "wedge").flatMap { marking =>
      val end = marking.end.getOrElse(Double.NaN)
      val inside = notes.indices.filter(k =>
        onsets(k) >= marking.onset && onsets(k) <= end && !noteLevel(k).isNaN
      )
      Option
        .when(inside.size >= 3) {
          val slope = line(
            inside.map(onsets),
            inside.map(noteLevel)
          )._1 * (end - marking.onset)
          val asked = if (marking.text == "crescendo") 1 else -1
          Option.when(asked * slope < dynamicContrastDb)(
            WeakDynamics(
              None,
              None,
              Some(marking.text),
              marking.measure,
              rounded(slope, 1)
            )
          )
        }
        .flatten
    }
    DynamicsReview(
      changes.toList,
      wedges.toList,
      marks.nonEmpty || wedges.nonEmpty
    )
  }
}
