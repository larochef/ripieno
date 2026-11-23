package ripieno.backend.review

import java.util.stream.IntStream

/** The pitch heard frame by frame, by probabilistic YIN (Mauch and Dixon, 2014)
  * as librosa's `pyin` finds it: each frame's YIN difference function, its
  * troughs made pitch candidates with probabilities under a spread of
  * thresholds, then the likeliest path through pitches a tenth of a semitone
  * apart, voiced or not (Viterbi).
  */
object PitchTracking {

  private val thresholds = 100
  // the thresholds' distribution, Beta(2, 18), and the troughs' prior,
  // Boltzmann(2): the lowest troughs likelier
  private val betaB = 18
  private val boltzmann = 2.0
  private val maxTransitionRate = 35.92
  private val switchProbability = 0.01
  private val noTroughProbability = 0.01
  private val transitionMinProbability = 1e-4
  // the smallest normal float and double (librosa's `tiny`)
  private val tinyDouble = 2.2250738585072014e-308

  /** The pitch (Hz, NaN where none) and whether each frame is voiced. */
  def pyin(
      signal: Array[Float],
      fmin: Double,
      fmax: Double,
      rate: Int,
      frameLength: Int,
      hop: Int,
      resolution: Double
  ): (Array[Double], Array[Boolean]) = {
    val frames = 1 + signal.length / hop
    val minPeriod = math.floor(rate / fmax).toInt
    val maxPeriod = math.min(math.ceil(rate / fmin).toInt, frameLength - 1)
    val binsPerSemitone = math.ceil(1.0 / resolution).toInt
    val pitchBins =
      math.floor(12 * binsPerSemitone * log2(fmax / fmin)).toInt + 1
    val betaProbabilities = {
      val cdf =
        Array.tabulate(thresholds + 1)(i => betaCdf(i.toDouble / thresholds))
      Array.tabulate(thresholds)(i => cdf(i + 1) - cdf(i))
    }
    val thresholdValues =
      Array.tabulate(thresholds + 1)(i => i.toDouble / thresholds)
    // each frame's probabilities: of each pitch bin voiced, then unvoiced;
    // the frames on all the cores, each its own
    val observations = new Array[Array[Double]](frames)
    IntStream.range(0, frames).parallel().forEach { t =>
      val frame = Array.tabulate(frameLength) { k =>
        val at = t * hop - frameLength / 2 + k
        if (at < 0 || at >= signal.length) 0.0f else signal(at)
      }
      val yin = normalizedDifference(frame, minPeriod, maxPeriod)
      val shifts = parabolicShifts(yin)
      val probabilities =
        troughProbabilities(yin, thresholdValues, betaProbabilities)
      val observed = new Array[Double](2 * pitchBins)
      for (period <- yin.indices if probabilities(period) != 0) {
        val candidate = minPeriod + period + shifts(period)
        val bin = 12 * binsPerSemitone * log2(rate / candidate / fmin)
        val index = math.max(0, math.min(pitchBins, math.rint(bin).toInt))
        observed(index) = probabilities(period)
      }
      var sum = 0.0
      for (bin <- 0 until pitchBins) sum += observed(bin)
      val voiced = math.max(0.0, math.min(1.0, sum))
      for (bin <- pitchBins until 2 * pitchBins)
        observed(bin) = (1 - voiced) / pitchBins
      observations(t) = observed
    }
    val states = viterbi(observations, pitchBins, binsPerSemitone, rate, hop)
    val frequencies = Array.tabulate(pitchBins)(bin =>
      fmin * math.pow(2, bin.toDouble / (12 * binsPerSemitone))
    )
    (
      states.map(state =>
        if (state < pitchBins) frequencies(state) else Double.NaN
      ),
      states.map(_ < pitchBins)
    )
  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  // Beta(2, b)'s distribution function: 1 − (1 − x)^b (1 + b x)
  private def betaCdf(x: Double): Double =
    1 - math.pow(1 - x, betaB) * (1 + betaB * x)

  /** YIN's cumulative mean normalised difference, periods `minPeriod` to
    * `maxPeriod`, as librosa has it: floats, until the mean (over integers)
    * makes it doubles.
    */
  private def normalizedDifference(
      frame: Array[Float],
      minPeriod: Int,
      maxPeriod: Int
  ): Array[Double] = {
    val padded = Integer.highestOneBit(2 * frame.length - 1) * 2
    val spectrum = Fft.real(
      Array.tabulate(padded)(k =>
        if (k < frame.length) frame(k).toDouble else 0.0
      )
    )
    val power = Complex(
      spectrum.re.indices
        .map(k =>
          (spectrum.re(k) * spectrum.re(k) + spectrum.im(k) * spectrum.im(
            k
          )).toFloat.toDouble
        )
        .toArray,
      new Array[Double](spectrum.length)
    )
    val correlation = Fft.inverseReal(power, padded).map(_.toFloat)
    // the energy so far, its first value left out as librosa leaves it
    val energy = new Array[Float](maxPeriod + 1)
    var sum = 0.0f
    for (k <- 0 to maxPeriod) {
      sum += frame(k) * frame(k)
      energy(k) = sum
    }
    energy(0) = 0
    val difference = new Array[Float](maxPeriod + 1)
    for (period <- 1 to maxPeriod) {
      difference(period) =
        2 * (correlation(0) - correlation(period)) - energy(period - 1)
    }
    val mean = new Array[Double](maxPeriod + 1)
    var running = 0.0f
    for (period <- 1 to maxPeriod) {
      running += difference(period)
      mean(period) = running.toDouble / period
    }
    Array.tabulate(maxPeriod - minPeriod + 1)(i =>
      difference(minPeriod + i).toDouble / (mean(minPeriod + i) + tinyDouble)
    )
  }

  // where between frames each trough really is: a parabola through it and its
  // neighbours, no further than a frame
  private def parabolicShifts(x: Array[Double]): Array[Double] =
    Array.tabulate(x.length) { i =>
      if (i == 0 || i == x.length - 1) 0.0
      else {
        val a = x(i + 1) + x(i - 1) - 2 * x(i)
        val b = (x(i + 1) - x(i - 1)) / 2
        if (math.abs(b) >= math.abs(a)) 0.0 else -b / a
      }
    }

  /** The probability of each period being the pitch's: under each threshold,
    * the troughs below it, the lowest likelier; the lowest trough also takes
    * what no trough was under.
    */
  private def troughProbabilities(
      yin: Array[Double],
      thresholds: Array[Double],
      beta: Array[Double]
  ): Array[Double] = {
    val n = yin.length
    val trough = Array.tabulate(n) { i =>
      if (i == 0) yin(0) < yin(1)
      else if (i == n - 1) yin(n - 1) < yin(n - 2)
      else yin(i) < yin(i - 1) && yin(i) <= yin(i + 1)
    }
    val troughs = yin.indices.filter(trough)
    val probabilities = new Array[Double](n)
    if (troughs.nonEmpty) {
      val heights = troughs.map(yin)
      val below = Array.tabulate(heights.size, beta.length)((i, j) =>
        heights(i) < thresholds(j + 1)
      )
      val counts = beta.indices.map(j => heights.indices.count(below(_)(j)))
      val prior = Array.ofDim[Double](heights.size, beta.length)
      for (j <- beta.indices) {
        var position = -1
        for (i <- heights.indices if below(i)(j)) {
          position += 1
          prior(i)(j) =
            (1 - math.exp(-boltzmann)) * math.exp(-boltzmann * position) /
              (1 - math.exp(-boltzmann * counts(j)))
        }
      }
      val probs = Array.tabulate(heights.size) { i =>
        var sum = 0.0
        for (j <- beta.indices) sum += prior(i)(j) * beta(j)
        sum
      }
      val lowest = ReviewNumerics.argmin(heights)
      val notBelow = beta.indices.count(j => !below(lowest)(j))
      var under = 0.0
      for (j <- 0 until notBelow) under += beta(j)
      probs(lowest) += noTroughProbability * under
      for ((index, i) <- troughs.zipWithIndex) probabilities(index) = probs(i)
    }
    probabilities
  }

  /** The likeliest states, voiced pitch bins first then unvoiced ones, from a
    * pitch to one at most a few semitones a frame away, voicing changing
    * rarely.
    */
  private def viterbi(
      observations: Array[Array[Double]],
      pitchBins: Int,
      binsPerSemitone: Int,
      rate: Int,
      hop: Int
  ): Array[Int] = {
    val semitonesPerFrame = math.rint(maxTransitionRate * 12 * hop / rate).toInt
    val width = semitonesPerFrame * binsPerSemitone + 1
    // a triangle centred on each bin, cut at the ends, each row summing to one
    val local = Array.tabulate(pitchBins) { i =>
      val row = Array.tabulate(pitchBins) { j =>
        val distance = math.abs(j - i)
        if (distance > width / 2) 0.0 else 1 - 2.0 * distance / (width + 1)
      }
      val total = row.sum
      row.map(_ / total)
    }
    val states = 2 * pitchBins
    def transition(from: Int, to: Int): Double = {
      val switch =
        if ((from < pitchBins) == (to < pitchBins)) 1 - switchProbability
        else switchProbability
      switch * local(from % pitchBins)(to % pitchBins)
    }
    val threshold = math.log(transitionMinProbability + tinyDouble)
    // each state's predecessors likely enough, and the log of coming from each
    val (predecessors, logTransitions) = (0 until states).map { to =>
      val kept = (0 until states)
        .map(from => from -> math.log(transition(from, to) + tinyDouble))
        .filter(_._2 >= threshold)
      (kept.map(_._1).toArray, kept.map(_._2).toArray)
    }.unzip
    val frames = observations.length
    val pointer = Array.ofDim[Int](frames, states)
    var value = Array.tabulate(states)(j =>
      math.log(observations(0)(j) + tinyDouble) + math.log(
        1.0 / states + tinyDouble
      )
    )
    for (t <- 1 until frames) {
      val next = new Array[Double](states)
      for (j <- 0 until states) {
        var best = Double.NegativeInfinity
        val (from, logTransition) = (predecessors(j), logTransitions(j))
        var k = 0
        while (k < from.length) {
          val cost = value(from(k)) + logTransition(k)
          if (cost > best) {
            pointer(t)(j) = from(k)
            best = cost
          }
          k += 1
        }
        next(j) = math.log(observations(t)(j) + tinyDouble) + best
      }
      value = next
    }
    val path = new Array[Int](frames)
    path(frames - 1) = ReviewNumerics.argmax(value.toIndexedSeq)
    for (t <- frames - 2 to 0 by -1) path(t) = pointer(t + 1)(path(t + 1))
    path
  }
}
