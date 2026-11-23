package ripieno.backend.review

/** The magnitudes of a constant-Q spectrum, as librosa's `cqt` computes them
  * (its defaults: Hann wavelets, an L1 norm, the faintest 1 % of each filter's
  * spectrum left out, each bin scaled by its filter's length): an octave at a
  * time from the top, the signal halved by libsoxr (`Soxr`) whenever the next
  * octave allows, as many as the hop allows halving early.
  */
object ConstantQ {

  // librosa's Hann window bandwidth (filters.WINDOW_BANDWIDTHS)
  private val hannBandwidth = 1.50018310546875
  private val sparsity = 0.01

  /** `[bins][frames]`, bins `binsPerOctave` an octave from `fmin`. */
  def magnitudes(
      signal: Array[Float],
      rate: Double,
      hop: Int,
      fmin: Double,
      bins: Int,
      binsPerOctave: Int
  ): Array[Array[Float]] = {
    val frequencies = Array.tabulate(bins)(k =>
      math.pow(2.0, (k / binsPerOctave).toDouble) * math.pow(
        2.0,
        (k % binsPerOctave).toDouble / binsPerOctave
      ) * fmin
    )
    val alpha = relativeBandwidth(frequencies)
    val octaves = math.ceil(bins.toDouble / binsPerOctave).toInt
    val filters = math.min(binsPerOctave, bins)
    val cutoff = frequencies.indices
      .map(k => frequencies(k) * (1 + 0.5 * hannBandwidth * alpha(k)))
      .max
    val nyquist = rate / 2
    // the halvings done before the first octave
    val early = math.min(
      math.max(0, math.ceil(log2(nyquist / cutoff)).toInt - 1 - 1),
      math.max(0, Integer.numberOfTrailingZeros(hop) - octaves + 1)
    )
    val factor = 1 << early
    val (startSignal, startRate, startHop) =
      if (early > 0)
        (Soxr.downsampled(signal, factor), rate / factor, hop / factor)
      else (signal, rate, hop)
    var (y, octaveRate, octaveHop) = (startSignal, startRate, startHop)
    val responses = (0 until octaves).map { octave =>
      val (from, until) =
        (math.max(0, bins - filters * (octave + 1)), bins - filters * octave)
      val response = octaveResponse(
        y,
        octaveRate,
        octaveHop,
        frequencies.slice(from, until),
        alpha.slice(from, until),
        startRate
      )
      if (octave < octaves - 1) {
        val nextTop =
          frequencies((bins - filters * (octave + 1) - 1 + bins) % bins)
        if (octaveHop % 2 == 0 && nextTop <= octaveRate / 5) {
          octaveHop /= 2
          octaveRate /= 2
          y = Soxr.downsampled(y, 2)
        }
      }
      response
    }
    // the octaves stacked, the frames all of them have; each bin scaled by its
    // filter's length
    val frames = responses.map(_.head.length).min
    val lengths =
      frequencies.indices.map(k => startRate / (alpha(k) * frequencies(k)))
    val stacked = Array.ofDim[Float](bins, frames)
    var end = bins
    for (response <- responses) {
      val rows = response.length
      val kept = if (end < rows) response.takeRight(end) else response
      for ((row, index) <- kept.zipWithIndex) {
        val bin = end - kept.length + index
        val scale = math.sqrt(lengths(bin))
        for (t <- 0 until frames) stacked(bin)(t) = (row(t) / scale).toFloat
      }
      end -= rows
    }
    stacked
  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  // each bin's bandwidth against its frequency, from its neighbours'
  private def relativeBandwidth(frequencies: Array[Double]): Array[Double] = {
    val log = frequencies.map(log2)
    val n = log.length
    val perOctave = Array.tabulate(n) { k =>
      if (k == 0) 1 / (log(1) - log(0))
      else if (k == n - 1) 1 / (log(n - 1) - log(n - 2))
      else 2 / (log(k + 1) - log(k - 1))
    }
    perOctave.map(bpo =>
      (math.pow(2.0, 2 / bpo) - 1) / (math.pow(2.0, 2 / bpo) + 1)
    )
  }

  /** The magnitudes of an octave's bins (`[bins][frames]`): each frame's
    * spectrum (a rectangular window centred on it) through each bin's filter.
    */
  private def octaveResponse(
      signal: Array[Float],
      rate: Double,
      hop: Int,
      frequencies: Array[Double],
      alpha: Array[Double],
      startRate: Double
  ): Array[Array[Double]] = {
    val lengths =
      frequencies.indices.map(k => (1.0 / alpha(k)) * rate / frequencies(k))
    // the filters padded to a power of two
    val n = 1 << math.ceil(log2(lengths.max)).toInt
    // each bin's filter spectrum, its faintest bins left out: (bin index, re, im)
    val basis = frequencies.indices.map { b =>
      val length = lengths(b)
      val start = math.floor(-length / 2)
      val count = math.ceil(math.floor(length / 2) - start).toInt
      val window = Spectra.hann(count)
      val re = Array.tabulate(count)(i =>
        math.cos((start + i) * 2 * math.Pi * frequencies(b) / rate) * window(i)
      )
      val im = Array.tabulate(count)(i =>
        math.sin((start + i) * 2 * math.Pi * frequencies(b) / rate) * window(i)
      )
      val norm = re.indices.map(i => math.hypot(re(i), im(i))).sum
      val scale = length / n
      val padded = Complex(new Array[Double](n), new Array[Double](n))
      val left = (n - count) / 2
      for (i <- 0 until count) {
        padded.re(left + i) = (re(i) / norm).toFloat * scale
        padded.im(left + i) = (im(i) / norm).toFloat * scale
      }
      Fft.transform(padded)
      val kept = n / 2 + 1
      val magnitudes =
        Array.tabulate(kept)(k => math.hypot(padded.re(k), padded.im(k)))
      val total = magnitudes.sum
      val sorted = magnitudes.sorted
      var cumulative = 0.0
      val threshold = sorted
        .find { magnitude =>
          cumulative += magnitude / total
          cumulative >= sparsity
        }
        .getOrElse(sorted.last)
      val gain = math.sqrt(startRate / rate)
      (0 until kept)
        .filter(k => magnitudes(k) >= threshold)
        .map(k => (k, padded.re(k) * gain, padded.im(k) * gain))
    }
    val spectra = Spectra.stft(signal, n, hop, Array.fill(n)(1.0))
    basis.map { filter =>
      spectra.map { spectrum =>
        var (re, im) = (0.0, 0.0)
        for ((k, fr, fi) <- filter) {
          re += fr * spectrum.re(k) - fi * spectrum.im(k)
          im += fr * spectrum.im(k) + fi * spectrum.re(k)
        }
        math.hypot(re, im)
      }.toArray
    }.toArray
  }
}
