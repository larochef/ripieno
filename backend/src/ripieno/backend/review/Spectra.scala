package ripieno.backend.review

import java.util.concurrent.ConcurrentHashMap
import java.util.stream.IntStream

/** A complex spectrum or signal, its real and imaginary parts apart. */
final case class Complex(re: Array[Double], im: Array[Double]) {
  def length: Int = re.length
}

/** The fast Fourier transform, for lengths that are powers of two. */
object Fft {

  // each size's twiddles, forward, from their own angles: no drift along the way
  private val twiddles =
    new ConcurrentHashMap[Int, (Array[Double], Array[Double])]()

  private def twiddlesOf(size: Int): (Array[Double], Array[Double]) =
    twiddles.computeIfAbsent(
      size,
      _ => {
        val angle = -2 * math.Pi / size
        (
          Array.tabulate(size / 2)(k =>
            if (k == 0) 1.0 else math.cos(angle * k)
          ),
          Array.tabulate(size / 2)(k =>
            if (k == 0) 0.0 else math.sin(angle * k)
          )
        )
      }
    )

  /** The transform of `x`, in place; the inverse one, unscaled, when asked. */
  def transform(x: Complex, inverse: Boolean = false): Unit = {
    val n = x.length
    require(Integer.bitCount(n) == 1, s"an FFT of $n points")
    val (re, im) = (x.re, x.im)
    var j = 0
    for (i <- 1 until n) {
      var bit = n >> 1
      while ((j & bit) != 0) {
        j ^= bit
        bit >>= 1
      }
      j |= bit
      if (i < j) {
        val (r, m) = (re(i), im(i))
        re(i) = re(j); im(i) = im(j); re(j) = r; im(j) = m
      }
    }
    var size = 2
    while (size <= n) {
      val half = size / 2
      val (twiddleRe, forwardIm) = twiddlesOf(size)
      val twiddleIm = if (inverse) forwardIm.map(-_) else forwardIm
      var start = 0
      while (start < n) {
        var k = 0
        while (k < half) {
          val (a, b) = (start + k, start + k + half)
          val tr = re(b) * twiddleRe(k) - im(b) * twiddleIm(k)
          val ti = re(b) * twiddleIm(k) + im(b) * twiddleRe(k)
          re(b) = re(a) - tr; im(b) = im(a) - ti
          re(a) += tr; im(a) += ti
          k += 1
        }
        start += size
      }
      size *= 2
    }
  }

  /** The spectrum of a real signal, its `n / 2 + 1` first bins. */
  def real(signal: Array[Double]): Complex = {
    val x = Complex(signal.clone(), new Array[Double](signal.length))
    transform(x)
    Complex(x.re.take(signal.length / 2 + 1), x.im.take(signal.length / 2 + 1))
  }

  /** The real signal of `n` points whose spectrum's first bins are `half`. */
  def inverseReal(half: Complex, n: Int): Array[Double] = {
    val re = new Array[Double](n)
    val im = new Array[Double](n)
    for (k <- 0 until math.min(half.length, n / 2 + 1)) {
      re(k) = half.re(k); im(k) = half.im(k)
      if (k > 0 && k < n - k) {
        re(n - k) = half.re(k); im(n - k) = -half.im(k)
      }
    }
    // the Nyquist and zero bins are real in a real signal's spectrum
    im(0) = 0
    if (n % 2 == 0) im(n / 2) = 0
    val x = Complex(re, im)
    transform(x, inverse = true)
    x.re.map(_ / n)
  }
}

/** librosa's short-time spectra and what is made of them, as the review was set
  * with: frames centred on multiples of the hop, zeros around the signal.
  */
object Spectra {

  /** The periodic Hann window (`get_window("hann", n)`). */
  def hann(n: Int): Array[Double] =
    Array.tabulate(n)(k => 0.5 - 0.5 * math.cos(2 * math.Pi * k / n))

  /** The frames' spectra, `[frames][n / 2 + 1]`: frame t centred on sample t ×
    * hop, the signal padded with zeros by n / 2 on both sides.
    */
  def stft(
      signal: Array[Float],
      n: Int,
      hop: Int,
      window: Array[Double]
  ): Vector[Complex] = {
    val frames = 1 + signal.length / hop
    // the frames on all the cores, each its own
    val spectra = new Array[Complex](frames)
    IntStream.range(0, frames).parallel().forEach { t =>
      val start = t * hop - n / 2
      spectra(t) = Fft.real(Array.tabulate(n) { k =>
        val at = start + k
        if (at < 0 || at >= signal.length) 0.0 else signal(at) * window(k)
      })
    }
    spectra.toVector
  }

  /** The signal of `length` samples whose spectra are `frames` (`stft`'s, with
    * the periodic Hann window): each frame windowed again and added up, divided
    * by the windows' squares where they are not nothing.
    */
  def istft(
      frames: Vector[Complex],
      n: Int,
      hop: Int,
      length: Int
  ): Array[Float] = {
    val window = hann(n)
    val padded = length + 2 * (n / 2)
    val count = math.min(frames.size, math.ceil(padded.toDouble / hop).toInt)
    val total = n + hop * (count - 1)
    val sum = new Array[Double](total)
    val squares = new Array[Double](total)
    val signals = new Array[Array[Double]](count)
    IntStream
      .range(0, count)
      .parallel()
      .forEach(t => signals(t) = Fft.inverseReal(frames(t), n))
    for (t <- 0 until count) {
      val frame = signals(t)
      for (k <- 0 until n) {
        sum(t * hop + k) += frame(k) * window(k)
        squares(t * hop + k) += window(k) * window(k)
      }
    }
    // the smallest normal float (librosa's `tiny`)
    val tiny = 1.17549435e-38
    Array.tabulate(length) { index =>
      val at = index + n / 2
      if (at >= total) 0.0f
      else (if (squares(at) > tiny) sum(at) / squares(at) else sum(at)).toFloat
    }
  }

  /** The root mean square of each frame of `n` samples, centred as `stft`'s. */
  def rms(signal: Array[Float], n: Int, hop: Int): Array[Float] =
    Array.tabulate(1 + signal.length / hop) { t =>
      val start = t * hop - n / 2
      var sum = 0.0
      for (k <- 0 until n) {
        val at = start + k
        if (at >= 0 && at < signal.length)
          sum += signal(at).toDouble * signal(at)
      }
      math.sqrt(sum / n).toFloat
    }

  /** Powers in decibels (`power_to_db`, a reference of 1): no lower than 10⁻¹⁰,
    * nor than 80 dB under the loudest.
    */
  def powerToDb(power: Array[Float]): Array[Float] = {
    val db = power.map(p => 10 * math.log10(math.max(1e-10, p.toDouble)))
    val floor = db.max - 80
    db.map(value => math.max(value, floor).toFloat)
  }

  /** Amplitudes in decibels (`amplitude_to_db`, a reference of 1). */
  def amplitudeToDb(amplitude: Array[Float]): Array[Float] =
    powerToDb(amplitude.map(a => a * a))

  /** librosa's mel filters (Slaney's scale and norm), `[mels][n / 2 + 1]`. */
  def melFilters(rate: Double, n: Int, mels: Int): Array[Array[Float]] = {
    val (minLogHz, linear) = (1000.0, 200.0 / 3)
    val minLogMel = minLogHz / linear
    val logStep = math.log(6.4) / 27.0
    def toMel(hz: Double) = if (hz >= minLogHz)
      minLogMel + math.log(hz / minLogHz) / logStep
    else hz / linear
    def toHz(mel: Double) = if (mel >= minLogMel)
      minLogHz * math.exp(logStep * (mel - minLogMel))
    else linear * mel
    val (low, high) = (toMel(0.0), toMel(rate / 2))
    val edges =
      Array.tabulate(mels + 2)(i => toHz(low + (high - low) * i / (mels + 1)))
    val spacing = 1.0 / (n * (1.0 / rate))
    val frequencies = Array.tabulate(n / 2 + 1)(k => k * spacing)
    Array.tabulate(mels) { i =>
      val norm = 2.0 / (edges(i + 2) - edges(i))
      Array.tabulate(frequencies.length) { k =>
        val lower = -(edges(i) - frequencies(k)) / (edges(i + 1) - edges(i))
        val upper =
          (edges(i + 2) - frequencies(k)) / (edges(i + 2) - edges(i + 1))
        (math.max(0.0, math.min(lower, upper)).toFloat * norm).toFloat
      }
    }
  }

  /** How strongly each frame starts a note (`onset_strength`): the mean rise,
    * from the frame before, of the mel spectrum in decibels, delayed as librosa
    * delays it.
    */
  def onsetStrength(
      signal: Array[Float],
      rate: Double,
      n: Int,
      hop: Int
  ): Array[Float] = {
    val spectra = stft(signal, n, hop, hann(n))
    val filters = melFilters(rate, n, 128)
    val frames = spectra.size
    val mel = Array.ofDim[Float](frames * filters.length)
    // each filter's bins that are not nothing
    val spans = filters.map(filter =>
      (filter.indexWhere(_ != 0), filter.lastIndexWhere(_ != 0) + 1)
    )
    for ((spectrum, t) <- spectra.zipWithIndex) {
      val power = Array.tabulate(spectrum.length)(k =>
        (spectrum.re(k) * spectrum.re(k) + spectrum.im(k) * spectrum.im(
          k
        )).toFloat
      )
      for ((filter, m) <- filters.zipWithIndex) {
        var sum = 0.0
        val (from, until) = spans(m)
        for (k <- from until until) sum += filter(k) * power(k)
        mel(t * filters.length + m) = sum.toFloat
      }
    }
    val db = powerToDb(mel)
    val delay = 1 + n / (2 * hop)
    Array.tabulate(frames) { t =>
      val source = t - delay + 1
      if (source < 1) 0.0f
      else {
        var sum = 0.0
        for (m <- filters.indices) {
          sum += math.max(
            0.0f,
            db(source * filters.length + m) - db(
              (source - 1) * filters.length + m
            )
          )
        }
        (sum / filters.length).toFloat
      }
    }
  }
}
