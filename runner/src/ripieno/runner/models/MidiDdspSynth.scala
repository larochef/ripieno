package ripieno.runner.models

import ripieno.runner.models.MidiDdspLayers.variable
import ripieno.runner.weights.WeightSource

import java.util.stream.IntStream

/** MIDI-DDSP's synthesiser (ddsp's), 16 kHz, 64 samples a frame: 60 harmonics
  * of the pitch played, their loudness and distribution from the controls, and
  * noise through a filter of 65 bands a frame; then the instrument's reverb,
  * learnt, after the dry sound.
  */
final class MidiDdspSynth(weights: WeightSource) {
  import MidiDdspSynth.*

  private val reverbs =
    variable(weights, "reverb_module/magnitudes_embedding/embeddings")
  private val reverbLength = reverbs.length / 20

  /** The part's sound from its pitch (Hz) and raw controls, `noise` its uniform
    * noise in [-1, 1], a sample at a time.
    */
  def sound(
      f0Hz: Array[Float],
      controls: Array[Array[Float]],
      instrument: Int,
      noise: Array[Float]
  ): Array[Float] = {
    val dry = harmonic(f0Hz, controls)
    val filtered = filteredNoise(controls, noise)
    val signal = Array.tabulate(dry.length)(n => filtered(n) + dry(n))
    reverberated(signal, instrument)
  }

  /** The harmonics: each one's frequency interpolated linearly between frames,
    * its amplitude by raised cosines, its phase its own sum; silent at or above
    * Nyquist.
    */
  def harmonic(
      f0Hz: Array[Float],
      controls: Array[Array[Float]]
  ): Array[Float] = {
    val frames = f0Hz.length
    val samples = frames * FrameSize
    // the frames' amplitudes: the loudness times the harmonics' distribution,
    // those at or above Nyquist left out
    val amplitudes = Array.tabulate(frames) { k =>
      val loudness = expSigmoid(controls(k)(0))
      val distribution = Array.tabulate(Harmonics) { h =>
        if (f0Hz(k) * (h + 1).toFloat >= Nyquist) 0f
        else expSigmoid(controls(k)(1 + h))
      }
      val total = distribution.sum
      val divisor = if (total == 0) 1e-7f else total
      distribution.map(value => loudness * (value / divisor))
    }
    val out = new Array[Float](samples)
    val sums = new Array[Array[Float]](Harmonics)
    IntStream.range(0, Harmonics).parallel().forEach { h =>
      val number = (h + 1).toFloat
      val frequency = new Array[Float](samples)
      val amplitude = new Array[Float](samples)
      var n = 0
      while (n < samples) {
        val (k, j) = (n / FrameSize, n % FrameSize)
        val next = math.min(k + 1, frames - 1)
        val (here, there) = (f0Hz(k) * number, f0Hz(next) * number)
        frequency(n) = here + (there - here) * (j.toFloat / FrameSize)
        val (fade, rise) = (
          math.cos(math.Pi * j / (2 * FrameSize)),
          math.sin(math.Pi * j / (2 * FrameSize))
        )
        amplitude(n) = (amplitudes(k)(h) * fade * fade + amplitudes(next)(
          h
        ) * rise * rise).toFloat
        n += 1
      }
      val phase = angularSum(frequency.map(f => f * TwoPi / SampleRate))
      sums(h) = Array.tabulate(samples)(n =>
        if (frequency(n) >= Nyquist) 0f
        else amplitude(n) * math.sin(phase(n)).toFloat
      )
    }
    for {
      h <- 0 until Harmonics
      n <- 0 until samples
    } out(n) += sums(h)(n)
    out
  }

  /** The noise filtered a frame at a time: each frame's 65 bands as a filter of
    * 128 taps, symmetric, windowed (Hann); the frames added up, delayed as ddsp
    * delays them.
    */
  def filteredNoise(
      controls: Array[Array[Float]],
      noise: Array[Float]
  ): Array[Float] = {
    val frames = controls.length
    val samples = frames * FrameSize
    val window = Array.tabulate(FilterTaps)(m =>
      0.5 - 0.5 * math.cos(2 * math.Pi * m / FilterTaps)
    )
    val total =
      new Array[Double]((frames - 1) * FrameSize + FilterTaps + FrameSize)
    val responses = new Array[Array[Double]](frames)
    IntStream.range(0, frames).parallel().forEach { k =>
      val magnitudes = Array.tabulate(NoiseBands)(b =>
        expSigmoid(controls(k)(1 + Harmonics + b) + NoiseBias).toDouble
      )
      // the zero-phase filter of the bands, centred, windowed, then made causal
      val zeroPhase = Array.tabulate(FilterTaps) { n =>
        var sum =
          magnitudes(0) + magnitudes(NoiseBands - 1) * (if (n % 2 == 0) 1
                                                        else -1)
        for (b <- 1 until NoiseBands - 1)
          sum += 2 * magnitudes(b) * math.cos(2 * math.Pi * b * n / FilterTaps)
        sum / FilterTaps
      }
      responses(k) = Array.tabulate(FilterTaps) { i =>
        val n = (i + FilterTaps / 2) % FilterTaps
        zeroPhase(n) * window((n + FilterTaps / 2) % FilterTaps)
      }
    }
    for (k <- 0 until frames) {
      val response = responses(k)
      for (i <- 0 until FrameSize) {
        val value = noise(k * FrameSize + i)
        if (value != 0) for (tap <- 0 until FilterTaps)
          total(k * FrameSize + i + tap) += value * response(tap)
      }
    }
    Array.tabulate(samples)(n => total(n + NoiseDelay).toFloat)
  }

  /** The instrument's reverb after the dry sound: its learnt response, its tail
    * decaying after a second, convolved with the sound, the sound added.
    */
  def reverberated(signal: Array[Float], instrument: Int): Array[Float] = {
    val response = Array.tabulate(reverbLength) { i =>
      val decay =
        if (i < SampleRate) 1.0
        else math.exp(-4.0 * (i - SampleRate) / (reverbLength - SampleRate - 1))
      if (i == 0) 0.0 else reverbs(instrument * reverbLength + i) * decay
    }
    val wet = DdspFft.convolved(signal.map(_.toDouble), response, signal.length)
    Array.tabulate(signal.length)(n => (wet(n) + signal(n)).toFloat)
  }
}

object MidiDdspSynth {
  val SampleRate = 16000
  val FrameSize = 64
  private val Nyquist = SampleRate / 2f
  private val Harmonics = 60
  private val NoiseBands = 65
  private val FilterTaps = 2 * (NoiseBands - 1)
  // the noise's bands start this low (ddsp's `initial_bias`)
  private val NoiseBias = -5f
  // where ddsp crops the filtered noise: two samples after its filters' middle
  private val NoiseDelay = (FilterTaps - 1) / 2 - 1

  private val TwoPi = (2 * math.Pi).toFloat
  private val Chunk = 1000

  /** Each sample's phase, the angular frequencies summed as ddsp sums them (in
    * floats, by chunks of a thousand samples, each chunk from where the ones
    * before ended, modulo 2π): the same rounding, the same phase after minutes.
    */
  def angularSum(omega: Array[Float]): Array[Float] = {
    val out = new Array[Float](omega.length)
    var offset = 0f
    var start = 0
    while (start < omega.length) {
      var sum = 0f
      var last = 0f
      for (i <- 0 until Chunk) {
        sum += (if (start + i < omega.length) omega(start + i) else 0f)
        if (start + i < omega.length)
          out(start + i) = floorMod(sum + offset, TwoPi)
        last = sum
      }
      offset = floorMod(offset + floorMod(last, TwoPi), TwoPi)
      start += Chunk
    }
    out
  }

  private def floorMod(x: Float, y: Float): Float = {
    val r = x - (math.floor(x / y).toFloat * y)
    r
  }

  /** ddsp's `exp_sigmoid`: 2 σ(x)^ln 10 + 10⁻⁷. */
  def expSigmoid(x: Float): Float =
    (2 * math.pow(1 / (1 + math.exp(-x)), math.log(10)) + 1e-7).toFloat
}

/** Linear convolution by the fast Fourier transform, in blocks. */
object DdspFft {

  /** The first `length` samples of `signal` convolved with `response`. */
  def convolved(
      signal: Array[Double],
      response: Array[Double],
      length: Int
  ): Array[Double] = {
    val block = 1 << 16
    val size = Integer.highestOneBit(block + response.length - 1) * 2
    val (responseRe, responseIm) = transformed(response, size)
    val out = new Array[Double](length + size)
    var start = 0
    while (start < length) {
      val piece = Array.tabulate(size)(i =>
        if (i < block && start + i < signal.length) signal(start + i) else 0.0
      )
      val (re, im) = transformed(piece, size)
      for (k <- 0 until size) {
        val (a, b) = (
          re(k) * responseRe(k) - im(k) * responseIm(k),
          re(k) * responseIm(k) + im(k) * responseRe(k)
        )
        re(k) = a
        im(k) = -b
      }
      // the inverse transform as the forward one of the conjugate
      transform(re, im)
      for (i <- 0 until size if start + i < out.length)
        out(start + i) += re(i) / size
      start += block
    }
    out.take(length)
  }

  private def transformed(
      values: Array[Double],
      size: Int
  ): (Array[Double], Array[Double]) = {
    val re =
      Array.tabulate(size)(i => if (i < values.length) values(i) else 0.0)
    val im = new Array[Double](size)
    transform(re, im)
    (re, im)
  }

  // in place, radix 2, forward
  private def transform(re: Array[Double], im: Array[Double]): Unit = {
    val n = re.length
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
      val angle = -2 * math.Pi / size
      val cosines = Array.tabulate(half)(k => math.cos(angle * k))
      val sines = Array.tabulate(half)(k => math.sin(angle * k))
      var start = 0
      while (start < n) {
        var k = 0
        while (k < half) {
          val (a, b) = (start + k, start + k + half)
          val tr = re(b) * cosines(k) - im(b) * sines(k)
          val ti = re(b) * sines(k) + im(b) * cosines(k)
          re(b) = re(a) - tr; im(b) = im(a) - ti
          re(a) += tr; im(a) += ti
          k += 1
        }
        start += size
      }
      size *= 2
    }
  }
}
