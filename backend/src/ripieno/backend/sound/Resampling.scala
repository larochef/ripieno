package ripieno.backend.sound

/** A sound resampled by a ratio `up / down`, as scipy's `resample_poly` does:
  * upsampled by `up`, filtered by a low-pass FIR (a sinc over ten times the
  * larger rate on each side, under a Kaiser window of β 5, its gain one),
  * downsampled by `down`, the output centred on the input.
  */
object Resampling {

  private val beta = 5.0

  def resampled(sound: Array[Float], up: Int, down: Int): Array[Float] = {
    val common = BigInt(up).gcd(BigInt(down)).toInt
    val (u, d) = (up / common, down / common)
    if (u == 1 && d == 1) sound.clone()
    else {
      val larger = math.max(u, d)
      val halfLength = 10 * larger
      // the filter as scipy has it: designed in doubles, applied in the
      // sound's floats
      val filter =
        lowPass(2 * halfLength + 1, 1.0 / larger).map(tap => tap.toFloat * u)
      val prePad = d - halfLength % d
      val preRemove = (halfLength + prePad) / d
      val produced = sound.length.toLong * u
      val length = (produced / d + (if (produced % d != 0) 1 else 0)).toInt
      Array.tabulate(length) { index =>
        // the output sample at the upsampled position `at`: the input samples
        // whose filter tap reaches it
        val at = (preRemove + index).toLong * d - prePad
        val first =
          math.max(0L, math.floorDiv(at - filter.length + 1 + u - 1, u.toLong))
        val last = math.min(sound.length - 1L, math.floorDiv(at, u.toLong))
        var sum = 0.0
        var input = first
        while (input <= last) {
          sum += sound(input.toInt) * filter((at - input * u).toInt).toDouble
          input += 1
        }
        sum.toFloat
      }
    }
  }

  /** `up / down` nearest a ratio among those whose `down` is at most `largest`,
    * as Python's `Fraction(ratio).limit_denominator(largest)` finds it.
    */
  def fraction(ratio: Double, largest: Int): (Int, Int) = {
    // the double's exact value: its 53 bits over a power of two
    val shift = 52 - Math.getExponent(ratio)
    val numerator = BigInt((ratio * math.pow(2, shift)).toLong)
    val denominator = BigInt(2).pow(shift)
    val common = numerator.gcd(denominator)
    val (top, bottom) = (numerator / common, denominator / common)
    if (bottom <= largest) (top.toInt, bottom.toInt)
    else {
      var (p0, q0, p1, q1) = (BigInt(0), BigInt(1), BigInt(1), BigInt(0))
      var (n, d) = (top, bottom)
      var going = true
      while (going) {
        val a = n / d
        val q2 = q0 + a * q1
        if (q2 > largest) going = false
        else {
          val (np1, nq1) = (p0 + a * p1, q2)
          p0 = p1; q0 = q1; p1 = np1; q1 = nq1
          val next = n - a * d
          n = d; d = next
        }
      }
      val k = (BigInt(largest) - q0) / q1
      if (2 * d * (q0 + k * q1) <= bottom) (p1.toInt, q1.toInt)
      else ((p0 + k * p1).toInt, (q0 + k * q1).toInt)
    }
  }

  // scipy's firwin(taps, cutoff, window=("kaiser", 5.0)): a windowed sinc
  // passing below `cutoff` (of the Nyquist rate), scaled to a gain of one
  private def lowPass(taps: Int, cutoff: Double): Array[Double] = {
    val alpha = 0.5 * (taps - 1)
    val raw = Array.tabulate(taps) { index =>
      val m = index - alpha
      cutoff * sinc(cutoff * m) * kaiser(index, taps)
    }
    val sum = raw.sum
    raw.map(_ / sum)
  }

  // numpy's sinc
  private def sinc(x: Double): Double = {
    val y = math.Pi * (if (x == 0) 1.0e-20 else x)
    math.sin(y) / y
  }

  private def kaiser(index: Int, taps: Int): Double = {
    val alpha = (taps - 1) / 2.0
    val ratio = (index - alpha) / alpha
    besselI0(beta * math.sqrt(1 - ratio * ratio)) / besselI0(beta)
  }

  // the modified Bessel function of order 0, by its series
  private def besselI0(x: Double): Double = {
    var term = 1.0
    var sum = 1.0
    var k = 1
    while (term > 1e-17 * sum) {
      term *= (x / (2 * k)) * (x / (2 * k))
      sum += term
      k += 1
    }
    sum
  }
}
