package ripieno.runner.models

/** The noise a window is sampled from, drawn as PyTorch's `randn` draws it on
  * the processor from a seed: a Mersenne Twister (MT19937) seeded with the
  * seed's low 32 bits; each value first uniform, 24 of its bits over 2²⁴; then,
  * sixteen at a time, made normal by Box-Muller, the value j paired with j + 8;
  * the last sixteen drawn again when the count is no multiple of sixteen.
  *
  * Its values are PyTorch's to the last bit or one away: PyTorch's logarithm,
  * sine and cosine there are SLEEF's vectorised approximations, these Java's. A
  * standard normal noise either way, as the model was trained on.
  */
object ViolinNoise {

  def gaussian(seed: Long, count: Int): Array[Float] = {
    val twister = new MersenneTwister(seed)
    val values = Array.fill(count)(twister.uniform())
    for (start <- 0 until count - 15 by 16) normal(values, start)
    if (count % 16 != 0) {
      val start = count - 16
      for (index <- start until count) values(index) = twister.uniform()
      normal(values, start)
    }
    values
  }

  private val twoPi = (2 * math.Pi).toFloat

  // sixteen uniform values from `start` made normal, the value j paired with j + 8
  private def normal(values: Array[Float], start: Int): Unit =
    for (j <- start until start + 8) {
      val u1 = 1.0f - values(j)
      val u2 = values(j + 8)
      val radius = math.sqrt((-2.0f * math.log(u1).toFloat).toDouble).toFloat
      val theta = twoPi * u2
      values(j) = radius * math.cos(theta.toDouble).toFloat
      values(j + 8) = radius * math.sin(theta.toDouble).toFloat
    }

  /** MT19937, seeded as `init_genrand` does. */
  final private class MersenneTwister(seed: Long) {
    private val state = new Array[Int](624)
    private var index = 624
    state(0) = seed.toInt
    for (j <- 1 until 624) {
      state(j) = 1812433253 * (state(j - 1) ^ (state(j - 1) >>> 30)) + j
    }

    def next(): Int = {
      if (index >= 624) {
        for (k <- 0 until 624) {
          val y = (state(k) & 0x80000000) | (state((k + 1) % 624) & 0x7fffffff)
          state(k) = state(
            (k + 397) % 624
          ) ^ (y >>> 1) ^ (if ((y & 1) != 0) 0x9908b0df else 0)
        }
        index = 0
      }
      var y = state(index)
      index += 1
      y ^= y >>> 11
      y ^= (y << 7) & 0x9d2c5680
      y ^= (y << 15) & 0xefc60000
      y ^ (y >>> 18)
    }

    /** In [0, 1): 24 random bits over 2²⁴. */
    def uniform(): Float = (next() & 0xffffff) * (1.0f / (1 << 24))
  }
}
