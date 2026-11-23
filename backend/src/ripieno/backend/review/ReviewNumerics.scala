package ripieno.backend.review

import java.math.{BigDecimal, RoundingMode}

/** The arithmetic the review's verdicts were set with (NumPy's and Python's),
  * kept as it was: a verdict on the edge of a threshold falls the same way.
  */
object ReviewNumerics {

  /** Python's `a[from:until]` on a sequence of `length`: negative bounds count
    * from the end, bounds past it stop there. The span, as `(first, last)` with
    * `last` excluded, empty when `last <= first`.
    */
  def span(length: Int, from: Int, until: Int): (Int, Int) = {
    def bound(at: Int) =
      math.max(0, math.min(length, if (at < 0) at + length else at))
    (bound(from), bound(until))
  }

  /** NumPy's median: NaN for none, which no comparison holds for. */
  def median(values: Seq[Double]): Double = {
    val sorted = values.sorted
    val size = sorted.size
    if (size == 0) Double.NaN
    else if (size % 2 == 1) sorted(size / 2)
    else (sorted(size / 2 - 1) + sorted(size / 2)) / 2
  }

  /** NumPy's percentile, by linear interpolation between the closest ranks. */
  def percentile(values: Seq[Double], percent: Double): Double = {
    val sorted = values.sorted
    val rank = percent / 100 * (sorted.size - 1)
    val below = math.floor(rank).toInt
    val above = math.min(below + 1, sorted.size - 1)
    sorted(below) + (rank - below) * (sorted(above) - sorted(below))
  }

  /** NumPy's `interp`: piecewise linear through `xs` (increasing) and `ys`, the
    * end values beyond them.
    */
  def interpolated(x: Double, xs: Array[Double], ys: Array[Double]): Double =
    if (x <= xs.head) ys.head
    else if (x >= xs.last) ys.last
    else {
      var low = 0
      var high = xs.length - 1
      while (high - low > 1) {
        val middle = (low + high) / 2
        if (xs(middle) <= x) low = middle else high = middle
      }
      val share = (x - xs(low)) / (xs(high) - xs(low))
      ys(low) + share * (ys(high) - ys(low))
    }

  /** The least-squares line through points: (slope, intercept). */
  def line(xs: Seq[Double], ys: Seq[Double]): (Double, Double) = {
    val n = xs.size
    val meanX = xs.sum / n
    val meanY = ys.sum / n
    val slope = xs.zip(ys).map((x, y) => (x - meanX) * (y - meanY)).sum /
      xs.map(x => (x - meanX) * (x - meanX)).sum
    (slope, meanY - slope * meanX)
  }

  /** Python's `round(x)`: to the nearest integer, half to even. */
  def rounded(x: Double): Long = math.rint(x).toLong

  /** Python's `round(x, digits)`: the exact value to `digits` decimals, half to
    * even.
    */
  def rounded(x: Double, digits: Int): Double =
    BigDecimal(x).setScale(digits, RoundingMode.HALF_EVEN).doubleValue

  /** The first index of the smallest value. */
  def argmin(values: Seq[Double]): Int = values.indices.minBy(values)

  /** The first index of the largest value. */
  def argmax(values: Seq[Double]): Int = {
    var best = 0
    for (index <- values.indices) if (values(index) > values(best)) best = index
    best
  }

  /** NumPy's `arange(start, stop, step)` for floats. */
  def arange(start: Double, stop: Double, step: Double): Vector[Double] = {
    val count = math.max(0, math.ceil((stop - start) / step).toInt)
    Vector.tabulate(count)(index => start + index * step)
  }
}
