package ripieno.runner.models

/** The noise levels VIOLET samples a window through: `steps` of them from 1 to
  * 0, evenly, as `torch.linspace` makes them in floats (from the start for the
  * first half, from the end for the second), to the bit.
  */
object ViolinSchedule {

  val Steps = 30

  def sigmas(steps: Int = Steps): Array[Float] = {
    val (start, end) = (1.0f, 0.0f)
    val step = ((end - start) / (steps - 1).toDouble).toFloat
    val halfway = steps / 2
    Array.tabulate(steps) { index =>
      // one rounding, as PyTorch's vectorised loop multiplies and adds
      if (index < halfway) Math.fma(step, index.toFloat, start)
      else Math.fma(-step, (steps - index - 1).toFloat, end)
    }
  }
}
