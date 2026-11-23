package ripieno.runner.models

import ripieno.runner.ops.Ops
import ripieno.runner.tensor.Tensor

/** Follows a rectified flow from noise to a sample: an Euler step along the
  * velocity, corrected by the velocity found where the step lands (Heun),
  * except for the last step, which lands on the sample itself.
  */
object HeunSampler {

  /** Turns `x`, noise on entry, into the sample, in place.
    *
    * @param sigmas
    *   the noise levels to pass through, from 1 down to 0
    * @param velocity
    *   the velocity of a tensor at a noise level, written to its last argument
    * @param work
    *   three tensors shaped like `x`
    */
  def sample(ops: Ops, x: Tensor, sigmas: IndexedSeq[Float], work: Seq[Tensor])(
      velocity: (Tensor, Float, Tensor) => Unit
  ): Unit = {
    val Seq(first, second, landing) = work
    ops.scale(x, sigmas.head, x)
    sigmas.zip(sigmas.tail).foreach { (sigma, next) =>
      val step = next - sigma
      velocity(x, sigma, first)
      if (next == 0f) {
        ops.scale(first, step, first)
        ops.add(x, first, x)
      } else {
        ops.scale(first, step, landing)
        ops.add(x, landing, landing)
        velocity(landing, next, second)
        ops.add(first, second, first)
        ops.scale(first, step / 2, first)
        ops.add(x, first, x)
      }
    }
  }
}
