package ripieno.runner.models

import ripieno.runner.weights.WeightSource

import java.util.stream.IntStream

/** MIDI-DDSP's layers on the processor, in floats as Keras runs them, a
  * sequence a matrix `[frames][width]`. MIDI-DDSP runs on the processor: in a
  * rendering it plays the parts the violin does not while VIOLET samples on the
  * GPU.
  */
object MidiDdspLayers {

  /** A Keras layer's variable, by its path in the model's objects. */
  def variable(weights: WeightSource, path: String): Array[Float] =
    weights.hostValues(s"$path/.ATTRIBUTES/VARIABLE_VALUE")

  def dimensions(weights: WeightSource, path: String): Seq[Long] =
    weights.shape(s"$path/.ATTRIBUTES/VARIABLE_VALUE").dimensions.toSeq

  /** `[frames][in] · kernel [in, out] + bias`, the frames on all the cores. */
  def dense(
      x: Array[Array[Float]],
      kernel: Array[Float],
      bias: Array[Float]
  ): Array[Array[Float]] = {
    val out = bias.length
    val in = kernel.length / out
    val result = new Array[Array[Float]](x.length)
    IntStream.range(0, x.length).parallel().forEach { t =>
      result(t) = denseRow(x(t), kernel, bias, in, out)
    }
    result
  }

  /** One row through `kernel [in, out]` and `bias`. */
  def denseRow(
      row: Array[Float],
      kernel: Array[Float],
      bias: Array[Float],
      in: Int,
      out: Int
  ): Array[Float] = {
    val sums = bias.clone()
    var i = 0
    while (i < in) {
      val value = row(i)
      if (value != 0) {
        var j = 0
        val offset = i * out
        while (j < out) {
          sums(j) += value * kernel(offset + j)
          j += 1
        }
      }
      i += 1
    }
    sums
  }

  /** A Keras `Dense` by its path. */
  final case class Dense(kernel: Array[Float], bias: Array[Float]) {
    val out: Int = bias.length
    val in: Int = kernel.length / out
    def apply(x: Array[Array[Float]]): Array[Array[Float]] =
      dense(x, kernel, bias)
    def row(x: Array[Float]): Array[Float] = denseRow(x, kernel, bias, in, out)
  }

  object Dense {
    def apply(weights: WeightSource, path: String): Dense =
      Dense(
        variable(weights, s"$path/kernel"),
        variable(weights, s"$path/bias")
      )
  }

  /** Keras' `LayerNormalization`: over the last axis, ε 10⁻³. */
  def layerNorm(
      row: Array[Float],
      gamma: Array[Float],
      beta: Array[Float],
      epsilon: Float = 1e-3f
  ): Array[Float] = {
    val n = row.length
    var mean = 0.0
    for (value <- row) mean += value
    mean /= n
    var variance = 0.0
    for (value <- row) variance += (value - mean) * (value - mean)
    variance /= n
    val scale = 1 / math.sqrt(variance + epsilon)
    Array.tabulate(n)(i =>
      ((row(i) - mean) * scale * gamma(i) + beta(i)).toFloat
    )
  }

  /** ddsp's `Normalize("layer")`: the mean and variance over all of `x` (one
    * frame's channels in a step, a whole sequence's frames and channels in a
    * stack), ε 10⁻⁵, then `scale` and `shift` per channel.
    */
  def ddspNormalize(
      x: Array[Array[Float]],
      scale: Array[Float],
      shift: Array[Float]
  ): Array[Array[Float]] = {
    val count = x.length.toDouble * scale.length
    var mean = 0.0
    for {
      row <- x
      value <- row
    } mean += value
    mean /= count
    var variance = 0.0
    for {
      row <- x
      value <- row
    } variance += (value - mean) * (value - mean)
    variance /= count
    val inverse = 1 / math.sqrt(variance + 1e-5)
    x.map(row =>
      Array.tabulate(row.length)(c =>
        ((row(c) - mean) * inverse * scale(c) + shift(c)).toFloat
      )
    )
  }

  def leakyRelu(x: Float): Float = if (x >= 0) x else 0.2f * x

  def sigmoid(x: Float): Float = (1 / (1 + math.exp(-x))).toFloat

  /** A Keras GRU (`reset_after`): `kernel [in, 3u]`, `recurrent [u, 3u]`,
    * `bias [2, 3u]` (the input's, then the state's), the gates `[z | r | h]`.
    */
  final case class Gru(
      kernel: Array[Float],
      recurrent: Array[Float],
      bias: Array[Float]
  ) {
    val units: Int = bias.length / 6
    val in: Int = kernel.length / (3 * units)
    val inputBias: Array[Float] = bias.take(3 * units)
    val stateBias: Array[Float] = bias.drop(3 * units)

    /** Inputs through the kernel, every frame at once: `[frames][3u]`. */
    def projected(x: Array[Array[Float]]): Array[Array[Float]] =
      dense(x, kernel, inputBias)

    /** The next state, from an input already projected and the state. */
    def step(input: Array[Float], state: Array[Float]): Array[Float] = {
      val u = units
      val fromState = denseRow(state, recurrent, stateBias, u, 3 * u)
      Array.tabulate(u) { i =>
        val z = sigmoid(input(i) + fromState(i))
        val r = sigmoid(input(u + i) + fromState(u + i))
        val candidate =
          math.tanh(input(2 * u + i) + r * fromState(2 * u + i)).toFloat
        z * state(i) + (1 - z) * candidate
      }
    }

    /** The states over a sequence, from zeros, forwards or backwards (the
      * outputs then in the sequence's order).
      */
    def run(
        x: Array[Array[Float]],
        backwards: Boolean = false
    ): Array[Array[Float]] = {
      val inputs = projected(x)
      val states = new Array[Array[Float]](x.length)
      var state = new Array[Float](units)
      val order = if (backwards) x.indices.reverse else x.indices
      for (t <- order) {
        state = step(inputs(t), state)
        states(t) = state
      }
      states
    }
  }

  object Gru {
    def apply(weights: WeightSource, path: String): Gru =
      Gru(
        variable(weights, s"$path/kernel"),
        variable(weights, s"$path/recurrent_kernel"),
        variable(weights, s"$path/bias")
      )
  }

  /** A Keras `Bidirectional` GRU, its outputs `[forward | backward]`. */
  def bidirectional(
      weights: WeightSource,
      path: String,
      x: Array[Array[Float]]
  ): Array[Array[Float]] = {
    val forward = Gru(weights, s"$path/forward_layer/cell").run(x)
    val backward =
      Gru(weights, s"$path/backward_layer/cell").run(x, backwards = true)
    x.indices.map(t => forward(t) ++ backward(t)).toArray
  }

  /** ddsp's `FcStackOut`: [Dense, LayerNorm, leaky ReLU] a layer, then a linear
    * Dense, the frames on all the cores.
    */
  final class FcStackOut(weights: WeightSource, path: String, layers: Int) {
    private val stack = (0 until layers).map { i =>
      val layer = s"$path/stack/layer_with_weights-$i"
      (
        Dense(weights, s"$layer/layer_with_weights-0"),
        variable(weights, s"$layer/layer_with_weights-1/gamma"),
        variable(weights, s"$layer/layer_with_weights-1/beta")
      )
    }
    private val out = Dense(weights, s"$path/dense_out")

    def row(x: Array[Float]): Array[Float] =
      out.row(
        stack.foldLeft(x)((h, layer) =>
          layerNorm(layer._1.row(h), layer._2, layer._3).map(leakyRelu)
        )
      )

    def apply(x: Array[Array[Float]]): Array[Array[Float]] = {
      val result = new Array[Array[Float]](x.length)
      IntStream
        .range(0, x.length)
        .parallel()
        .forEach(t => result(t) = row(x(t)))
      result
    }
  }
}
