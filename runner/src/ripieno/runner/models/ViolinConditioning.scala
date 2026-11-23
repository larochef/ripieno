package ripieno.runner.models

import ripieno.runner.ops.*
import ripieno.runner.tensor.*
import ripieno.runner.weights.WeightSource

/** A window's conditions as VIOLET's network takes them, made from its rolls
  * (`ViolinWindow`), the network's embedders as its reference implementation
  * runs them while sampling:
  *   - the notes: the roll's harmonics (three causal convolutions, the
  *     strongest four of their 84 bins kept at each frame), embedded;
  *   - the techniques: the roll projected (a 1×1 convolution), taken at each
  *     latent frame, embedded;
  *   - the dynamics: the window's curve, embedded. The reference's inference
  *     drops them (its guidance weight for them 0, every frame given the learnt
  *     "no dynamics"), and plays every note forte; with both weights at 1, its
  *     guidance is this one prediction from all the conditions.
  *
  * An embedding is a linear layer, a SiLU and a layer norm.
  */
final class ViolinConditioning(ops: Ops, weights: WeightSource)
    extends AutoCloseable {
  import ViolinConditioning.*

  private val scope = new TensorScope(ops)

  // the convolutions' kernels as the convolution takes them: F32 `[out, in × taps]`
  private def kernel(name: String): Tensor = {
    val shape = weights.shape(s"$name.weight").dimensions
    scope.fromFloats(
      Shape.of(shape(0), shape.drop(1).product),
      weights.hostValues(s"$name.weight")
    )
  }
  private def bias(name: String): Tensor = {
    val values = weights.hostValues(s"$name.bias")
    scope.fromFloats(Shape.of(values.length.toLong), values)
  }

  private val convolutions = Seq(1, 2, 3).map(index =>
    (
      kernel(s"midi_harmonic_extractor.conv$index"),
      bias(s"midi_harmonic_extractor.conv$index")
    )
  )
  private val techniqueProjection =
    (kernel("tech_roll_extractor.proj"), bias("tech_roll_extractor.proj"))

  private def buffer(dimensions: Long*): Tensor =
    scope.allocate(DType.F32, Shape.of(dimensions*))

  private val first = buffer(ViolinWindows.RollFrames / 2, 256)
  private val second = buffer(Frames, 512)
  private val bins = buffer(Frames, Bins)
  private val projected =
    buffer(ViolinWindows.RollFrames, ViolinWindows.Techniques)

  /** The window's conditions, each `[frames, width]`, held by the caller. */
  def conditions(window: ViolinWindow): ViolinConditions = {
    val notes = embedded(harmonics(window), Bins, "midi_roll_frame_embedder")

    val techniqueRoll = rollTensor(window.techniques, ViolinWindows.Techniques)
    try {
      ops.conv1d(
        techniqueRoll,
        techniqueProjection._1,
        Some(techniqueProjection._2),
        1,
        1,
        1,
        0,
        0,
        projected
      )
    } finally ops.release(techniqueRoll)
    val techniques = embedded(
      nearestFrames(ops.toFloats(projected), ViolinWindows.Techniques),
      ViolinWindows.Techniques,
      "tech_roll_frame_embedder"
    )

    val dynamics = embedded(window.dynamics, 1, "cc_frame_embedder")
    ViolinConditions(notes, techniques, dynamics)
  }

  // the notes' harmonics, `[frames, bins]`: each frame's strongest bins
  private def harmonics(window: ViolinWindow): Array[Float] = {
    val ((kernel1, bias1), (kernel2, bias2), (kernel3, bias3)) =
      (convolutions(0), convolutions(1), convolutions(2))
    val roll = rollTensor(window.notes, ViolinWindows.Pitches)
    try ops.conv1d(roll, kernel1, Some(bias1), 5, 1, 2, 4, 0, first)
    finally ops.release(roll)
    ops.activation(Activation.Silu, first, first)
    ops.conv1d(first, kernel2, Some(bias2), 3, 1, 2, 2, 0, second)
    ops.activation(Activation.Silu, second, second)
    ops.conv1d(second, kernel3, Some(bias3), 3, 1, 1, 2, 0, bins)
    strongest(ops.toFloats(bins))
  }

  private def rollTensor(values: Array[Float], rows: Int): Tensor =
    ops.fromFloats(
      Shape.of(ViolinWindows.RollFrames, rows),
      framesFirst(values, rows)
    )

  // a frame's features through an embedder: linear, SiLU, layer norm
  private def embedded(
      features: Array[Float],
      width: Int,
      embedder: String
  ): Tensor = {
    val input = ops.fromFloats(Shape.of(Frames, width), features)
    val out = ops.allocate(DType.F32, Shape.of(Frames, Width))
    try {
      ops.linear(input, weights(s"$embedder.proj.weight"), out)
      ops.addRow(out, weights(s"$embedder.proj.bias"), out)
      ops.activation(Activation.Silu, out, out)
      ops.layerNorm(
        out,
        Some(weights(s"$embedder.norm.weight")),
        Some(weights(s"$embedder.norm.bias")),
        1e-5f,
        out
      )
    } finally {
      ops.release(input)
    }
    out
  }

  def close(): Unit = scope.close()
}

object ViolinConditioning {

  val Frames: Int = ViolinWindows.WindowFrames
  val Width = 768
  // the harmonics' bins, and how many of them a frame keeps (the inference
  // experiment's `midi_roll_top_k`, which the model's default of 5 gives way to)
  private val Bins = 84
  private val Strongest = 4

  /** A roll stored row by row (`[rows, frames]`, as the windows are) laid out
    * frame by frame (`[frames, rows]`, channels last).
    */
  def framesFirst(values: Array[Float], rows: Int): Array[Float] = {
    val frames = values.length / rows
    Array.tabulate(values.length)(index =>
      values((index % rows) * frames + index / rows)
    )
  }

  /** The harmonics through a ReLU, each frame keeping its strongest bins only
    * (`topk` then `scatter`), `[frames, bins]`.
    */
  def strongest(bins: Array[Float]): Array[Float] = {
    val kept = new Array[Float](bins.length)
    for (frame <- 0 until bins.length / Bins) {
      val row = frame * Bins
      (0 until Bins)
        .map(bin => bin -> math.max(0.0f, bins(row + bin)))
        .sortBy(-_._2)
        .take(Strongest)
        .foreach((bin, value) => kept(row + bin) = value)
    }
    kept
  }

  /** `[rollFrames, channels]` at the latent frames, each taking the roll frame
    * nearest below it (`interpolate(mode="nearest")`).
    */
  def nearestFrames(values: Array[Float], channels: Int): Array[Float] = {
    val frames = values.length / channels
    Array.tabulate(Frames * channels) { index =>
      val source = (index / channels).toLong * frames / Frames
      values(source.toInt * channels + index % channels)
    }
  }
}
