package ripieno.runner.models

import ripieno.runner.ops.*
import ripieno.runner.tensor.*
import ripieno.runner.weights.WeightSource

/** What a window of music asks of the violin, a row per latent frame: the
  * notes, the playing techniques and the dynamics, each already embedded to the
  * network's width (`[frames, width]`).
  */
final case class ViolinConditions(
    notes: Tensor,
    techniques: Tensor,
    dynamics: Tensor
)

/** VIOLET's network (Tian et al., ISMIR 2026): a diffusion transformer that
  * predicts the velocity of a window of audio latents, 25 frames a second, from
  * the notes, techniques and dynamics of that window.
  *
  * Every block is modulated per frame (AdaLN) by the time step and by the three
  * conditions. The conditions do not change while a window is sampled, so their
  * share of every block's modulation is worked out once per window
  * (`condition`), and a step only adds the time step's.
  */
final class ViolinDiffusion(ops: Ops, weights: WeightSource, val frames: Int)
    extends AutoCloseable {
  import ViolinDiffusion.*

  private val scope = new TensorScope(ops)

  private def buffer(dimensions: Long*): Tensor =
    scope.allocate(DType.F32, Shape.of(dimensions*))

  private val blockNames = (0 until Blocks).map(index => s"blocks.$index")

  // the conditions' share of each block's six modulation chunks, and of the final layer's two
  private val blockConditions = blockNames.map(_ => buffer(frames, 6L * Width))
  private val finalConditions = buffer(frames, 2L * Width)

  private val blockModulation = buffer(frames, 6L * Width)
  private val finalModulation = buffer(frames, 2L * Width)
  private val timeRow = buffer(1, 6L * Width)

  private val x = buffer(frames, Width)
  private val normed = buffer(frames, Width)
  private val projected = buffer(frames, Width)
  private val queries = buffer(frames, Width)
  private val keys = buffer(frames, Width)
  private val values = buffer(frames, Width)
  private val turnedQueries = buffer(frames, Width)
  private val turnedKeys = buffer(frames, Width)
  private val attended = buffer(frames, Width)
  private val mlpBoth = buffer(frames, 2L * MlpWidth)
  private val mlpUp = buffer(frames, MlpWidth)
  private val mlpGate = buffer(frames, MlpWidth)
  private val condition = buffer(frames, Width)
  private val conditionShare = buffer(frames, 6L * Width)
  private val time = buffer(1, Width)
  private val timeHidden = buffer(1, Width)

  // Half of each head turns with its frame's position, the other half is left alone.
  private val (cosines, sines) = {
    val angles = for {
      frame <- 0 until frames
      pair <- 0 until RotaryPairs
    } yield frame * math.pow(RotaryBase, -2.0 * pair / HeadWidth)
    val shape = Shape.of(frames, RotaryPairs)
    (
      scope.fromFloats(shape, angles.map(math.cos(_).toFloat).toArray),
      scope.fromFloats(shape, angles.map(math.sin(_).toFloat).toArray)
    )
  }

  private def affine(input: Tensor, layer: String, out: Tensor): Unit = {
    ops.linear(input, weights(s"$layer.weight"), out)
    ops.addRow(out, weights(s"$layer.bias"), out)
  }

  /** Takes in the conditions of the window sampled from now on. */
  def condition(conditions: ViolinConditions): Unit = {
    val sources = Seq(
      "cc_modulation" -> conditions.dynamics,
      "midi_roll_modulation" -> conditions.notes,
      "tech_roll_modulation" -> conditions.techniques
    )
    def shares(owner: String, total: Tensor): Unit = {
      val share = conditionShare.prefix(frames, total.shape.last)
      ops.zero(total)
      sources.foreach { (modulation, features) =>
        ops.activation(Activation.Silu, features, condition)
        affine(condition, s"$owner.$modulation.1", share)
        ops.add(total, share, total)
      }
    }
    blockNames
      .zip(blockConditions)
      .foreach((block, total) => shares(block, total))
    shares("final_layer", finalConditions)
  }

  /** The velocity of `latent` (`[frames, 128]`) at `sigma`, into `out`. */
  def velocity(latent: Tensor, sigma: Float, out: Tensor): Unit = {
    affine(latent, "x_embedder.proj", x)
    ops.layerNorm(
      x,
      Some(weights("x_embedder.norm.weight")),
      Some(weights("x_embedder.norm.bias")),
      1e-5f,
      x
    )

    val sinusoid = ops.fromFloats(Shape.of(1, Width), timeSinusoid(sigma))
    try {
      affine(sinusoid, "t_embedder.mlp.0", timeHidden)
    } finally {
      ops.release(sinusoid)
    }
    ops.activation(Activation.Silu, timeHidden, timeHidden)
    affine(timeHidden, "t_embedder.mlp.2", time)
    // every modulation starts from the time step through a SiLU
    ops.activation(Activation.Silu, time, time)

    blockNames.zip(blockConditions).foreach { (block, conditions) =>
      affine(time, s"$block.adaLN_modulation.1", timeRow)
      ops.addRow(conditions, timeRow.view(6L * Width), blockModulation)

      ops.layerNorm(x, None, None, NormEpsilon, normed)
      ops.modulateChunks(
        normed,
        blockModulation,
        AttentionShift,
        AttentionScale,
        normed
      )
      val keyAndValue = weights(s"$block.self_attn.to_kv.weight")
      ops.linears(
        normed,
        Seq(
          weights(s"$block.self_attn.to_q.weight"),
          keyAndValue.rows(0, Width),
          keyAndValue.rows(Width, Width)
        ),
        Seq(queries, keys, values)
      )
      ops.ropeTable(heads(queries), cosines, sines, heads(turnedQueries))
      ops.ropeTable(heads(keys), cosines, sines, heads(turnedKeys))
      ops.shortAttention(
        sequence(turnedQueries),
        sequence(turnedKeys),
        sequence(values),
        AttentionScaling,
        sequence(attended)
      )
      ops.linear(
        attended,
        weights(s"$block.self_attn.to_out.weight"),
        projected
      )
      ops.gatedAddChunk(x, projected, blockModulation, AttentionGate)

      ops.layerNorm(x, None, None, NormEpsilon, normed)
      ops.modulateChunks(normed, blockModulation, MlpShift, MlpScale, normed)
      affine(normed, s"$block.mlp.glu.proj", mlpBoth)
      ops.splitHalves(mlpBoth, mlpUp, mlpGate)
      ops.gated(Activation.Silu, mlpGate, mlpUp, mlpUp)
      affine(mlpUp, s"$block.mlp.fc2", projected)
      ops.gatedAddChunk(x, projected, blockModulation, MlpGate)
    }

    val finalRow = timeRow.prefix(1, 2L * Width)
    affine(time, "final_layer.adaLN_modulation.1", finalRow)
    ops.addRow(finalConditions, finalRow.view(2L * Width), finalModulation)
    ops.layerNorm(x, None, None, NormEpsilon, normed)
    ops.modulateChunks(normed, finalModulation, 0, 1, normed)
    affine(normed, "final_layer.linear", out)
  }

  private def heads(tensor: Tensor): Tensor =
    tensor.view(frames, Heads, HeadWidth)

  // one sequence of `frames` positions
  private def sequence(tensor: Tensor): Tensor =
    tensor.view(frames, 1, Heads, HeadWidth)

  override def close(): Unit = {
    scope.close()
  }
}

object ViolinDiffusion {
  val LatentChannels = 128
  val Width = 768

  private val Blocks = 12
  private val Heads = 12
  private val HeadWidth = Width / Heads
  private val MlpWidth = 4 * Width
  private val NormEpsilon = 1e-6f
  private val AttentionScaling = (1.0 / math.sqrt(HeadWidth)).toFloat

  private val RotaryBase = 10000.0
  private val RotaryPairs = HeadWidth / 4

  // the six chunks of a block's modulation, in the order the network stores them
  private val AttentionShift = 0
  private val AttentionScale = 1
  private val AttentionGate = 2
  private val MlpShift = 3
  private val MlpScale = 4
  private val MlpGate = 5

  private val TimePeriod = 10000.0

  /** The time step as cosines then sines of `Width / 2` frequencies. */
  private def timeSinusoid(sigma: Float): Array[Float] = {
    val half = Width / 2
    val angles = (0 until half).map(index =>
      sigma * math.exp(-math.log(TimePeriod) * index / half)
    )
    (angles.map(math.cos(_).toFloat) ++ angles.map(math.sin(_).toFloat)).toArray
  }
}
