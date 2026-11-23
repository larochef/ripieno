package ripieno.runner.models

import ripieno.runner.ops.Ops
import ripieno.runner.tensor.{DType, Shape}
import ripieno.runner.weights.WeightSource

import scala.util.Random

/** A window's audio from its rolls and a seed: conditioned
  * (`ViolinConditioning`), sampled through `sigmas` from the seed's noise
  * (`ViolinNoise`, `HeunSampler`), decoded (`ViolinDecoder`), 48 kHz, one
  * channel.
  */
final class ViolinRenderer(
    ops: Ops,
    network: WeightSource,
    decoderWeights: WeightSource,
    sigmas: IndexedSeq[Float]
) extends AutoCloseable {

  private val decoder = new ViolinDecoder(ops, decoderWeights)
  private val diffusion =
    new ViolinDiffusion(ops, network, ViolinWindows.WindowFrames)
  private val conditioning = new ViolinConditioning(ops, network)
  private val noiseValues =
    ViolinDiffusion.LatentChannels * ViolinWindows.WindowFrames
  // the window whose conditions the network holds: its attempts are sampled in a row
  private var conditioned = Option.empty[ViolinWindow]

  def audio(window: ViolinWindow, seed: Long): Array[Float] = {
    if (!conditioned.exists(_ eq window)) {
      val conditions = conditioning.conditions(window)
      try diffusion.condition(conditions)
      finally
        Seq(conditions.notes, conditions.techniques, conditions.dynamics)
          .foreach(ops.release)
      conditioned = Some(window)
    }
    val shape =
      Shape.of(ViolinWindows.WindowFrames, ViolinDiffusion.LatentChannels)
    val noise = ViolinConditioning.framesFirst(
      ViolinNoise.gaussian(seed, noiseValues),
      ViolinDiffusion.LatentChannels
    )
    val x = ops.fromFloats(shape, noise)
    val work = Seq.fill(3)(ops.allocate(DType.F32, shape))
    try {
      HeunSampler.sample(ops, x, sigmas, work)(diffusion.velocity)
      // the decoder's second half carries a message, which the reference draws at random
      val message = Seq.fill(ViolinDecoder.MessageBits)(Random.nextBoolean())
      decoder.audioOfSteadyMark(x, message)
    } finally {
      (x +: work).foreach(ops.release)
    }
  }

  def close(): Unit = {
    conditioning.close()
    diffusion.close()
  }
}
