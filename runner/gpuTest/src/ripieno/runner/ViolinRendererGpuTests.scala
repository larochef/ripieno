package ripieno.runner

import ripieno.backend.models.{Download, Downloaded}
import ripieno.runner.models.*
import ripieno.runner.native.HipRuntime
import ripieno.runner.ops.{HipOps, MatVecInputs, Ops}
import ripieno.runner.tensor.Shape
import ripieno.runner.weights.WeightSource

import java.nio.file.Path

import utest.*

/** VIOLET on the GPU, from the weights as published (`Downloaded`), in launches
  * that leave the GPU to the desktop: a tenth of a window decoded first, then a
  * whole window of a line rendered (`violet/synthetic`), every launch watched.
  * That the sound is VIOLET's was checked against its reference implementation
  * when it was ported. Skipped where the weights are not there.
  */
object ViolinRendererGpuTests extends TestSuite {

  private lazy val weights = Downloaded.folder(Download.Violet)
  private lazy val files =
    Seq(VioletWeights.NetworkFile, VioletWeights.DecoderFile).map(
      weights.resolve
    )

  private def withWeights(body: (Ops, WeightSource, WeightSource) => Unit) =
    if (!Gpu.enabled) Gpu.run(())
    else {
      val ops = new HipOps(Gpu.hip, MatVecInputs.Int8)
      val network = VioletWeights.network(ops, files(0))
      val decoder = VioletWeights.decoder(ops, files(1))
      try body(ops, network, decoder)
      finally {
        Seq(network, decoder).foreach(_.close())
        ops.close()
      }
    }

  private def printed(longest: Map[String, Double]): Unit =
    longest.toSeq
      .sortBy(-_._2)
      .take(6)
      .foreach((kernel, millis) =>
        println(f"  $kernel: $millis%.2f ms at most")
      )

  // The whole window only runs once a tenth of it has shown launches well
  // under the limit. Work is cut into launches of a set size, so a longer
  // window means more launches, not longer ones. The bound is on each
  // kernel's average: a single launch also waits for whatever else uses the
  // GPU, the desktop first, and now and then takes ten times its usual time.
  private var shortRunSlowest = Option.empty[Double]
  private val shortRunBound = Gpu.LongestLaunchMillis / 2

  val tests = Tests {
    test("a tenth of a window decoded first") {
      withWeights { (ops, _, decoderWeights) =>
        val decoder = new ViolinDecoder(ops, decoderWeights)
        val latents = ops.fromFloats(
          Shape.of(25, ViolinDiffusion.LatentChannels),
          TestData.gaussian(7, 25 * ViolinDiffusion.LatentChannels)
        )
        // once before the watch: a kernel's first launch also loads it
        ops.release(decoder.stages(latents))
        val longest = HipRuntime.Trace.watch {
          ops.release(decoder.stages(latents))
        }
        printed(longest)
        shortRunSlowest =
          Some(HipRuntime.Trace.statistics.values.map((_, mean, _) => mean).max)
        ops.release(latents)
        assert(shortRunSlowest.exists(_ < shortRunBound))
        assert(longest.values.max < Gpu.LongestLaunchMillis)
      }
    }
    test("a window rendered, no launch keeping the GPU from the desktop") {
      withWeights { (ops, network, decoder) =>
        assert(shortRunSlowest.exists(_ < shortRunBound))
        val line = Path.of(
          getClass.getResource("/violet/synthetic/part0-section0.notes").toURI
        )
        val window = ViolinWindows.windows(ViolinNote.read(line)).head
        val renderer = new ViolinRenderer(
          ops,
          network,
          decoder,
          ViolinSchedule.sigmas().toIndexedSeq
        )
        try {
          // once unwatched: the first launches also map fresh memory
          renderer.audio(window, 1)
          val started = System.nanoTime()
          var audio = Array.emptyFloatArray
          val longest = HipRuntime.Trace.watch {
            audio = renderer.audio(window, 2)
          }
          println(
            f"  a window in ${(System.nanoTime() - started) / 1e9}%.1f s, every launch timed"
          )
          printed(longest)
          val rms = math.sqrt(audio.map(v => v.toDouble * v).sum / audio.length)
          assert(audio.nonEmpty, audio.forall(!_.isNaN), rms > 1e-3)
          assert(longest.values.max < Gpu.LongestLaunchMillis)
        } finally renderer.close()
      }
    }
  }
}
