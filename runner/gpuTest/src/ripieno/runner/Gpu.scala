package ripieno.runner

import ripieno.backend.models.{Download, Downloaded}
import ripieno.runner.native.HipRuntime

/** The one HIP runtime of the test process. */
object Gpu {

  /** The runtime of the ROCm tree the server downloads, downloaded first when
    * it is not there.
    */
  lazy val hip: HipRuntime = new HipRuntime(Downloaded.folder(Download.Rocm))

  /** What a single launch may take before the desktop, which shares the GPU,
    * misses frames. Past a few seconds its graphics ring times out and the
    * whole session goes down with it.
    */
  val LongestLaunchMillis = 50.0

  /** Tests of this module only touch the GPU when asked to: `./mill __.test`
    * must stay harmless on a machine someone is working on.
    */
  val enabled: Boolean = sys.env.get("RUNNER_GPU_TESTS").exists(_.nonEmpty)

  def run(body: => Unit): Unit = {
    if (enabled) {
      body
    } else {
      println("  skipped: set RUNNER_GPU_TESTS=1 to run on the GPU")
    }
  }
}
