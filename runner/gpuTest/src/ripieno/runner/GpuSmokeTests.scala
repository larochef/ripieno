package ripieno.runner

import ripieno.runner.ops.{HipOps, MatVecInputs}
import ripieno.runner.tensor.Shape

import utest.*

/** The least the GPU can be asked: the first thing to run on a machine, or
  * after anything went wrong with it.
  */
object GpuSmokeTests extends TestSuite {
  val tests = Tests {
    test("the device answers") {
      Gpu.run {
        val (free, total) = Gpu.hip.memoryInfo
        println(
          s"  ${Gpu.hip.deviceName} from ${Gpu.hip.rocmRoot}: ${free >> 20} of ${total >> 20} MiB free"
        )
        assert(total > 0)
      }
    }
    test("a thousand values come back from device memory") {
      Gpu.run {
        val hip = new HipOps(Gpu.hip, MatVecInputs.Int8)
        try {
          val values = TestData.gaussian(1, 1000)
          assert(
            hip
              .toFloats(hip.fromFloats(Shape.of(1000), values))
              .sameElements(values)
          )
        } finally {
          hip.close()
        }
      }
    }
  }
}
