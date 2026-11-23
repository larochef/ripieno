package ripieno.runner.models

import java.lang.Float.intBitsToFloat

import utest.*

/** The noise levels against those VIOLET's sampler was given, bit for bit. */
object ViolinScheduleTests extends TestSuite {

  private val reference = Seq(0x3f800000, 0x3f772c23, 0x3f6e5847, 0x3f65846a,
    0x3f5cb08d, 0x3f53dcb1, 0x3f4b08d4, 0x3f4234f7, 0x3f39611a, 0x3f308d3e,
    0x3f27b961, 0x3f1ee584, 0x3f1611a8, 0x3f0d3dcb, 0x3f0469ee, 0x3ef72c23,
    0x3ee5846a, 0x3ed3dcb0, 0x3ec234f7, 0x3eb08d3e, 0x3e9ee584, 0x3e8d3dcb,
    0x3e772c23, 0x3e53dcb0, 0x3e308d3e, 0x3e0d3dcb, 0x3dd3dcb0, 0x3d8d3dcb,
    0x3d0d3dcb, 0x00000000).map(intBitsToFloat)

  val tests: Tests = Tests {
    test("the sampler's noise levels are VIOLET's") {
      assert(ViolinSchedule.sigmas().toSeq == reference)
    }
  }
}
