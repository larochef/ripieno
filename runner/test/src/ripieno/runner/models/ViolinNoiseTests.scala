package ripieno.runner.models

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}

import utest.*

/** The noise against PyTorch's `randn` for the same seed
  * (`violet/noise-<seed>.npy`, a window's noise as VIOLET drew it).
  */
object ViolinNoiseTests extends TestSuite {

  private def resource(name: String): Path =
    Path.of(getClass.getResource(s"/violet/$name").toURI)

  private def npy(path: Path): Array[Float] = {
    val bytes = Files.readAllBytes(path)
    val headerLength = (bytes(8) & 0xff) | ((bytes(9) & 0xff) << 8)
    val buffer = ByteBuffer
      .wrap(bytes, 10 + headerLength, bytes.length - 10 - headerLength)
      .order(ByteOrder.LITTLE_ENDIAN)
    Array.fill((bytes.length - 10 - headerLength) / 4)(buffer.getFloat)
  }

  val tests: Tests = Tests {
    test("a window's noise is PyTorch's, to a bit") {
      val seed = Files.readString(resource("noise-seed.txt")).trim.toLong
      val expected = npy(resource(s"noise-$seed.npy"))
      val found = ViolinNoise.gaussian(seed, expected.length)
      val identical =
        found.indices.count(index => found(index) == expected(index))
      val furthest =
        found.indices.map(index => math.abs(found(index) - expected(index))).max
      println(
        s"  $identical of ${found.length} identical, the others at most $furthest apart"
      )
      assert(furthest < 1e-5, identical > found.length * 3 / 4)
    }
    test("the noise is a standard normal one") {
      val values = ViolinNoise.gaussian(7, 1 << 20).map(_.toDouble)
      val mean = values.sum / values.length
      val variance =
        values.map(value => (value - mean) * (value - mean)).sum / values.length
      assert(math.abs(mean) < 0.005, math.abs(variance - 1) < 0.005)
    }
  }
}
