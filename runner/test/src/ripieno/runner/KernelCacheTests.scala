package ripieno.runner

import ripieno.backend.models.{Download, Downloaded}
import ripieno.runner.native.KernelCache

import java.nio.file.{Files, Path}

import utest.*

/** The kernels compile with the ROCm tree the server downloads (downloaded
  * first when it is not there, `Downloaded`), which needs no GPU: a kernel that
  * no longer compiles is found here, not at a rendering.
  */
object KernelCacheTests extends TestSuite {

  val tests: Tests = Tests {
    test("every kernel compiles with the ROCm tree") {
      val folder = KernelCache.folder(Downloaded.folder(Download.Rocm))
      val kernels = Files
        .readString(
          Path.of(getClass.getResource("/kernels/sources.txt").toURI)
        )
        .linesIterator
        .filter(_.endsWith(".hip"))
        .map(_.stripSuffix(".hip"))
        .toSeq
      assert(
        kernels.nonEmpty,
        kernels.forall(name => Files.size(folder.resolve(s"$name.hsaco")) > 0)
      )
      println(s"  ${kernels.size} kernels in $folder")
    }
  }
}
