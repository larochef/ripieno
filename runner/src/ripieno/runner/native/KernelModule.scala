package ripieno.runner.native

import java.lang.foreign.{Arena, MemorySegment}
import java.nio.file.Files

/** One code object compiled from `runner/kernels/<name>.hip` for this machine
  * (`KernelCache`).
  */
final class KernelModule(hip: HipRuntime, val name: String)
    extends AutoCloseable {

  private val arena = Arena.ofShared()

  private val module = {
    val bytes = Files.readAllBytes(
      KernelCache.folder(hip.rocmRoot).resolve(s"$name.hsaco")
    )
    val image = arena.allocate(bytes.length.toLong, 64)
    MemorySegment.copy(MemorySegment.ofArray(bytes), 0, image, 0, bytes.length)
    hip.loadModule(image)
  }

  def function(functionName: String): KernelFunction =
    hip.function(module, functionName)

  def close(): Unit = {
    hip.unloadModule(module)
    arena.close()
  }
}
