package ripieno.runner.ops

/** A weights file as a backend maps it; `close` unmaps it. */
final class MappedWeights(
    val segment: java.lang.foreign.MemorySegment,
    val storage: ripieno.runner.tensor.Storage,
    closing: () => Unit
) extends AutoCloseable {
  def close(): Unit = closing()
}
