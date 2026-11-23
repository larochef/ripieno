package ripieno.backend.review

import java.lang.foreign.*
import java.lang.foreign.ValueLayout.*
import scala.util.Using

/** Resampling by the system's libsoxr (`soxr_oneshot`, at its "high quality"),
  * which librosa resamples with between the octaves of a constant-Q spectrum:
  * the same library, the same samples.
  */
object Soxr {

  private val highQuality = 4L
  private val float32Interleaved = 0

  private val linker = Linker.nativeLinker()
  private val library =
    SymbolLookup.libraryLookup("libsoxr.so.0", Arena.global())
  private val qualityLayout = MemoryLayout.structLayout(
    JAVA_DOUBLE.withName("precision"),
    JAVA_DOUBLE.withName("phase_response"),
    JAVA_DOUBLE.withName("passband_end"),
    JAVA_DOUBLE.withName("stopband_begin"),
    ADDRESS.withName("e"),
    JAVA_LONG.withName("flags")
  )
  private val ioLayout = MemoryLayout.structLayout(
    JAVA_INT.withName("itype"),
    JAVA_INT.withName("otype"),
    JAVA_DOUBLE.withName("scale"),
    ADDRESS.withName("e"),
    JAVA_LONG.withName("flags")
  )
  private def function(name: String, descriptor: FunctionDescriptor) =
    linker.downcallHandle(
      library
        .find(name)
        .orElseThrow(() => IllegalStateException(s"libsoxr has no $name")),
      descriptor
    )
  private val qualitySpec = function(
    "soxr_quality_spec",
    FunctionDescriptor.of(qualityLayout, JAVA_LONG, JAVA_LONG)
  )
  private val ioSpec = function(
    "soxr_io_spec",
    FunctionDescriptor.of(ioLayout, JAVA_INT, JAVA_INT)
  )
  private val oneshot = function(
    "soxr_oneshot",
    FunctionDescriptor.of(
      ADDRESS,
      JAVA_DOUBLE,
      JAVA_DOUBLE,
      JAVA_INT,
      ADDRESS,
      JAVA_LONG,
      ADDRESS,
      ADDRESS,
      JAVA_LONG,
      ADDRESS,
      ADDRESS,
      ADDRESS,
      ADDRESS
    )
  )

  /** `signal` at a `factor`th of its rate, as librosa's `resample` gives it:
    * its length rounded up, its level kept (`scale=True`).
    */
  def downsampled(signal: Array[Float], factor: Int): Array[Float] =
    Using.resource(Arena.ofConfined()) { arena =>
      val length = math.ceil(signal.length / factor.toDouble).toInt
      val input = arena.allocateFrom(JAVA_FLOAT, signal*)
      val capacity = length + 64
      val output = arena.allocate(JAVA_FLOAT, capacity.toLong)
      val done = arena.allocate(JAVA_LONG)
      val quality = qualitySpec
        .invokeWithArguments(arena, highQuality, 0L)
        .asInstanceOf[MemorySegment]
      val io = ioSpec
        .invokeWithArguments(arena, float32Interleaved, float32Interleaved)
        .asInstanceOf[MemorySegment]
      val error = oneshot
        .invokeWithArguments(
          factor.toDouble,
          1.0,
          1,
          input,
          signal.length.toLong,
          MemorySegment.NULL,
          output,
          capacity.toLong,
          done,
          io,
          quality,
          MemorySegment.NULL
        )
        .asInstanceOf[MemorySegment]
      require(
        error.address == 0L,
        s"libsoxr failed: ${error.reinterpret(256).getString(0)}"
      )
      val produced = done.get(JAVA_LONG, 0).toInt
      val scale = math.sqrt(1.0 / factor)
      Array.tabulate(length)(index =>
        if (index < produced)
          (output.getAtIndex(JAVA_FLOAT, index.toLong) / scale).toFloat
        else 0.0f
      )
    }
}
