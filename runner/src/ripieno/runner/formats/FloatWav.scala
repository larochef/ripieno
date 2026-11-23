package ripieno.runner.formats

import java.nio.{ByteBuffer, ByteOrder}

/** A one-channel WAV file of floats, as the engines' sounds are written. */
object FloatWav {

  def bytes(samples: Array[Float], rate: Int): Array[Byte] = {
    val data = samples.length * 4
    val buffer = ByteBuffer.allocate(44 + data).order(ByteOrder.LITTLE_ENDIAN)
    buffer.put("RIFF".getBytes("ISO-8859-1")).putInt(36 + data)
    buffer.put("WAVEfmt ".getBytes("ISO-8859-1")).putInt(16)
    buffer.putShort(3.toShort).putShort(1.toShort).putInt(rate).putInt(rate * 4)
    buffer.putShort(4.toShort).putShort(32.toShort)
    buffer.put("data".getBytes("ISO-8859-1")).putInt(data)
    samples.foreach(buffer.putFloat)
    buffer.array
  }
}
