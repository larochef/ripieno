package ripieno.backend.sound

import java.nio.{ByteBuffer, ByteOrder}

/** A WAV file's sound: its channels, at its rate. */
final case class Wav(channels: Vector[Array[Float]], rate: Int) {

  /** Its channels averaged into one. */
  def mono: Array[Float] =
    if (channels.size == 1) channels.head
    else
      Array.tabulate(channels.head.length) { index =>
        var sum = 0.0f
        for (channel <- channels) sum += channel(index)
        sum / channels.size
      }
}

object Wav {

  private val pcm = 1
  private val float = 3
  private val extensible = 0xfffe

  /** Reads 16, 24 or 32 bit integer and 32 bit float WAV files, as libsndfile
    * does: an integer sample over its full scale (32768 for 16 bits).
    */
  def read(bytes: Array[Byte]): Wav = {
    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    var offset = 12
    var format = 0
    var channels = 0
    var rate = 0
    var bits = 0
    var samples = Vector.empty[Array[Float]]
    while (offset + 8 <= bytes.length) {
      val id = new String(bytes, offset, 4, "ISO-8859-1")
      val size = buffer.getInt(offset + 4)
      val body = offset + 8
      if (id == "fmt ") {
        format = buffer.getShort(body) & 0xffff
        channels = buffer.getShort(body + 2).toInt
        rate = buffer.getInt(body + 4)
        bits = buffer.getShort(body + 14).toInt
        if (format == extensible) format = buffer.getShort(body + 24) & 0xffff
      } else if (id == "data") {
        val width = bits / 8
        val frames = math.min(size, bytes.length - body) / (width * channels)
        samples = Vector.tabulate(channels) { channel =>
          Array.tabulate(frames) { frame =>
            sample(
              buffer,
              body + (frame * channels + channel) * width,
              format,
              bits
            )
          }
        }
      }
      offset = body + size + (size & 1)
    }
    require(format == pcm || format == float, s"WAV format $format is not read")
    Wav(samples, rate)
  }

  private def sample(
      buffer: ByteBuffer,
      at: Int,
      format: Int,
      bits: Int
  ): Float =
    if (format == float) buffer.getFloat(at)
    else
      bits match {
        case 16 => buffer.getShort(at) / 32768.0f
        case 24 =>
          val value =
            (buffer.get(at) & 0xff) | ((buffer.get(at + 1) & 0xff) << 8) |
              (buffer.get(at + 2) << 16)
          value / 8388608.0f
        case 32 => (buffer.getInt(at) / 2147483648.0).toFloat
        case _  => throw IllegalArgumentException(s"$bits bit WAV is not read")
      }

  /** 16 bits, as libsndfile writes floats: times 32767, rounded, clipped. */
  def pcm16(channels: Vector[Array[Float]], rate: Int): Array[Byte] = {
    val frames = channels.map(_.length).maxOption.getOrElse(0)
    val data = frames * channels.size * 2
    val buffer = ByteBuffer.allocate(44 + data).order(ByteOrder.LITTLE_ENDIAN)
    buffer.put("RIFF".getBytes("ISO-8859-1")).putInt(36 + data)
    buffer.put("WAVEfmt ".getBytes("ISO-8859-1")).putInt(16)
    buffer
      .putShort(pcm.toShort)
      .putShort(channels.size.toShort)
      .putInt(rate)
      .putInt(rate * channels.size * 2)
    buffer.putShort((channels.size * 2).toShort).putShort(16.toShort)
    buffer.put("data".getBytes("ISO-8859-1")).putInt(data)
    for {
      frame <- 0 until frames
      channel <- channels
    } {
      val value = if (frame < channel.length) channel(frame) else 0.0f
      val scaled = math.rint(value * 32767.0).toLong
      buffer.putShort(math.max(-32768L, math.min(32767L, scaled)).toShort)
    }
    buffer.array
  }
}
