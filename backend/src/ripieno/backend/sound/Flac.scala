package ripieno.backend.sound

import java.security.MessageDigest
import java.util.{Arrays, HexFormat}

/** A FLAC file decoded: its channels' samples, its rate and its bits a sample.
  */
final case class Flac(channels: Vector[Array[Int]], rate: Int, bits: Int) {

  /** The samples as libsndfile reads them into floats: over their full scale.
    */
  def floats: Vector[Array[Float]] = {
    val scale = (1L << (bits - 1)).toFloat
    channels.map(_.map(_ / scale))
  }

  /** The MD5 of the samples, interleaved, little-endian, as raw PCM is. */
  def digest: String = {
    val md5 = MessageDigest.getInstance("MD5")
    val width = (bits + 7) / 8
    val frame = new Array[Byte](width * channels.size)
    for (index <- channels.head.indices) {
      for {
        (channel, rank) <- channels.zipWithIndex
        byte <- 0 until width
      } {
        frame(rank * width + byte) = (channel(index) >> (8 * byte)).toByte
      }
      md5.update(frame)
    }
    HexFormat.of().formatHex(md5.digest())
  }
}

/** Decodes FLAC (the piano samples are kept in it): the stream's frames, each a
  * subframe a channel, constant, verbatim or predicted (fixed or linear
  * prediction) with Rice-coded residuals, the channels decorrelated.
  */
object Flac {

  final private class Bits(bytes: Array[Byte], var position: Long) {

    def bit(): Int = {
      val value =
        (bytes((position >> 3).toInt) >> (7 - (position & 7).toInt)) & 1
      position += 1
      value
    }

    def unsigned(count: Int): Long = {
      var value = 0L
      var left = count
      // whole bytes at once when aligned
      while (left >= 8 && (position & 7) == 0) {
        value = (value << 8) | (bytes((position >> 3).toInt) & 0xff)
        position += 8
        left -= 8
      }
      while (left > 0) {
        value = (value << 1) | bit()
        left -= 1
      }
      value
    }

    def signed(count: Int): Long =
      if (count == 0) 0L
      else {
        val value = unsigned(count)
        if ((value >> (count - 1)) == 1) value - (1L << count) else value
      }

    def unary(): Int = {
      var zeros = 0
      while (bit() == 0) zeros += 1
      zeros
    }

    def align(): Unit = position = (position + 7) & ~7L

    def byteOffset: Int = (position >> 3).toInt
  }

  def decode(bytes: Array[Byte]): Flac = {
    require(new String(bytes, 0, 4, "ISO-8859-1") == "fLaC", "not a FLAC file")
    val bits = Bits(bytes, 32)
    var last = false
    var rate = 0
    var channelCount = 0
    var sampleBits = 0
    var total = 0L
    while (!last) {
      last = bits.bit() == 1
      val kind = bits.unsigned(7).toInt
      val length = bits.unsigned(24).toInt
      val start = bits.byteOffset
      if (kind == 0) {
        bits.unsigned(16 + 16 + 24 + 24)
        rate = bits.unsigned(20).toInt
        channelCount = bits.unsigned(3).toInt + 1
        sampleBits = bits.unsigned(5).toInt + 1
        total = bits.unsigned(36)
      }
      bits.position = (start + length).toLong * 8
    }
    val channels = Vector.fill(channelCount)(new Array[Int](total.toInt))
    var written = 0
    while (written < total && bits.byteOffset + 2 < bytes.length) {
      written += frame(bits, channels, written, sampleBits)
    }
    Flac(channels, rate, sampleBits)
  }

  // one frame into the channels from `at`: how many samples it held
  private def frame(
      bits: Bits,
      channels: Vector[Array[Int]],
      at: Int,
      streamBits: Int
  ): Int = {
    require(bits.unsigned(14) == 0x3ffe, "lost the FLAC frames' sync")
    bits.unsigned(2)
    val sizeCode = bits.unsigned(4).toInt
    val rateCode = bits.unsigned(4).toInt
    val assignment = bits.unsigned(4).toInt
    val bitsCode = bits.unsigned(3).toInt
    bits.unsigned(1)
    // the frame's number, UTF-8 coded
    val first = bits.unsigned(8).toInt
    val following = Integer.numberOfLeadingZeros(~(first << 24)) - 1
    if (following > 0) bits.unsigned(8 * following)
    val size = sizeCode match {
      case 1                 => 192
      case code if code <= 5 => 576 << (code - 2)
      case 6                 => bits.unsigned(8).toInt + 1
      case 7                 => bits.unsigned(16).toInt + 1
      case code              => 256 << (code - 8)
    }
    rateCode match {
      case 12      => bits.unsigned(8)
      case 13 | 14 => bits.unsigned(16)
      case _       => 0L
    }
    bits.unsigned(8)
    val sampleBits = bitsCode match {
      case 1 => 8
      case 2 => 12
      case 4 => 16
      case 5 => 20
      case 6 => 24
      case 7 => 32
      case _ => streamBits
    }
    val decoded = channels.indices.map { channel =>
      // a side channel has a bit more
      val side = (assignment == 8 && channel == 1) ||
        (assignment == 9 && channel == 0) || (assignment == 10 && channel == 1)
      subframe(bits, size, sampleBits + (if (side) 1 else 0))
    }
    bits.align()
    bits.unsigned(16)
    val count = math.min(size, channels.head.length - at)
    for (index <- 0 until count) {
      assignment match {
        case 8 =>
          channels(0)(at + index) = decoded(0)(index).toInt
          channels(1)(at + index) =
            (decoded(0)(index) - decoded(1)(index)).toInt
        case 9 =>
          channels(0)(at + index) =
            (decoded(0)(index) + decoded(1)(index)).toInt
          channels(1)(at + index) = decoded(1)(index).toInt
        case 10 =>
          val side = decoded(1)(index)
          val mid = (decoded(0)(index) << 1) | (side & 1)
          channels(0)(at + index) = ((mid + side) >> 1).toInt
          channels(1)(at + index) = ((mid - side) >> 1).toInt
        case _ =>
          for (channel <- channels.indices) {
            channels(channel)(at + index) = decoded(channel)(index).toInt
          }
      }
    }
    size
  }

  private def subframe(bits: Bits, size: Int, sampleBits: Int): Array[Long] = {
    bits.unsigned(1)
    val kind = bits.unsigned(6).toInt
    val wasted = if (bits.bit() == 1) bits.unary() + 1 else 0
    val width = sampleBits - wasted
    val samples = new Array[Long](size)
    if (kind == 0) {
      Arrays.fill(samples, bits.signed(width))
    } else if (kind == 1) {
      for (index <- 0 until size) samples(index) = bits.signed(width)
    } else if (kind >= 8 && kind <= 12) {
      val order = kind - 8
      for (index <- 0 until order) samples(index) = bits.signed(width)
      residual(bits, samples, order)
      fixed(samples, order)
    } else if (kind >= 32) {
      val order = kind - 31
      for (index <- 0 until order) samples(index) = bits.signed(width)
      val precision = bits.unsigned(4).toInt + 1
      val shift = bits.signed(5).toInt
      val coefficients = Array.fill(order)(bits.signed(precision))
      residual(bits, samples, order)
      for (index <- order until size) {
        var sum = 0L
        for (j <- 0 until order) sum += coefficients(j) * samples(index - 1 - j)
        samples(index) += sum >> shift
      }
    } else {
      throw IllegalArgumentException(s"FLAC subframe type $kind is reserved")
    }
    if (wasted > 0) for (index <- 0 until size) samples(index) <<= wasted
    samples
  }

  // the residuals, added onto the samples after the predictor's warm-up
  private def residual(bits: Bits, samples: Array[Long], order: Int): Unit = {
    val method = bits.unsigned(2).toInt
    val parameterBits = if (method == 0) 4 else 5
    val escape = (1 << parameterBits) - 1
    val partitionOrder = bits.unsigned(4).toInt
    val partitions = 1 << partitionOrder
    var index = order
    for (partition <- 0 until partitions) {
      val count =
        (samples.length >> partitionOrder) - (if (partition == 0) order else 0)
      val parameter = bits.unsigned(parameterBits).toInt
      if (parameter == escape) {
        val width = bits.unsigned(5).toInt
        for (_ <- 0 until count) {
          samples(index) = bits.signed(width)
          index += 1
        }
      } else {
        for (_ <- 0 until count) {
          val value =
            (bits.unary().toLong << parameter) | bits.unsigned(parameter)
          samples(index) = (value >>> 1) ^ -(value & 1)
          index += 1
        }
      }
    }
  }

  private def fixed(samples: Array[Long], order: Int): Unit =
    for (index <- order until samples.length) {
      def s(back: Int) = samples(index - back)
      samples(index) += (order match {
        case 0 => 0L
        case 1 => s(1)
        case 2 => 2 * s(1) - s(2)
        case 3 => 3 * s(1) - 3 * s(2) + s(3)
        case _ => 4 * s(1) - 6 * s(2) + 4 * s(3) - s(4)
      })
    }
}
