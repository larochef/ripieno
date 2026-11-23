package ripieno.runner.formats

import ripieno.runner.tensor.*

import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_BYTE
import java.nio.charset.StandardCharsets
import java.util.Arrays

/** A TensorFlow checkpoint's tensors, read as TensorFlow writes them (a "tensor
  * bundle"): `<prefix>.index`, a LevelDB table whose keys are the tensors'
  * names and whose values say, in a `BundleEntryProto`, each tensor's type,
  * shape and where its bytes are in `<prefix>.data-00000-of-00001`, which holds
  * them raw, little-endian, end to end.
  *
  * Only what TensorFlow writes is read: uncompressed blocks, one data shard,
  * whole tensors (not slices). The names are TensorFlow's, the variables' paths
  * through the model's objects (`.../kernel/.ATTRIBUTES/VARIABLE_VALUE`);
  * optimiser slots and the object graph are left out.
  */
final class TensorflowCheckpoint(
    val tensors: Map[String, StoredTensor],
    val unsupported: Map[String, String]
) extends TensorSource

object TensorflowCheckpoint {

  private val tableMagic = 0xdb4775248b80fb57L
  private val footerBytes = 48
  private val blockTrailerBytes = 5

  /** The tensors of the checkpoint whose index is `index`, their bytes slices
    * of `data`, the data shard's mapping.
    */
  def read(
      index: MemorySegment,
      data: MemorySegment,
      source: String
  ): TensorflowCheckpoint = {
    def fail(message: String) = throw new FormatException(s"$source: $message")
    val bytes = index.toArray(JAVA_BYTE)
    if (bytes.length < footerBytes) fail("too short for a checkpoint index")
    val footer = Reader(bytes, bytes.length - footerBytes)
    // the metaindex block, unused
    footer.varint()
    footer.varint()
    val (indexOffset, indexSize) = (footer.varint(), footer.varint())
    val magic = (0 until 8).foldLeft(0L)((sum, i) =>
      sum | ((bytes(bytes.length - 8 + i) & 0xffL) << (8 * i))
    )
    if (magic != tableMagic) fail("not a LevelDB table")
    val entries = for {
      (_, handle) <- block(bytes, indexOffset.toInt, indexSize.toInt, fail)
      dataHandle = Reader(handle, 0)
      (key, value) <- block(
        bytes,
        dataHandle.varint().toInt,
        dataHandle.varint().toInt,
        fail
      )
    } yield new String(key, StandardCharsets.UTF_8) -> value
    val described = entries
      .filter((name, _) => name.nonEmpty && kept(name))
      .map((name, value) => name -> entry(value))
    val tensors = described.collect {
      case (name, Right((dtype, shape, offset))) =>
        name -> StoredTensor(name, dtype, shape, offset, data)
    }.toMap
    val unsupported = described.collect { case (name, Left(reason)) =>
      name -> reason
    }.toMap
    new TensorflowCheckpoint(tensors, unsupported)
  }

  // the model's variables: not the optimiser's slots, nor the object graph
  private def kept(name: String): Boolean =
    !name.contains(".OPTIMIZER_SLOT") && !name.startsWith(
      "_CHECKPOINTABLE_OBJECT_GRAPH"
    ) &&
      !name.contains("save_counter") && !name.startsWith("optimizer/")

  /** A table block's entries: each key shares a prefix with the one before. */
  private def block(
      bytes: Array[Byte],
      offset: Int,
      size: Int,
      fail: String => Nothing
  ): Vector[(Array[Byte], Array[Byte])] = {
    if (offset + size + blockTrailerBytes > bytes.length)
      fail("a block runs past the end of the index")
    if (bytes(offset + size) != 0)
      fail("a compressed block: TensorFlow writes none")
    val restarts = readFixed32(bytes, offset + size - 4)
    val end = offset + size - 4 - 4 * restarts
    val reader = Reader(bytes, offset)
    val entries = Vector.newBuilder[(Array[Byte], Array[Byte])]
    var key = Array.emptyByteArray
    while (reader.position < end) {
      val shared = reader.varint().toInt
      val unshared = reader.varint().toInt
      val valueLength = reader.varint().toInt
      key = key.take(shared) ++ reader.take(unshared)
      entries += key -> reader.take(valueLength)
    }
    entries.result()
  }

  private def readFixed32(bytes: Array[Byte], at: Int): Int =
    (0 until 4).foldLeft(0)((sum, i) =>
      sum | ((bytes(at + i) & 0xff) << (8 * i))
    )

  /** A `BundleEntryProto`: its tensor's type, shape and offset in the shard. */
  private def entry(
      value: Array[Byte]
  ): Either[String, (DType, Shape, Long)] = {
    val reader = Reader(value, 0)
    var dtype = 0L
    var dimensions = Vector.empty[Long]
    var shard = 0L
    var offset = 0L
    var slices = false
    while (reader.position < value.length) {
      val tag = reader.varint()
      ((tag >> 3).toInt, (tag & 7).toInt) match {
        case (1, 0) => dtype = reader.varint()
        case (2, 2) => dimensions = shape(reader.take(reader.varint().toInt))
        case (3, 0) => shard = reader.varint()
        case (4, 0) => offset = reader.varint()
        case (7, 2) =>
          slices = true
          reader.skip(2)
        case (_, wire) => reader.skip(wire)
      }
    }
    // TensorFlow's DataType: 1 float, 3 int32, 9 int64
    val types = Map(1L -> DType.F32, 3L -> DType.I32, 9L -> DType.I64)
    if (slices) Left("a sliced tensor")
    else if (shard != 0) Left(s"in shard $shard: one shard is read")
    else
      types
        .get(dtype)
        .toRight(s"TensorFlow type $dtype")
        .map(kind => (kind, Shape.of(dimensions*), offset))
  }

  /** A `TensorShapeProto`'s dimensions. */
  private def shape(bytes: Array[Byte]): Vector[Long] = {
    val reader = Reader(bytes, 0)
    val found = Vector.newBuilder[Long]
    while (reader.position < bytes.length) {
      val tag = reader.varint()
      ((tag >> 3).toInt, (tag & 7).toInt) match {
        case (2, 2) =>
          val dimension = Reader(reader.take(reader.varint().toInt), 0)
          var size = 0L
          while (dimension.position < dimension.length) {
            val field = dimension.varint()
            if ((field >> 3) == 1 && (field & 7) == 0) size = dimension.varint()
            else dimension.skip((field & 7).toInt)
          }
          found += size
        case (_, wire) => reader.skip(wire)
      }
    }
    found.result()
  }

  // protobuf's and LevelDB's varints and fields, from `start`
  final private class Reader(bytes: Array[Byte], start: Int) {
    var position: Int = start
    def length: Int = bytes.length

    def varint(): Long = {
      var value = 0L
      var shift = 0
      var more = true
      while (more) {
        val byte = bytes(position)
        value |= (byte & 0x7fL) << shift
        shift += 7
        more = (byte & 0x80) != 0
        position += 1
      }
      value
    }

    def take(count: Int): Array[Byte] = {
      val taken = Arrays.copyOfRange(bytes, position, position + count)
      position += count
      taken
    }

    def skip(wire: Int): Unit = wire match {
      case 0 => varint()
      case 1 => position += 8
      case 2 => position += varint().toInt
      case 5 => position += 4
      case _ => throw new FormatException(s"protobuf wire type $wire")
    }
  }
}
