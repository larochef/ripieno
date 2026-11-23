package ripieno.runner.formats

import ripieno.runner.formats.Pickled.*
import ripieno.runner.tensor.*
import ripieno.runner.tensor.LittleEndian.*

import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_BYTE
import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** A PyTorch checkpoint's tensors, read as `torch.save` writes them, without
  * running its pickles (`Pickle`): its tensors by the names `state_dict()`
  * gives them, their bytes where the file holds them.
  *
  * Three layouts are read: the zip archive (`<name>/data.pkl`, each storage
  * `<name>/data/<key>`, stored uncompressed); the legacy stream (a pickle of
  * the object, then each storage's element count and bytes); and a module
  * pickled whole, each storage a legacy stream of its own inside the pickle
  * (`torch.storage._load_from_bytes`). What the file holds is a state dict (or
  * a dictionary with one under `state_dict`), or a module, whose parameters and
  * persistent buffers are named as `state_dict()` names them. A tensor must be
  * contiguous; its storage may sit at any byte.
  */
final class TorchCheckpoint(
    val tensors: Map[String, StoredTensor],
    val unsupported: Map[String, String]
) extends TensorSource

object TorchCheckpoint {

  private val legacyMagic = BigInt("119547037146038801333356")

  // the storages' types, and the bytes of an element of each
  private val storageTypes: Map[String, DType] = Map(
    "FloatStorage" -> DType.F32,
    "HalfStorage" -> DType.F16,
    "BFloat16Storage" -> DType.BF16,
    "LongStorage" -> DType.I64,
    "IntStorage" -> DType.I32,
    "CharStorage" -> DType.I8,
    "ByteStorage" -> DType.U8
  )
  private val elementBytes: Map[String, Int] = Map(
    "FloatStorage" -> 4,
    "HalfStorage" -> 2,
    "BFloat16Storage" -> 2,
    "LongStorage" -> 8,
    "IntStorage" -> 4,
    "CharStorage" -> 1,
    "ByteStorage" -> 1,
    "BoolStorage" -> 1,
    "ShortStorage" -> 2,
    "DoubleStorage" -> 8,
    "ComplexFloatStorage" -> 8,
    "ComplexDoubleStorage" -> 16
  )

  /** A storage: its type, its elements, where its bytes start in the file. */
  final private class Storage(val kind: String, val count: Long) {
    var offset: Long = -1
  }

  def read(segment: MemorySegment, source: String): TorchCheckpoint = {
    def fail(message: String) = throw new FormatException(s"$source: $message")
    if (segment.byteSize() < 4) fail("too short for a PyTorch checkpoint")
    val root =
      if (uint32(segment, 0) == 0x04034b50L) fromZip(segment, source)
      else if (uint8(segment, 0) == 0x80) {
        val (first, end) = Pickle.read(segment, 0, source)
        first match {
          case Integer(`legacyMagic`) => legacy(segment, 0, source)
          case _                      =>
            if (end != segment.byteSize())
              fail("a pickle followed by more than itself")
            Pickle
              .read(segment, 0, source, storageId(_, source, (_, _) => ()))
              ._1
        }
      } else fail("not a PyTorch checkpoint: neither a zip nor a pickle")
    val described = named(root, "").map((name, value) =>
      name -> tensorOf(name, value, segment, source)
    )
    new TorchCheckpoint(
      described.collect { case (name, Right(tensor)) => name -> tensor }.toMap,
      described.collect { case (name, Left(reason)) => name -> reason }.toMap
    )
  }

  /** The object of a zip archive, its storages its entries. */
  private def fromZip(segment: MemorySegment, source: String): Pickled = {
    def fail(message: String) = throw new FormatException(s"$source: $message")
    val entries = zipEntries(segment, source)
    val pickle = entries.keys
      .find(name => name == "data.pkl" || name.endsWith("/data.pkl"))
      .getOrElse(fail("a zip without a data.pkl"))
    val folder = pickle.stripSuffix("data.pkl")
    Pickle
      .read(
        segment,
        entries(pickle)._1,
        source,
        storageId(
          _,
          source,
          (key, storage) => {
            val (offset, size) = entries.getOrElse(
              s"${folder}data/$key",
              fail(s"no entry for storage $key")
            )
            if (size < storage.count * elementBytes(storage.kind))
              fail(s"storage $key is shorter than its elements")
            storage.offset = offset
          }
        )
      )
      ._1
  }

  /** The stored entries of a zip: where each one's bytes start, and how many.
    */
  private def zipEntries(
      segment: MemorySegment,
      source: String
  ): Map[String, (Long, Long)] = {
    def fail(message: String) = throw new FormatException(s"$source: $message")
    val size = segment.byteSize()
    // the end of the central directory, before a comment of at most 64 KiB
    val end = (size - 22 to math.max(0, size - 22 - 65535) by -1)
      .find(at => uint32(segment, at) == 0x06054b50L)
      .getOrElse(fail("a zip without its central directory"))
    val count = uint16(segment, end + 10)
    val directory = uint32(segment, end + 16)
    if (directory == 0xffffffffL || count == 0xffff)
      fail("a zip64 archive, not read")
    var at = directory
    (0 until count).map { _ =>
      if (uint32(segment, at) != 0x02014b50L) fail("a broken central directory")
      val method = uint16(segment, at + 10)
      val stored = uint32(segment, at + 20)
      val nameLength = uint16(segment, at + 28)
      val local = uint32(segment, at + 42)
      val name = new String(
        segment.asSlice(at + 46, nameLength).toArray(JAVA_BYTE),
        StandardCharsets.UTF_8
      )
      at += 46 + nameLength + uint16(segment, at + 30) + uint16(
        segment,
        at + 32
      )
      if (method != 0) fail(s"$name is compressed: PyTorch stores its entries")
      if (uint32(segment, local) != 0x04034b50L) fail(s"$name: no local header")
      val data =
        local + 30 + uint16(segment, local + 26) + uint16(segment, local + 28)
      name -> (data, stored)
    }.toMap
  }

  /** The object of the legacy stream at `start`: the magic number, the
    * protocol, the system's description, the object, its storages' keys, then
    * each storage, its element count before its bytes.
    */
  private def legacy(
      segment: MemorySegment,
      start: Long,
      source: String
  ): Pickled = {
    def fail(message: String) =
      throw new FormatException(s"$source at $start: $message")
    val (_, afterMagic) = Pickle.read(segment, start, source)
    val (_, afterProtocol) = Pickle.read(segment, afterMagic, source)
    val (system, afterSystem) = Pickle.read(segment, afterProtocol, source)
    system match {
      case info: DictOf if info.get("little_endian").contains(Bool(true)) => ()
      case _ => fail("not written by a little-endian machine")
    }
    val storages = mutable.Map.empty[String, Storage]
    val (root, afterRoot) = Pickle.read(
      segment,
      afterSystem,
      source,
      storageId(_, source, (key, storage) => storages(key) = storage)
    )
    val (keys, afterKeys) = Pickle.read(segment, afterRoot, source)
    var at = afterKeys
    keys match {
      case list: ListOf =>
        for (key <- list.items) {
          val storage = key match {
            case Text(name) =>
              storages.getOrElse(name, fail(s"storage $name is not used"))
            case other => fail(s"a storage key $other")
          }
          val count = int64(segment, at)
          if (count != storage.count) fail(s"storage $key: $count elements")
          storage.offset = at + 8
          at += 8 + count * elementBytes(storage.kind)
        }
      case other => fail(s"storage keys $other")
    }
    if (at > segment.byteSize()) fail("storages past the end of the file")
    root
  }

  /** A storage's persistent id, `("storage", type, key, location, count, ...)`,
    * as the storage: `found` places its bytes.
    */
  private def storageId(
      id: Pickled,
      source: String,
      found: (String, Storage) => Unit
  ): Pickled = id match {
    case Tuple(
          Text("storage") +: Global("torch", kind) +: Text(key) +: _ +:
          Integer(count) +: _
        ) if elementBytes.contains(kind) =>
      val storage = new Storage(kind, count.toLong)
      found(key, storage)
      Resolved(storage)
    case other => throw new FormatException(s"$source: a storage $other")
  }

  /** The tensors of the object, by the names `state_dict()` gives them. */
  private def named(value: Pickled, prefix: String): Seq[(String, Pickled)] =
    value match {
      case dict: DictOf =>
        dict.get("state_dict") match {
          case Some(inner: DictOf) if prefix.isEmpty => named(inner, "")
          case _                                     =>
            dict.entries.toSeq.flatMap {
              case (Text(key), tensor: Call) if isTensor(tensor) =>
                Seq(s"$prefix$key" -> tensor)
              case (Text(key), inner: (DictOf | Instance)) =>
                named(inner, s"$prefix$key.")
              case _ => Seq.empty
            }
        }
      case module: Instance =>
        module.state match {
          case state: DictOf if state.get("_modules").nonEmpty =>
            val transient = state.get("_non_persistent_buffers_set") match {
              case Some(set: SetOf) =>
                set.items.collect { case Text(name) => name }.toSet
              case _ => Set.empty[String]
            }
            def entries(field: String): Seq[(String, Pickled)] =
              state.get(field) match {
                case Some(dict: DictOf) =>
                  dict.entries.toSeq.collect {
                    case (Text(name), value) if value != Empty => name -> value
                  }
                case _ => Seq.empty
              }
            entries("_parameters").map((name, tensor) =>
              s"$prefix$name" -> tensor
            ) ++
              entries("_buffers")
                .filterNot((name, _) => transient.contains(name))
                .map((name, tensor) => s"$prefix$name" -> tensor) ++
              entries("_modules").flatMap((name, inner) =>
                named(inner, s"$prefix$name.")
              )
          case _ => Seq.empty
        }
      case _ => Seq.empty
    }

  private def isTensor(value: Call): Boolean =
    value.function == Global("torch._utils", "_rebuild_tensor_v2") ||
      value.function == Global("torch._utils", "_rebuild_parameter")

  /** A tensor of the object as stored: its storage, its offset in it, its shape
    * and strides.
    */
  private def tensorOf(
      name: String,
      value: Pickled,
      segment: MemorySegment,
      source: String
  ): Either[String, StoredTensor] = value match {
    case call: Call
        if call.function == Global("torch._utils", "_rebuild_parameter") =>
      tensorOf(name, call.arguments.head, segment, source)
    case call: Call
        if call.function == Global("torch._utils", "_rebuild_tensor_v2") =>
      call.arguments match {
        case stored +: Integer(offset) +: Tuple(sizes) +: Tuple(strides) +: _ =>
          val storage = storageOf(stored, segment, source)
          val shape = sizes.collect { case Integer(size) => size.toLong }
          val steps = strides.collect { case Integer(step) => step.toLong }
          val contiguous = shape.indices.forall(i =>
            shape(i) == 1 || steps(i) == shape.drop(i + 1).product
          )
          storageTypes.get(storage.kind) match {
            case None                   => Left(s"a ${storage.kind}")
            case Some(_) if !contiguous => Left("not contiguous")
            case Some(dtype)            =>
              val tensor = StoredTensor(
                name,
                dtype,
                Shape(shape),
                storage.offset + dtype.byteSize(offset.toLong),
                segment
              )
              if (offset.toLong + tensor.shape.elementCount > storage.count)
                throw new FormatException(
                  s"$source: $name runs past its storage"
                )
              Right(tensor)
          }
        case other =>
          throw new FormatException(s"$source: $name rebuilt from $other")
      }
    case other => throw new FormatException(s"$source: $name is $other")
  }

  private def storageOf(
      value: Pickled,
      segment: MemorySegment,
      source: String
  ): Storage = value match {
    case Resolved(storage: Storage) => storage
    // a storage pickled whole: a legacy stream of its own
    case call: Call
        if call.function == Global("torch.storage", "_load_from_bytes") =>
      call.arguments match {
        case Vector(Bytes(offset, _)) =>
          storageOf(legacy(segment, offset, source), segment, source)
        case other =>
          throw new FormatException(s"$source: a storage from $other")
      }
    case other => throw new FormatException(s"$source: a storage $other")
  }
}
