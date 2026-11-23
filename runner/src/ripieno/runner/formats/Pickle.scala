package ripieno.runner.formats

import ripieno.runner.tensor.LittleEndian.*

import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_BYTE
import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** A value of a Python pickle, as `Pickle` reads it: what the pickle builds,
  * never what Python would run to build it. A class or function is only its
  * name (`Global`); calling it is only recorded (`Call`, `Instance`), for the
  * reader of the file to recognise what it knows.
  */
sealed trait Pickled

object Pickled {
  case object Empty extends Pickled
  final case class Bool(value: Boolean) extends Pickled
  final case class Integer(value: BigInt) extends Pickled
  final case class Real(value: Double) extends Pickled
  final case class Text(value: String) extends Pickled

  /** Bytes of the pickle, where they are in the file: never copied. */
  final case class Bytes(offset: Long, length: Long) extends Pickled
  final case class Tuple(items: Vector[Pickled]) extends Pickled
  final class ListOf(val items: mutable.ArrayBuffer[Pickled]) extends Pickled
  final class SetOf(val items: mutable.ArrayBuffer[Pickled]) extends Pickled

  /** A dictionary, an `OrderedDict` too, in its order. */
  final class DictOf(val entries: mutable.LinkedHashMap[Pickled, Pickled])
      extends Pickled {
    def get(key: String): Option[Pickled] = entries.get(Text(key))
  }

  final case class Global(module: String, name: String) extends Pickled {
    override def toString: String = s"$module.$name"
  }

  /** `function(*arguments)`, not run. */
  final class Call(val function: Global, val arguments: Vector[Pickled])
      extends Pickled

  /** An object of `type`, with what `__setstate__` would have been given. */
  final class Instance(val kind: Global) extends Pickled {
    var state: Pickled = Empty
  }

  /** What the file's reader made of a persistent id. */
  final case class Resolved(value: Any) extends Pickled
}

/** Python's pickles, protocols 2 to 5, the opcodes the binary protocols write,
  * read without running anything: a class, a function and what they would be
  * called with stay names and values (`Pickled`). A persistent id (`BINPERSID`)
  * is handed to `persistent`.
  */
object Pickle {
  import Pickled.*

  /** The value of the pickle at `start` in `segment`, and where it ends. */
  def read(
      segment: MemorySegment,
      start: Long,
      source: String,
      persistent: Pickled => Pickled = id =>
        throw new FormatException(s"a persistent id: $id")
  ): (Pickled, Long) = {
    def fail(message: String) =
      throw new FormatException(s"$source at $start: $message")
    var at = start
    val stack = mutable.ArrayBuffer.empty[Pickled]
    // where each MARK left the stack
    val marks = mutable.ArrayBuffer.empty[Int]
    val memo = mutable.Map.empty[Long, Pickled]

    def byte(): Int = {
      val value = uint8(segment, at)
      at += 1
      value
    }
    def text(length: Long): String = {
      val value = new String(
        segment.asSlice(at, length).toArray(JAVA_BYTE),
        StandardCharsets.UTF_8
      )
      at += length
      value
    }
    def line(): String = {
      var end = at
      while (segment.get(JAVA_BYTE, end) != '\n') end += 1
      val value = text(end - at)
      at += 1
      value
    }
    def u32(): Long = { val value = uint32(segment, at); at += 4; value }
    def u64(): Long = { val value = int64(segment, at); at += 8; value }
    def bytes(length: Long): Bytes = {
      val value = Bytes(at, length); at += length; value
    }
    // a little-endian two's complement integer of `length` bytes
    def long(length: Int): Integer = {
      val value = segment.asSlice(at, length).toArray(JAVA_BYTE).reverse
      at += length
      Integer(if (value.isEmpty) BigInt(0) else BigInt(value))
    }
    def pop(): Pickled = stack.remove(stack.length - 1)
    def top: Pickled = stack.last
    // what the last MARK left on the stack, taken off with it
    def marked(): Vector[Pickled] = {
      val from = marks.remove(marks.length - 1)
      val items = stack.drop(from).toVector
      stack.dropRightInPlace(stack.length - from)
      items
    }
    def tuple(count: Int): Tuple = {
      val items = stack.takeRight(count).toVector
      stack.dropRightInPlace(count)
      Tuple(items)
    }
    def setItems(target: Pickled, items: Seq[Pickled]): Unit = target match {
      case dict: DictOf =>
        items.grouped(2).foreach(pair => dict.entries(pair(0)) = pair(1))
      case other => fail(s"items set on $other")
    }
    def appended(target: Pickled, items: Seq[Pickled]): Unit = target match {
      case list: ListOf => list.items ++= items
      case set: SetOf   => set.items ++= items
      case other        => fail(s"items appended to $other")
    }
    def called(function: Pickled, arguments: Pickled): Pickled =
      (function, arguments) match {
        case (Global("collections", "OrderedDict"), Tuple(Vector())) =>
          new DictOf(mutable.LinkedHashMap.empty)
        case (global: Global, Tuple(items)) => new Call(global, items)
        case _ => fail(s"$function called with $arguments")
      }

    var result = Option.empty[Pickled]
    while (result.isEmpty) {
      byte() match {
        case 0x80 => byte() // PROTO
        case 0x95 => at += 8 // FRAME: frames are only a reading hint
        case '.'  => result = Some(pop()) // STOP
        case '('  => marks += stack.length // MARK
        case 0x94 => memo(memo.size.toLong) = top // MEMOIZE
        case 'q'  => memo(byte().toLong) = top // BINPUT
        case 'r'  => memo(u32()) = top // LONG_BINPUT
        case 'h'  => stack += memo(byte().toLong) // BINGET
        case 'j'  => stack += memo(u32()) // LONG_BINGET
        case 'N'  => stack += Empty
        case 0x88 => stack += Bool(true)
        case 0x89 => stack += Bool(false)
        case 'J'  => stack += Integer(BigInt(u32().toInt)) // BININT
        case 'K'  => stack += Integer(BigInt(byte())) // BININT1
        case 'M'  => // BININT2
          stack += Integer(BigInt(uint16(segment, at)))
          at += 2
        case 0x8a => stack += long(byte()) // LONG1
        case 0x8b => stack += long(u32().toInt) // LONG4
        case 'G'  => // BINFLOAT, big-endian
          stack += Real(
            java.lang.Double.longBitsToDouble(
              java.lang.Long.reverseBytes(u64())
            )
          )
        case 0x8c => stack += Text(text(byte().toLong)) // SHORT_BINUNICODE
        case 'X'  => stack += Text(text(u32())) // BINUNICODE
        case 0x8d => stack += Text(text(u64())) // BINUNICODE8
        case 'C'  => stack += bytes(byte().toLong) // SHORT_BINBYTES
        case 'B'  => stack += bytes(u32()) // BINBYTES
        case 0x8e => stack += bytes(u64()) // BINBYTES8
        case ')'  => stack += Tuple(Vector.empty) // EMPTY_TUPLE
        case 't'  => stack += Tuple(marked()) // TUPLE
        case 0x85 => stack += tuple(1)
        case 0x86 => stack += tuple(2)
        case 0x87 => stack += tuple(3)
        case ']' => stack += new ListOf(mutable.ArrayBuffer.empty) // EMPTY_LIST
        case 'a' => // APPEND
          val item = pop()
          appended(top, Seq(item))
        case 'e' => // APPENDS
          val items = marked()
          appended(top, items)
        case '}' =>
          stack += new DictOf(mutable.LinkedHashMap.empty) // EMPTY_DICT
        case 's' => // SETITEM
          val value = pop()
          val key = pop()
          setItems(top, Seq(key, value))
        case 'u' => // SETITEMS
          val items = marked()
          setItems(top, items)
        case 0x8f => stack += new SetOf(mutable.ArrayBuffer.empty) // EMPTY_SET
        case 0x90 => // ADDITEMS
          val items = marked()
          appended(top, items)
        case 'c' => // GLOBAL
          val module = line()
          stack += Global(module, line())
        case 0x93 => // STACK_GLOBAL
          (pop(), pop()) match {
            case (Text(name), Text(module)) => stack += Global(module, name)
            case other                      => fail(s"a global named by $other")
          }
        case 'R' => // REDUCE
          val arguments = pop()
          stack += called(pop(), arguments)
        case 0x81 => // NEWOBJ
          val arguments = pop()
          (pop(), arguments) match {
            case (kind: Global, Tuple(Vector())) => stack += new Instance(kind)
            case (kind, _) => fail(s"$kind made with $arguments")
          }
        case 'b' => // BUILD
          val state = pop()
          top match {
            case instance: Instance => instance.state = state
            // an OrderedDict's attributes, and a tensor's: not kept
            case _: DictOf | _: Call => ()
            case other               => fail(s"$other given a state")
          }
        case 'Q'  => stack += persistent(pop()) // BINPERSID
        case code => fail(f"opcode 0x$code%02x")
      }
    }
    (result.get, at)
  }
}
