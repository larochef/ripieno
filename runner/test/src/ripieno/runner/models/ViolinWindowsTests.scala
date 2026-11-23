package ripieno.runner.models

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.HexFormat

import utest.*

/** A line's windows against its reference implementation's (`violet/<case>/`:
  * each line's notes as note_seq read them, and `windows.json` where each
  * window started and the SHA-256 of its two rolls, as little-endian floats).
  */
object ViolinWindowsTests extends TestSuite {

  private def folder(name: String): Path =
    Path.of(getClass.getResource(s"/violet/$name").toURI)

  private def digest(values: Array[Float]): String = {
    val bytes =
      ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(bytes.putFloat)
    HexFormat
      .of()
      .formatHex(MessageDigest.getInstance("SHA-256").digest(bytes.array))
  }

  private def check(name: String): Unit = {
    val expected =
      ujson.read(Files.readString(folder(name).resolve("windows.json"))).obj
    for ((line, wanted) <- expected) {
      val notes = ViolinNote.read(folder(name).resolve(s"$line.notes"))
      assert(
        (line, ViolinWindows.totalTime(notes)) == (
          line,
          wanted("total_time").num
        )
      )
      assert(
        (line, ViolinWindows.totalFrames(notes)) == (
          line,
          wanted("total_frames").num.toInt
        )
      )
      val windows = ViolinWindows.windows(notes)
      assert(
        (line, windows.map(_.startFrame)) == (
          line,
          wanted("start_frames").arr.map(_.num.toInt).toVector
        )
      )
      for ((window, index) <- windows.zipWithIndex) {
        assert(
          (line, index, "notes", digest(window.notes)) == (
            line,
            index,
            "notes",
            wanted("midi_rolls")(index).str
          )
        )
        assert(
          (line, index, "techniques", digest(window.techniques)) ==
            (line, index, "techniques", wanted("tech_rolls")(index).str)
        )
      }
    }
  }

  val tests: Tests = Tests {
    test("the dynamics: each note's velocity, held to the next") {
      val notes = Vector(
        ViolinNote(36, 100, 0.99, 1.0),
        ViolinNote(67, 55, 1.0, 1.5),
        ViolinNote(69, 94, 2.0, 2.2)
      )
      val curve = ViolinWindows.dynamics(notes, 0.0)
      def at(seconds: Double) = curve((seconds * ViolinWindows.FrameRate).toInt)
      assert(
        curve.length == ViolinWindows.WindowFrames,
        at(0.0) == 55f / 127,
        at(1.8) == 55f / 127,
        at(2.0) == 94f / 127,
        at(9.0) == 94f / 127
      )
    }
    test("the windows of the synthetic lines") { check("synthetic") }
    test("the windows of a sonata's lines") { check("clerambault") }
  }
}
