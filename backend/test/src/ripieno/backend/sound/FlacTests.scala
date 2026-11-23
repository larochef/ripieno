package ripieno.backend.sound

import ripieno.backend.models.{Download, Downloaded}

import java.nio.file.Files

import utest.*

/** FLAC decoded as the reference decoder does: each of the piano's samples to
  * the MD5 `flac -d` gives of its raw samples (`flac-md5.json`), downloaded
  * first when they are not there (`Downloaded`).
  */
object FlacTests extends TestSuite {

  val tests: Tests = Tests {
    test("the piano's samples decode as the reference decoder does") {
      val samples = Downloaded.folder(Download.Piano).resolve("Samples")
      val stream = getClass.getResourceAsStream("/flac-md5.json")
      val expected =
        try ujson.read(stream.readAllBytes()).obj.view.mapValues(_.str).toMap
        finally stream.close()
      val wrong = expected.keys.toList.sorted.filter { name =>
        Flac
          .decode(Files.readAllBytes(samples.resolve(name)))
          .digest != expected(name)
      }
      assert(expected.size == 641, wrong.isEmpty)
    }
  }
}
