package ripieno.backend.sound.violet

import ripieno.backend.sound.*

import utest.*

/** The glue around VIOLET against what the Python it replaces made of the same
  * files (`rendering/`: a page's MIDI and a synthetic one, `<case>.json` what
  * pretty_midi, which VIOLET reads its files with, read of the input, of each
  * line split and of each line bowed; the synthetic case's mix of the tones
  * `stem` makes): each file it writes is read again as the engines read it, and
  * must hold the same notes at the same ticks. Since then, the mix
  * (`synthetic-mix.wav`) measures each note by its loudest moment: it must stay
  * the same within a step of 16 bits.
  */
object VioletGlueTests extends TestSuite {

  private def resource(name: String): Array[Byte] = {
    val stream = getClass.getResourceAsStream(s"/rendering/$name")
    assert(stream != null)
    try stream.readAllBytes()
    finally stream.close()
  }

  private def golden(name: String): ujson.Value =
    ujson.read(resource(s"$name.json"))

  // a file as the goldens describe it
  private def described(file: MidiFile): ujson.Value = ujson.Obj(
    "instruments" -> file.instruments.map(instrument =>
      ujson.Obj(
        "program" -> instrument.program,
        "drum" -> instrument.isDrum,
        "name" -> instrument.name,
        "notes" -> instrument.notes.map(note =>
          ujson.Arr(note.pitch, note.velocity, note.start, note.end)
        )
      )
    ),
    "texts" -> file.texts.map(text => ujson.Arr(text.text, text.time))
  )

  private def readAgain(file: MidiFile): MidiFile = MidiFile.read(file.bytes)

  // the first difference between two descriptions, where it is
  private def same(
      found: ujson.Value,
      expected: ujson.Value,
      where: String
  ): Unit =
    (found, expected) match {
      case (ujson.Arr(one), ujson.Arr(other)) =>
        for (((item, wanted), index) <- one.zip(other).zipWithIndex) {
          same(item, wanted, s"$where[$index]")
        }
        assert((where, one.size) == (where, other.size))
      case (ujson.Obj(one), ujson.Obj(other)) =>
        assert((where, one.keySet) == (where, other.keySet))
        for (key <- one.keys) same(one(key), other(key), s"$where.$key")
      case _ => assert((where, found) == (where, expected))
    }

  private def split(name: String) =
    VioletLines.split(MidiFile.read(resource(s"$name.mid")))

  private def checkSplit(name: String): Unit = {
    val expected = golden(name)
    val lines = split(name)
    same(
      ujson.Obj.from(
        lines.lines.map(line => line.name -> described(readAgain(line.file)))
      ),
      expected("split"),
      "split"
    )
    same(
      lines.others.fold(ujson.Null)(others => described(readAgain(others))),
      expected("others"),
      "others"
    )
  }

  private def bowedLines(name: String): Vector[(String, MidiFile)] =
    split(name).lines.map(line =>
      line.name -> readAgain(VioletBowing.bowed(readAgain(line.file), "bow"))
    )

  private def checkBowing(name: String): Unit =
    same(
      ujson.Obj.from(
        bowedLines(name).map((line, file) => line -> described(file))
      ),
      golden(name)("bowed"),
      "bowed"
    )

  // the stems the synthetic case was mixed from
  private def stem(index: Int, seconds: Double): Array[Float] =
    Array.tabulate((seconds * VioletMix.rate).toInt) { sample =>
      val n = sample.toDouble
      val tone =
        0.2 * math.sin(2 * math.Pi * 110 * (index + 2) * n / VioletMix.rate)
      (tone * (1 + 0.5 * math.sin(
        2 * math.Pi * 1.5 * n / VioletMix.rate
      ))).toFloat
    }

  val tests: Tests = Tests {
    test("the engines' reading of a page's MIDI") {
      for (name <- Seq("synthetic", "clerambault")) {
        same(
          described(MidiFile.read(resource(s"$name.mid"))),
          golden(name)("input"),
          name
        )
      }
    }
    test("lines and sections, synthetic") { checkSplit("synthetic") }
    test("lines and sections, Clérambault") { checkSplit("clerambault") }
    test("bowing, synthetic") { checkBowing("synthetic") }
    test("bowing, Clérambault") { checkBowing("clerambault") }
    test("mix") {
      val seconds = golden("synthetic")("mix_seconds").num
      val lines = bowedLines("synthetic").sortBy((name, _) => s"$name.mid")
      val stems = lines.zipWithIndex.map { case ((name, file), index) =>
        VioletMix.Stem(name, stem(index, seconds), file)
      }
      val mixedSound = VioletMix.mixed(0.08, stems, Some(stem(9, seconds)), 2)
      val found = Wav.read(
        Wav.pcm16(Vector(mixedSound), VioletMix.rate)
      )
      val expected = Wav.read(resource("synthetic-mix.wav"))
      assert(found.mono.length == expected.mono.length)
      val furthest = found.mono
        .zip(expected.mono)
        .map((one, other) => math.abs(one - other) * 32768)
        .max
      assert(furthest <= 2)
    }
  }
}
