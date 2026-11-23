package ripieno.backend.sound.salamander

import ripieno.backend.models.{Download, Downloaded}
import ripieno.backend.sound.*

import utest.*

/** The piano against what the Python it replaces made of a synthetic score
  * (`salamander/`: `piano.mid`; `piano.json` the other parts it left, as
  * pretty_midi read them, and how many notes it played; `piano.flac` a quarter
  * of the piano's sound, at 24 bits; `mix.flac` its mix with the tone `tone`
  * makes), the piano's samples downloaded first when they are not there.
  */
object SalamanderTests extends TestSuite {

  private def resource(name: String): Array[Byte] = {
    val stream = getClass.getResourceAsStream(s"/salamander/$name")
    assert(stream != null)
    try stream.readAllBytes()
    finally stream.close()
  }

  private lazy val golden = ujson.read(resource("piano.json"))

  private lazy val played = SalamanderPiano.played(
    MidiFile.read(resource("piano.mid")),
    Downloaded.folder(Download.Piano).resolve("Samples")
  )

  private def withSamples(check: SalamanderPiano.Played => Unit): Unit =
    check(played)

  // the furthest two sounds are apart, sample by sample
  private def furthest(
      found: Vector[Array[Float]],
      expected: Vector[Array[Float]]
  ): Double = {
    assert(found.map(_.length) == expected.map(_.length))
    found
      .zip(expected)
      .map((one, other) =>
        one.indices
          .map(index => math.abs(one(index) - other(index)).toDouble)
          .max
      )
      .max
  }

  private def tone(samples: Int): Array[Float] =
    Array.tabulate(samples)(n =>
      (0.3 * math.sin(
        2 * math.Pi * 330 * n.toDouble / SalamanderPiano.rate
      )).toFloat
    )

  val tests: Tests = Tests {
    test("the parts left to the other engine") {
      withSamples { played =>
        val resolution = MidiFile.read(resource("piano.mid")).timing.resolution
        val others = MidiFile.read(
          MidiFile(
            played.others,
            Vector.empty,
            MidiTiming(resolution, Vector(0 -> 60.0 / (120.0 * resolution)), 0)
          ).bytes
        )
        val described = others.instruments.map(instrument =>
          ujson.Obj(
            "program" -> instrument.program,
            "drum" -> instrument.isDrum,
            "name" -> instrument.name,
            "notes" -> instrument.notes.map(note =>
              ujson.Arr(note.pitch, note.velocity, note.start, note.end)
            ),
            "controls" -> instrument.controls.map(control =>
              ujson.Arr(control.number, control.value, control.time)
            )
          )
        )
        assert(ujson.Arr.from(described) == golden("others")("instruments"))
        assert(played.notes == golden("notes").num.toInt)
      }
    }
    test("the piano's sound") {
      withSamples { played =>
        val expected =
          Flac.decode(resource("piano.flac")).floats.map(_.map(_ * 4))
        val apart = furthest(played.sound, expected)
        println(s"furthest apart: $apart")
        assert(apart < 1e-4)
      }
    }
    test("the mix") {
      withSamples { played =>
        val other = tone(golden("tone_samples").num.toInt)
        val mix =
          SalamanderRendering.mixed(Vector(played.sound, Vector(other, other)))
        val found = Wav.read(Wav.pcm16(mix, SalamanderPiano.rate)).channels
        val expected = Flac.decode(resource("mix.flac")).floats
        val apart = furthest(found, expected) * 32768
        println(s"furthest apart: $apart steps")
        assert(apart <= 2)
      }
    }
  }
}
