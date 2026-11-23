package ripieno.backend.sound

import utest.*

/** The program a part MIDI-DDSP plays is played with. */
object MidiDdspRenderingTests extends TestSuite {

  val tests: Tests = Tests {
    test(
      "a part MIDI-DDSP does not model goes to the highest string that reaches it"
    ) {
      def instrument(program: Int, lowest: Int) =
        MidiInstrument(
          program,
          false,
          "",
          Vector(MidiNote(80, lowest, 0, 1), MidiNote(80, lowest + 12, 1, 2))
        )
      assert(MidiDdspRendering.programOf(instrument(0, 60)) == 40)
      assert(MidiDdspRendering.programOf(instrument(26, 50)) == 41)
      assert(MidiDdspRendering.programOf(instrument(19, 40)) == 42)
      assert(MidiDdspRendering.programOf(instrument(33, 28)) == 43)
      assert(MidiDdspRendering.programOf(instrument(71, 50)) == 71)
    }
  }
}
