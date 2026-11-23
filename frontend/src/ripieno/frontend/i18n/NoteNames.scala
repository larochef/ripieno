package ripieno.frontend.i18n

/** Notes as the player names them: C D E in English, do ré mi in French. The
  * octave of middle C is 4 in English and 3 in French.
  */
object NoteNames {
  private val englishSteps = Vector("C", "D", "E", "F", "G", "A", "B")
  private val frenchSteps = Vector("do", "ré", "mi", "fa", "sol", "la", "si")

  // each semitone as a step and its alteration: the black keys are sharps,
  // except E♭ and B♭, as they are most often written
  private val chromatic: Vector[(Int, Int)] = Vector(
    0 -> 0,
    0 -> 1,
    1 -> 0,
    2 -> -1,
    2 -> 0,
    3 -> 0,
    3 -> 1,
    4 -> 0,
    4 -> 1,
    5 -> 0,
    6 -> -1,
    6 -> 0
  )

  /** A step written C to B, in the page's language. */
  def step(written: String): String = {
    val index = englishSteps.indexOf(written)
    Language.current match {
      case Language.English => written
      case Language.French  => frenchSteps.lift(index).getOrElse(written)
    }
  }

  private def accidental(alter: Int): String = alter match {
    case 1  => "♯"
    case -1 => "♭"
    case 2  => "𝄪"
    case -2 => "𝄫"
    case _  => ""
  }

  /** A note without its octave: F♯, fa♯. */
  def pitchClass(written: String, alter: Int): String =
    s"${step(written)}${accidental(alter)}"

  /** A note with its octave, written as in the score: F♯5, fa♯4. */
  def written(written: String, alter: Int, octave: Int): String =
    s"${pitchClass(written, alter)}${octave - octaveShift}"

  /** The note nearest a MIDI pitch, with its octave: A4, la3. */
  def ofMidi(midi: Double): String = {
    val pitch = math.round(midi).toInt
    s"${ofMidiClass(midi)}${pitch / 12 - 1 - octaveShift}"
  }

  /** The note nearest a MIDI pitch, without its octave: A, la. */
  def ofMidiClass(midi: Double): String = {
    val (index, alter) = chromatic(Math.floorMod(math.round(midi).toInt, 12))
    pitchClass(englishSteps(index), alter)
  }

  private def octaveShift: Int = Language.current match {
    case Language.English => 0
    case Language.French  => 1
  }
}
