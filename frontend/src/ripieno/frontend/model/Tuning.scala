package ripieno.frontend.model

/** The open strings of the instruments the tuner knows, low to high, in MIDI:
  * an instrument is named by the library's key for it.
  */
object Tuning {
  val openStrings: List[(String, List[Int])] = List(
    "violin" -> List(55, 62, 69, 76),
    "viola" -> List(48, 55, 62, 69),
    "cello" -> List(36, 43, 50, 57),
    "bass" -> List(28, 33, 38, 43),
    "guitar" -> List(40, 45, 50, 55, 59, 64),
    "mandolin" -> List(55, 62, 69, 76)
  )

  /** What is sounded for an instrument without strings: its A. */
  val reference: List[Int] = List(69)

  def of(instrument: String): Option[List[Int]] =
    openStrings.collectFirst {
      case (kind, strings) if kind == instrument => strings
    }
}
