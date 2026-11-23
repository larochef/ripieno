package ripieno.frontend.model

/** Which parts a PDF shows, and how. */
enum ExportLayout {

  /** Every part, as the score prints them. */
  case Full

  /** That part only, counted in score order. */
  case Alone(part: Int)

  /** Every part, that one at full size and the others smaller, as an
    * accompanist reads them over the part played.
    */
  case Accompanying(part: Int)
}
