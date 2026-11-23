package ripieno.frontend.model

/** A stretch of the score, from the start of its first note to the end of its
  * last, in milliseconds of the score at its written tempo, with the measures
  * it runs over.
  */
final case class ScoreRange(
    from: Double,
    to: Double,
    firstMeasure: String,
    lastMeasure: String
)
