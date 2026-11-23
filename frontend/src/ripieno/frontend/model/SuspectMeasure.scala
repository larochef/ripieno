package ripieno.frontend.model

/** A measure of a part that may have been read wrong, to be checked: `kind` is
  * "too-long", "too-short" or "tie", `detail` says what is odd, `notes` are the
  * ids of the notes concerned.
  */
final case class SuspectMeasure(
    part: Int,
    measure: String,
    kind: String,
    detail: String,
    notes: List[String]
) {

  /** The same suspicion, whatever the edits around it. */
  def key: String = s"$part|$measure|$kind"
}
