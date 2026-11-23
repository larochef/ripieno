package ripieno.runner.models

import ripieno.runner.models.MidiDdspLayers.*
import ripieno.runner.weights.WeightSource

/** MIDI-DDSP's expression generator: how each note of a part is played, its six
  * controls (volume, volume's fluctuation, vibrato, brightness, attack, where
  * the volume peaks), a note at a time from the one before, each in [0, 1]. The
  * notes read both ways first (a bidirectional GRU over their pitch, length and
  * the instrument); then two GRUs, a normalisation and a stack of dense layers
  * give a note's controls, a rest's zeroed.
  */
final class MidiDdspExpression(weights: WeightSource) {

  private val controls = 6
  private val pitchEmbedding = variable(weights, "pitch_emb/embeddings")
  private val durationEmbedding = Dense(weights, "duration_emb")
  private val instrumentEmbedding =
    variable(weights, "instrument_emb/embeddings")
  private val width = durationEmbedding.out
  private val first = Gru(weights, "rnn1/cell")
  private val second = Gru(weights, "rnn2/cell")
  private val normScale = variable(weights, "norm/scale")
  private val normShift = variable(weights, "norm/shift")
  private val out = new FcStackOut(weights, "dense_out/dense_out", 2)

  /** Each note's controls before they are kept in [0, 1], `[notes][6]`. */
  def raw(sequence: MidiDdspSequence, instrument: Int): Array[Array[Float]] = {
    val seconds = sequence.seconds
    val instrumentRow =
      instrumentEmbedding.slice(instrument * width, (instrument + 1) * width)
    val notes = sequence.pitches.indices.map { i =>
      pitchEmbedding.slice(
        sequence.pitches(i) * width,
        (sequence.pitches(i) + 1) * width
      ) ++
        durationEmbedding.row(Array(seconds(i))) ++ instrumentRow
    }.toArray
    val read = bidirectional(weights, "birnn", notes)
    var previous = new Array[Float](controls)
    var state1 = new Array[Float](first.units)
    var state2 = new Array[Float](second.units)
    read.indices.map { i =>
      state1 = first.step(
        denseRow(
          read(i) ++ previous,
          first.kernel,
          first.inputBias,
          first.in,
          3 * first.units
        ),
        state1
      )
      state2 = second.step(
        denseRow(
          state1,
          second.kernel,
          second.inputBias,
          second.in,
          3 * second.units
        ),
        state2
      )
      val normed = ddspNormalize(Array(state2), normScale, normShift).head
      val played = out.row(normed)
      // a rest plays nothing; what is fed back is not clipped
      previous =
        if (sequence.pitches(i) == 0) new Array[Float](controls) else played
      previous
    }.toArray
  }

  /** Each note's controls, in [0, 1]. */
  def controlsOf(
      sequence: MidiDdspSequence,
      instrument: Int
  ): Array[Array[Float]] =
    raw(sequence, instrument).map(
      _.map(value => math.max(0f, math.min(1f, value)))
    )
}
