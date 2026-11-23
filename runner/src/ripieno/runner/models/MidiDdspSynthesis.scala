package ripieno.runner.models

import ripieno.runner.models.MidiDdspLayers.*
import ripieno.runner.weights.WeightSource

import java.util.stream.IntStream

/** What MIDI-DDSP's synthesis generator makes of a part's notes and their
  * expression, a frame (4 ms) at a time: the pitch played (`f0Midi`, MIDI, and
  * `f0Hz`) and the synthesiser's raw controls, `[frames][1 + 60 + 65]`: the
  * loudness, the harmonics' distribution, the noise's bands.
  */
final case class MidiDdspParameters(
    conditioning: Array[Array[Float]],
    midi: Array[Array[Float]],
    f0Midi: Array[Float],
    f0Hz: Array[Float],
    controls: Array[Array[Float]]
)

/** MIDI-DDSP's synthesis generator (its "MIDI decoder"): each frame's
  * conditions (the expression, the pitch written, onsets, offsets, where in its
  * note it is) through a stack of dense layers, with the instrument; from them
  * the pitch played, a frame at a time from the one before, drawn among 201
  * steps of a hundredth of a semitone around the pitch written (`sample`, given
  * the logits and the frame); then, from all of them at once, the synthesiser's
  * controls through a stack of dilated convolutions.
  */
object MidiDdspSynthesis {

  // the steps drawn among the likeliest whose probabilities make this much
  private val topP = 0.95

  /** The steps a draw may give (`top_p_sample`): the likeliest, as many as
    * reach `topP` of the probability with the one that crosses it, ties with
    * the least likely kept.
    */
  def candidates(logits: Array[Float]): IndexedSeq[Int] = {
    val sorted = logits.indices.sortBy(i => -logits(i))
    val largest = logits(sorted.head)
    val exponentials = sorted.map(i => math.exp(logits(i) - largest))
    val total = exponentials.sum
    val before = exponentials.scanLeft(0.0)(_ + _).map(_ / total)
    val kept = sorted.indices.filter(rank => before(rank) < topP).map(sorted)
    val least = kept.map(logits).min
    logits.indices.filter(logits(_) >= least)
  }

  /** A step drawn among the candidates, by their probabilities, from `random`
    * in [0, 1).
    */
  def drawn(logits: Array[Float], random: Double): Int = {
    val kept = candidates(logits)
    val largest = kept.map(logits).max
    val weights = kept.map(i => math.exp(logits(i) - largest))
    val target = random * weights.sum
    val chosen = weights.scanLeft(0.0)(_ + _).drop(1).indexWhere(_ > target)
    kept(if (chosen < 0) kept.size - 1 else chosen)
  }
}

final class MidiDdspSynthesis(weights: WeightSource) {

  private val decoder = "midi_decoder/decoder"
  private val f0 = s"$decoder/midi_to_f0"
  private val harmonic = s"$decoder/midi_f0_to_harmonic"
  // past these many notes (rests too), the position in the note is not told
  private val regions = 100
  private val stackLayers = 20

  private val preconditioning =
    new FcStackOut(weights, "midi_decoder/z_preconditioning_stack", 5)
  private val instrumentEmbedding =
    variable(weights, "midi_decoder/instrument_emb/embeddings")
  private val instrumentWidth =
    (dimensions(weights, "midi_decoder/instrument_emb/embeddings")(1)).toInt
  private val first = Gru(weights, s"$f0/rnn1/cell")
  private val second = Gru(weights, s"$f0/rnn2/cell")
  private val f0NormScale = variable(weights, s"$f0/norm/scale")
  private val f0NormShift = variable(weights, s"$f0/norm/shift")
  private val f0Out = Dense(weights, s"$f0/dense_out")
  private val pitchEmbedding = Dense(weights, s"$decoder/q_pitch_emb")
  private val convIn = (
    variable(weights, s"$harmonic/net/conv_in/kernel"),
    variable(weights, s"$harmonic/net/conv_in/bias")
  )
  private val layers = (0 until stackLayers).map { i =>
    (
      variable(weights, s"$harmonic/net/layers/$i/layer_with_weights-0/kernel"),
      variable(weights, s"$harmonic/net/layers/$i/layer_with_weights-0/bias"),
      variable(weights, s"$harmonic/net/norms/$i/scale"),
      variable(weights, s"$harmonic/net/norms/$i/shift")
    )
  }
  private val normScale = variable(weights, s"$harmonic/norm/scale")
  private val normShift = variable(weights, s"$harmonic/norm/shift")
  private val controlsOut = Dense(weights, s"$harmonic/dense_out")

  def parameters(
      sequence: MidiDdspSequence,
      expression: Array[Array[Float]],
      instrument: Int,
      sample: (Array[Float], Int) => Int
  ): MidiDdspParameters = {
    val conditioning = conditioningOf(sequence, expression)
    val instrumentRow = instrumentEmbedding.slice(
      instrument * instrumentWidth,
      (instrument + 1) * instrumentWidth
    )
    val midi = preconditioning(conditioning).map(_ ++ instrumentRow)
    val pitch = sequence.pitchFrames
    val f0Midi = pitched(midi, pitch, sample)
    val f0Hz = f0Midi.map(m =>
      if (m == 0) 0f else (440 * math.pow(2, (m - 69) / 12.0)).toFloat
    )
    val z = midi.indices
      .map(t => midi(t) ++ pitchEmbedding.row(Array(f0Midi(t) / 127)))
      .toArray
    MidiDdspParameters(conditioning, midi, f0Midi, f0Hz, controlsOut(stack(z)))
  }

  /** Each frame's ten conditions: the six controls of its note, its written
    * pitch / 127, an onset, an offset, and where in its note it is.
    */
  def conditioningOf(
      sequence: MidiDdspSequence,
      expression: Array[Array[Float]]
  ): Array[Array[Float]] = {
    val controls =
      (0 until 6).map(c => sequence.framed(expression.map(_(c)).toVector))
    val pitch = sequence.pitchFrames
    val (onsets, offsets) = (sequence.onsets, sequence.offsets)
    val position = relativePosition(onsets, pitch)
    Array.tabulate(pitch.length) { t =>
      controls.map(_(t)).toArray ++ Array(
        pitch(t) / 127,
        onsets(t),
        offsets(t),
        position(t)
      )
    }
  }

  /** Where in its note each frame is, (1 / length … 1), 0 in rests and past the
    * hundredth note (each onset begins a note, a rest's too).
    */
  private def relativePosition(
      onsets: Array[Float],
      pitch: Array[Float]
  ): Array[Float] = {
    val region = onsets.indices
      .scanLeft(-1)((r, t) => if (t == 0 || onsets(t) != 0) r + 1 else r)
      .drop(1)
    val counted = new Array[Int](onsets.length)
    val counts = scala.collection.mutable.Map.empty[Int, Int]
    for (t <- onsets.indices if region(t) < regions && pitch(t) > 0) {
      counts(region(t)) = counts.getOrElse(region(t), 0) + 1
      counted(t) = counts(region(t))
    }
    Array.tabulate(onsets.length)(t =>
      if (counted(t) == 0) 0f else counted(t).toFloat / counts(region(t))
    )
  }

  /** The pitch played, a frame at a time: the frames read both ways, then each
    * one's step drawn from the one drawn before.
    */
  private def pitched(
      midi: Array[Array[Float]],
      pitch: Array[Float],
      sample: (Array[Float], Int) => Int
  ): Array[Float] = {
    val read = bidirectional(weights, s"$f0/birnn", midi)
    val width = read.head.length
    val units3 = 3 * first.units
    // the frames' share of the first layer's input, the step drawn before's
    // being a row of its kernel
    val fromFrames =
      dense(read, first.kernel.take(width * units3), first.inputBias)
    var state1 = new Array[Float](first.units)
    var state2 = new Array[Float](second.units)
    var previous = -1
    Array.tabulate(midi.length) { t =>
      val input = fromFrames(t).clone()
      if (previous >= 0) {
        val row = (width + previous) * units3
        for (j <- 0 until units3) input(j) += first.kernel(row + j)
      }
      state1 = first.step(input, state1)
      state2 = second.step(
        denseRow(state1, second.kernel, second.inputBias, second.in, units3),
        state2
      )
      val logits =
        f0Out.row(ddspNormalize(Array(state2), f0NormScale, f0NormShift).head)
      previous = sample(logits, t)
      ((previous / 100.0 - 1).toFloat) + pitch(t)
    }
  }

  /** The dilated convolutions' stack over the whole part, then its
    * normalisation: each layer adds to what it reads its normalised convolution
    * of it, through a ReLU.
    */
  private def stack(z: Array[Array[Float]]): Array[Array[Float]] = {
    var x = convolution(z, convIn._1, convIn._2, 1)
    for ((layer, i) <- layers.zipWithIndex) {
      val (kernel, bias, scale, shift) = layer
      val convolved = convolution(
        x.map(_.map(value => math.max(0f, value))),
        kernel,
        bias,
        1 << (i % 5)
      )
      val normed = ddspNormalize(convolved, scale, shift)
      x = x.indices
        .map(t => Array.tabulate(x(t).length)(c => x(t)(c) + normed(t)(c)))
        .toArray
    }
    ddspNormalize(x, normScale, normShift)
  }

  /** A convolution of three taps `dilation` frames apart over the frames, as
    * Keras' "same" one: zeros beyond both ends. `kernel [3, 1, in, out]`.
    */
  private def convolution(
      x: Array[Array[Float]],
      kernel: Array[Float],
      bias: Array[Float],
      dilation: Int
  ): Array[Array[Float]] = {
    val out = bias.length
    val in = x.head.length
    val result = new Array[Array[Float]](x.length)
    IntStream.range(0, x.length).parallel().forEach { t =>
      val sums = bias.clone()
      for (tap <- 0 until 3) {
        val source = t + (tap - 1) * dilation
        if (source >= 0 && source < x.length) {
          val row = x(source)
          var i = 0
          while (i < in) {
            val value = row(i)
            if (value != 0) {
              val offset = (tap * in + i) * out
              var j = 0
              while (j < out) {
                sums(j) += value * kernel(offset + j)
                j += 1
              }
            }
            i += 1
          }
        }
      }
      result(t) = sums
    }
    result
  }
}
