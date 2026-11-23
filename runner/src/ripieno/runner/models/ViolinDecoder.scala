package ripieno.runner.models

import ripieno.runner.ops.{Activation, Ops}
import ripieno.runner.tensor.*
import ripieno.runner.weights.WeightSource

/** VIOLET's audio decoder (a DAC decoder fine-tuned on the violin), which turns
  * latents, 25 frames a second, into 48 kHz audio. It has two halves.
  *
  * The first makes a signal: a wide convolution, then four stages that each
  * stretch time (by 12, 10, 8 and 2) and halve the channels, through Snake
  * activations and dilated residual units.
  *
  * The second was built to hide a 16-bit message in that signal: causal
  * convolutions take it down to 150 frames a second, two LSTMs and the
  * message's embedding work on it there, and the mirror image brings it back to
  * one channel at 48 kHz, added to the signal at a quarter of its level. With
  * these weights it has stopped carrying anything: its output is the same value
  * on every sample but the very first ones (−0.207 on the reference window),
  * and what it does is cancel the offset the first half's signal sits at. Past
  * that constant, the audio and the signal differ by 52 dB less than the
  * signal.
  */
final class ViolinDecoder(ops: Ops, weights: WeightSource) {
  import ViolinDecoder.*

  private def allocate(rows: Long, channels: Long): Tensor =
    ops.allocate(DType.F32, Shape.of(rows, channels))

  private def channels(layer: String): Long =
    weights.shape(s"$layer.bias").elementCount

  // A convolution that keeps the length: as many zeros before as after.
  private def convolution(
      x: Tensor,
      layer: String,
      taps: Int,
      dilation: Int = 1
  ): Tensor = {
    val out = allocate(x.shape.dimensions.head, channels(layer))
    val padding = dilation * (taps - 1) / 2
    ops.conv1d(
      x,
      weights(s"$layer.weight"),
      Some(weights(s"$layer.bias")),
      taps,
      dilation,
      1,
      padding,
      padding,
      out
    )
    out
  }

  private def snake(x: Tensor, layer: String): Tensor = {
    val out = allocate(x.shape.dimensions.head, x.shape.last)
    ops.snake(x, weights(s"$layer.alpha"), out)
    out
  }

  // A transposed convolution of `2 × stride` taps: every frame times the
  // kernel, the products overlapped and added `stride` apart.
  private def stretched(x: Tensor, layer: String, stride: Int): Tensor = {
    val frames = x.shape.dimensions.head
    val taps = 2 * stride
    val columns = allocate(frames, channels(layer) * taps)
    try {
      ops.conv1d(x, weights(s"$layer.weight"), None, 1, 1, 1, 0, 0, columns)
      val out = allocate(frames * stride, channels(layer))
      ops.overlapAdd(
        columns,
        taps,
        stride,
        stride / 2,
        weights(s"$layer.bias"),
        out
      )
      out
    } finally {
      ops.release(columns)
    }
  }

  /** Runs `step` on `x` and releases `x`: a chain of layers holds two tensors
    * at most.
    */
  private def through(x: Tensor)(step: Tensor => Tensor): Tensor = {
    try {
      step(x)
    } finally {
      ops.release(x)
    }
  }

  private def residualUnit(x: Tensor, unit: String, dilation: Int): Tensor = {
    val activated = snake(x, s"$unit.snake1")
    val wide =
      through(activated)(convolution(_, s"$unit.conv1", UnitTaps, dilation))
    val again = through(wide)(snake(_, s"$unit.snake2"))
    val narrow = through(again)(convolution(_, s"$unit.conv2", 1))
    ops.add(x, narrow, narrow)
    narrow
  }

  private def stage(x: Tensor, index: Int): Tensor = {
    val block = s"blocks.$index"
    val activated = snake(x, s"$block.snake")
    val longer = through(activated)(stretched(_, s"$block.up", Strides(index)))
    UnitDilations.zipWithIndex.foldLeft(longer) {
      case (signal, (dilation, unit)) =>
        through(signal)(residualUnit(_, s"$block.units.$unit", dilation))
    }
  }

  /** The decoder's stages on `latents` (`[frames, 128]`), each stage's output
    * handed to `observe` before the next one releases it: `[frames × 1920, 96]`
    * at the end, which the caller releases.
    */
  def stages(
      latents: Tensor,
      observe: (String, Tensor) => Unit = (_, _) => ()
  ): Tensor = {
    val projected = convolution(latents, "out_proj", 1)
    observe("projected", projected)
    val first = through(projected)(convolution(_, "first", FirstTaps))
    observe("first", first)
    Strides.indices.foldLeft(first) { (signal, index) =>
      val next = through(signal)(stage(_, index))
      observe(s"block$index", next)
      next
    }
  }

  private def activated(x: Tensor, kind: Activation): Tensor = {
    val out = allocate(x.shape.dimensions.head, x.shape.last)
    ops.activation(kind, x, out)
    out
  }

  /** The signal of `latents`, one channel at 48 kHz, which the caller releases.
    */
  def signal(latents: Tensor): Tensor = {
    val shaped = through(stages(latents))(snake(_, "tail.snake"))
    val mono = through(shaped)(convolution(_, "tail.conv", FirstTaps))
    through(mono)(activated(_, Activation.Tanh))
  }

  // A convolution that only reads the past: its zeros all come before.
  private def causal(
      x: Tensor,
      layer: String,
      taps: Int,
      stride: Int = 1
  ): Tensor = {
    val out = allocate(x.shape.dimensions.head / stride, channels(layer))
    ops.conv1d(
      x,
      weights(s"$layer.weight"),
      Some(weights(s"$layer.bias")),
      taps,
      1,
      stride,
      taps - stride,
      0,
      out
    )
    out
  }

  // The causal counterpart of `stretched`: nothing is trimmed before the
  // first frame, and the kernel's overhang after the last one is dropped.
  private def causalStretched(x: Tensor, layer: String, stride: Int): Tensor = {
    val frames = x.shape.dimensions.head
    val taps = 2 * stride
    val columns = allocate(frames, channels(layer) * taps)
    try {
      ops.conv1d(x, weights(s"$layer.weight"), None, 1, 1, 1, 0, 0, columns)
      val overhanging = allocate((frames + 1) * stride, channels(layer))
      try {
        ops.overlapAdd(
          columns,
          taps,
          stride,
          0,
          weights(s"$layer.bias"),
          overhanging
        )
        val out = allocate(frames * stride, channels(layer))
        ops.copy(overhanging.rows(0, frames * stride), out)
        out
      } finally {
        ops.release(overhanging)
      }
    } finally {
      ops.release(columns)
    }
  }

  private def markUnit(x: Tensor, unit: String): Tensor = {
    val first = activated(x, Activation.Elu)
    val narrow = through(first)(causal(_, s"$unit.conv1", MarkUnitTaps))
    val second = through(narrow)(activated(_, Activation.Elu))
    val wide = through(second)(causal(_, s"$unit.conv2", 1))
    ops.add(x, wide, wide)
    wide
  }

  /** Two LSTM layers over the frames of `x` (`[frames, width]`), added to `x`.
    * Each layer's products with the input are one launch for every frame; what
    * depends on the previous frame is two launches a frame.
    */
  private def recurrent(x: Tensor, lstm: String): Tensor = {
    val Seq(frames, width) = x.shape.dimensions
    val recurrentSums = allocate(1, 4 * width)
    val cell = allocate(1, width)
    val start = allocate(1, width)
    try {
      val out = (0 until LstmLayers).foldLeft(x) { (input, layer) =>
        val inputSums = allocate(frames, 4 * width)
        val states = allocate(frames, width)
        try {
          ops.linear(input, weights(s"$lstm.$layer.input"), inputSums)
          ops.zero(cell)
          ops.zero(start)
          val bias = weights(s"$lstm.$layer.bias")
          val recurrentWeight = weights(s"$lstm.$layer.recurrent")
          (0L until frames).foreach { frame =>
            val previous = if (frame == 0) start else states.rows(frame - 1, 1)
            ops.linear(previous, recurrentWeight, recurrentSums)
            ops.lstmStep(
              inputSums.rows(frame, 1),
              recurrentSums,
              bias,
              cell,
              states.rows(frame, 1)
            )
          }
          states
        } finally {
          ops.release(inputSums)
          if (input ne x) {
            ops.release(input)
          }
        }
      }
      ops.add(out, x, out)
      out
    } finally {
      Seq(recurrentSums, cell, start).foreach(ops.release)
    }
  }

  // The message as the row added to every frame: the sum of one embedding per
  // bit, by the bit's value.
  private def messageRow(message: Seq[Boolean]): Tensor = {
    val width = weights.shape("mark.message").last.toInt
    val rows = message.zipWithIndex.map((bit, index) =>
      weights.hostRows("mark.message", 2L * index + (if (bit) 1 else 0), 1)
    )
    ops.fromFloats(Shape.of(width), rows.transpose.map(_.sum).toArray)
  }

  /** What the second half adds for `signal` and a message of `MessageBits`
    * bits, before it is scaled: one channel at 48 kHz, which the caller
    * releases. `observe` is handed its stages as `stages` does.
    */
  def mark(
      signal: Tensor,
      message: Seq[Boolean],
      observe: (String, Tensor) => Unit = (_, _) => ()
  ): Tensor = {
    require(
      message.size == MessageBits,
      s"a message of ${message.size} bits, not $MessageBits"
    )
    val entered = causal(signal, "mark.in", FirstTaps)
    observe("mark_in", entered)
    val low = MarkStrides.indices.reverse.foldLeft(entered) { (h, index) =>
      val block = s"blocks.$index.mark"
      val unit = through(h)(markUnit(_, s"$block.unit_down"))
      val shaped = through(unit)(activated(_, Activation.Elu))
      through(shaped)(
        causal(_, s"$block.down", 2 * MarkStrides(index), MarkStrides(index))
      )
    }
    observe("mark_low", low)
    val remembered = through(low)(recurrent(_, "mark.encoder.lstm"))
    val shaped = through(remembered)(activated(_, Activation.Elu))
    val encoded = through(shaped)(causal(_, "mark.encoder.out", FirstTaps))
    observe("mark_encoded", encoded)
    val row = messageRow(message)
    try {
      ops.addRow(encoded, row, encoded)
    } finally {
      ops.release(row)
    }
    val widened = through(encoded)(causal(_, "mark.decoder.in", FirstTaps))
    val decoded = through(widened)(recurrent(_, "mark.decoder.lstm"))
    observe("mark_decoded", decoded)
    val back = MarkStrides.indices.foldLeft(decoded) { (h, index) =>
      val block = s"blocks.$index.mark"
      val shaped = through(h)(activated(_, Activation.Elu))
      val longer =
        through(shaped)(causalStretched(_, s"$block.up", MarkStrides(index)))
      through(longer)(markUnit(_, s"$block.unit_up"))
    }
    val last = through(back)(activated(_, Activation.Elu))
    through(last)(causal(_, "mark.decoder.out", FirstTaps))
  }

  // the second half's steady value, once a window has shown it
  private var steadyMark = Option.empty[Float]

  /** The audio of `latents` as `audio` makes it, the second half run on the
    * first window only: past its first samples it gives one value whatever the
    * signal (see the class), which later windows are given instead. The second
    * half is most of the decoding's launches (its LSTMs go frame by frame).
    */
  def audioOfSteadyMark(
      latents: Tensor,
      message: Seq[Boolean]
  ): Array[Float] =
    steadyMark match {
      case Some(value) =>
        val first = signal(latents)
        try {
          ops.toFloats(first).map(_ + MarkLevel * value)
        } finally {
          ops.release(first)
        }
      case None =>
        val first = signal(latents)
        try {
          val second = mark(first, message)
          try {
            val marked = ops.toFloats(second)
            val settled = marked.drop(SettlingSamples).sorted
            if (settled.nonEmpty) {
              steadyMark = Some(settled(settled.length / 2))
            }
            ops.toFloats(first).zip(marked).map((x, m) => x + MarkLevel * m)
          } finally {
            ops.release(second)
          }
        } finally {
          ops.release(first)
        }
    }

  /** The second half's steady value, once found. */
  def steadyMarkValue: Option[Float] = steadyMark

  /** The audio of `latents` (`[frames, 128]`): 48,000 samples a second, on the
    * host.
    */
  def audio(latents: Tensor, message: Seq[Boolean]): Array[Float] = {
    val first = signal(latents)
    try {
      val second = mark(first, message)
      try {
        ops.scale(second, MarkLevel, second)
        ops.add(first, second, second)
        ops.toFloats(second)
      } finally {
        ops.release(second)
      }
    } finally {
      ops.release(first)
    }
  }
}

object ViolinDecoder {
  val Strides: Seq[Int] = Seq(12, 10, 8, 2)

  /** The bits of the message the decoder's second half carries. */
  val MessageBits = 16

  private val FirstTaps = 7
  private val UnitTaps = 7
  private val UnitDilations = Seq(1, 3, 9)

  // the second half's own strides, stage by stage: 320 in all, down to 150 frames a second
  private val MarkStrides = Seq(8, 5, 4, 2)
  private val MarkUnitTaps = 3
  private val MarkLevel = 0.25f
  // the second half's first samples, before it settles on its value
  private val SettlingSamples = 4800
  private val LstmLayers = 2
}
