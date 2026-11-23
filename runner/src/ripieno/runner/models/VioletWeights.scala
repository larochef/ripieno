package ripieno.runner.models

import ripieno.runner.formats.*
import ripieno.runner.ops.Ops
import ripieno.runner.tensor.*
import ripieno.runner.weights.WeightSource

import java.lang.Float.floatToFloat16
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.{
  JAVA_FLOAT_UNALIGNED,
  JAVA_SHORT_UNALIGNED
}
import java.nio.ByteOrder
import java.nio.file.Path
import java.util.stream.IntStream

/** VIOLET's weights as its authors publish them (`User-tian/VIOLET` on Hugging
  * Face), as its models here read them.
  *
  * The network (`pretrained_checkpoint/ema_snapshots/ema_prof_99515`) is its
  * `DiT` pickled whole, nearly all in F16: its matrices are read as they are,
  * its vectors widened to F32. The audio decoder (`dacvae_ft/weights.pth`) is
  * the fine-tuned DAC-VAE's state dict, in F32: each convolution's
  * weight-normed halves folded into its weight as PyTorch folds them
  * (`weightNormed`) and laid out as `ViolinDecoder` reads it, its LSTMs'
  * matrices rounded to F16 and their two biases added.
  */
object VioletWeights {

  /** The network's file, in the published repository. */
  val NetworkFile = "pretrained_checkpoint/ema_snapshots/ema_prof_99515"

  /** The audio decoder's file, in the published repository. */
  val DecoderFile = "dacvae_ft/weights.pth"

  def network(ops: Ops, path: Path): WeightSource =
    fromPublished(ops, path, networkTensors)

  def decoder(ops: Ops, path: Path): WeightSource =
    fromPublished(ops, path, decoderTensors)

  private def fromPublished(
      ops: Ops,
      path: Path,
      tensors: TensorSource => Map[String, StoredTensor]
  ): WeightSource = {
    val mapped = new MappedFile(path)
    try
      WeightSource.onHost(
        ops,
        tensors(TorchCheckpoint.read(mapped.segment, path.toString)),
        () => mapped.close()
      )
    catch {
      case error: Throwable =>
        mapped.close()
        throw error
    }
  }

  /** The network's floating tensors: matrices in F16, vectors in F32. */
  def networkTensors(published: TensorSource): Map[String, StoredTensor] =
    published.tensors.collect {
      case (name, tensor) if floating(tensor.dtype) =>
        name -> {
          if (tensor.shape.rank < 2) made(name, tensor.shape, tensor.decode())
          else if (tensor.dtype == DType.F16) tensor
          else halves(name, tensor.shape, tensor.decode())
        }
    }

  private def floating(dtype: DType): Boolean =
    dtype == DType.F32 || dtype == DType.F16 || dtype == DType.BF16

  /** The decoder's tensors, by the names `ViolinDecoder` reads. */
  def decoderTensors(published: TensorSource): Map[String, StoredTensor] = {
    val tensors = Map.newBuilder[String, StoredTensor]
    def add(tensor: StoredTensor): Unit = tensors += tensor.name -> tensor
    def has(name: String): Boolean = published.tensors.contains(name)
    def values(name: String): Array[Float] = published(name).decode()
    def bias(name: String, path: String): Unit =
      add(
        made(
          s"$name.bias",
          published(s"$path.bias").shape,
          values(s"$path.bias")
        )
      )
    def conv(name: String, path: String): Unit = {
      val (weight, shape) = folded(path)
      val Vector(out, in, taps) = shape.dimensions
      add(made(s"$name.weight", Shape.of(out, in * taps), weight))
      bias(name, path)
    }
    // [in, out, taps] as [out × taps, in]
    def transposed(name: String, path: String): Unit = {
      val (weight, shape) = folded(path)
      val Vector(in, out, taps) = shape.dimensions.map(_.toInt)
      val laid = new Array[Float](weight.length)
      for {
        i <- 0 until in
        o <- 0 until out
        k <- 0 until taps
      } laid((o * taps + k) * in + i) = weight((i * out + o) * taps + k)
      add(made(s"$name.weight", Shape.of(out.toLong * taps, in), laid))
      bias(name, path)
    }
    def snake(name: String, path: String): Unit = {
      val alpha = values(s"$path.alpha")
      add(made(s"$name.alpha", Shape.of(alpha.length), alpha))
    }
    def lstm(name: String, path: String): Unit =
      for (
        layer <- Iterator.from(0).takeWhile(l => has(s"$path.weight_ih_l$l"))
      ) {
        def matrix(part: String, as: String): Unit = {
          val stored = published(s"$path.${part}_l$layer")
          add(halves(s"$name.$layer.$as", stored.shape, stored.decode()))
        }
        matrix("weight_ih", "input")
        matrix("weight_hh", "recurrent")
        val (input, state) =
          (values(s"$path.bias_ih_l$layer"), values(s"$path.bias_hh_l$layer"))
        add(
          made(
            s"$name.$layer.bias",
            Shape.of(input.length),
            Array.tabulate(input.length)(i => input(i) + state(i))
          )
        )
      }
    // a convolution's weight, its weight norm folded, and its shape
    def folded(path: String): (Array[Float], Shape) =
      if (!has(s"$path.weight_v"))
        (values(s"$path.weight"), published(s"$path.weight").shape)
      else {
        val direction = published(s"$path.weight_v")
        (
          weightNormed(direction.decode(), values(s"$path.weight_g")),
          direction.shape
        )
      }

    val wm = "decoder.wm_model"
    conv("out_proj", "quantizer.out_proj")
    conv("first", "decoder.model.0")
    val blocks =
      Iterator
        .from(1)
        .takeWhile(i => has(s"decoder.model.$i.block.1.bias"))
        .size
    for (i <- 0 until blocks) {
      val block = s"decoder.model.${i + 1}.block"
      snake(s"blocks.$i.snake", s"$block.0")
      transposed(s"blocks.$i.up", s"$block.1")
      // the units of the sound's path, between those of the watermark's
      for ((layer, j) <- Seq(4, 5, 8).zipWithIndex) {
        val unit = s"$block.$layer.block"
        snake(s"blocks.$i.units.$j.snake1", s"$unit.0")
        conv(s"blocks.$i.units.$j.conv1", s"$unit.1")
        snake(s"blocks.$i.units.$j.snake2", s"$unit.2")
        conv(s"blocks.$i.units.$j.conv2", s"$unit.3")
      }
      if (published.tensors.keys.exists(_.startsWith(s"$block.9.block.")))
        throw new FormatException(s"$block.9: a unit VIOLET's decoder has not")
      transposed(s"blocks.$i.mark.up", s"$block.3")
      conv(s"blocks.$i.mark.down", s"$block.11")
      for ((layer, name) <- Seq(6 -> "unit_up", 7 -> "unit_down")) {
        conv(s"blocks.$i.mark.$name.conv1", s"$block.$layer.block.1")
        conv(s"blocks.$i.mark.$name.conv2", s"$block.$layer.block.3")
      }
    }
    snake("tail.snake", s"$wm.encoder_block.pre.0")
    conv("tail.conv", s"$wm.encoder_block.pre.1")
    conv("mark.in", s"$wm.encoder_block.pre.3")
    lstm("mark.encoder.lstm", s"$wm.encoder_block.post.0.lstm")
    conv("mark.encoder.out", s"$wm.encoder_block.post.2")
    add(
      published(s"$wm.msg_processor.msg_processor.weight")
        .copy(name = "mark.message")
    )
    conv("mark.decoder.in", s"$wm.decoder_block.pre.0")
    lstm("mark.decoder.lstm", s"$wm.decoder_block.pre.1.lstm")
    conv("mark.decoder.out", s"$wm.decoder_block.post.1")
    tensors.result()
  }

  // the lanes PyTorch's weight-norm kernel sums a row's squares in
  private val lanes = 8

  /** `v · g / ‖v‖`, a row of `v` for each of `g`, in floats as PyTorch's kernel
    * computes it on the processor (`weight_norm_first_dim_kernel`): the squares
    * summed in `lanes` sums, those added in halves, the norm's square root, `g`
    * divided by it, each value times that.
    */
  def weightNormed(v: Array[Float], g: Array[Float]): Array[Float] = {
    val rows = g.length
    val width = v.length / rows
    val weight = new Array[Float](v.length)
    IntStream.range(0, rows).parallel().forEach { row =>
      val start = row * width
      val sums = new Array[Float](lanes)
      for (i <- 0 until width) {
        val value = v(start + i)
        sums(i % lanes) += value * value
      }
      var half = lanes / 2
      while (half > 0) {
        for (lane <- 0 until half) sums(lane) += sums(lane + half)
        half /= 2
      }
      val scale = g(row) / math.sqrt(sums(0)).toFloat
      for (i <- start until start + width) weight(i) = v(i) * scale
    }
    weight
  }

  private val float = JAVA_FLOAT_UNALIGNED.withOrder(ByteOrder.LITTLE_ENDIAN)
  private val half = JAVA_SHORT_UNALIGNED.withOrder(ByteOrder.LITTLE_ENDIAN)

  /** Values made on the heap, in F32. */
  private def made(
      name: String,
      shape: Shape,
      values: Array[Float]
  ): StoredTensor = {
    val bytes = MemorySegment.ofArray(new Array[Byte](4 * values.length))
    for (i <- values.indices) bytes.set(float, 4L * i, values(i))
    StoredTensor(name, DType.F32, shape, 0, bytes)
  }

  /** Values made on the heap, rounded to F16 (to the nearest, ties to even, as
    * PyTorch rounds them).
    */
  private def halves(
      name: String,
      shape: Shape,
      values: Array[Float]
  ): StoredTensor = {
    val bytes = MemorySegment.ofArray(new Array[Byte](2 * values.length))
    for (i <- values.indices) bytes.set(half, 2L * i, floatToFloat16(values(i)))
    StoredTensor(name, DType.F16, shape, 0, bytes)
  }
}
