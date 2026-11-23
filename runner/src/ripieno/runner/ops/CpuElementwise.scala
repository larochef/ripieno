package ripieno.runner.ops

import ripieno.runner.tensor.*

/** The reference backend's elementwise arithmetic.
  */
private[ops] trait CpuElementwise extends CpuBase {

  def add(a: Tensor, b: Tensor, out: Tensor): Unit = {
    Ops.checkElementwise("add", a, b, out)
    val (left, right) = (reader(a), reader(b))
    fill(out)(i => left(i) + right(i))
  }

  def lstmStep(
      input: Tensor,
      recurrent: Tensor,
      bias: Tensor,
      cell: Tensor,
      hidden: Tensor
  ): Unit = {
    val width = Ops.checkLstmStep(input, recurrent, bias, cell, hidden)
    val (in, rec, b, c) =
      (reader(input), reader(recurrent), reader(bias), reader(cell))
    def sigmoid(v: Double): Double = 1 / (1 + math.exp(-v))
    def gate(index: Int, j: Long): Double = {
      val at = index * width + j
      in(at) + rec(at) + b(at)
    }
    val cells = Array.tabulate(width.toInt) { j =>
      sigmoid(gate(1, j)) * c(j) + sigmoid(gate(0, j)) * math.tanh(gate(2, j))
    }
    val states = Array.tabulate(width.toInt)(j =>
      sigmoid(gate(3, j)) * math.tanh(cells(j))
    )
    fill(cell)(j => cells(j.toInt))
    fill(hidden)(j => states(j.toInt))
  }

  def mul(a: Tensor, b: Tensor, out: Tensor): Unit = {
    Ops.checkElementwise("mul", a, b, out)
    val (left, right) = (reader(a), reader(b))
    fill(out)(i => left(i) * right(i))
  }

  def scale(x: Tensor, factor: Float, out: Tensor): Unit = {
    Ops.checkElementwise("scale", x, out)
    val input = reader(x)
    fill(out)(i => input(i) * factor)
  }

  def addRow(x: Tensor, row: Tensor, out: Tensor): Unit = {
    Ops.checkAddRow(x, row, out)
    val (input, vector, columns) = (reader(x), reader(row), x.shape.last)
    fill(out)(i => input(i) + vector(i % columns))
  }

  def modulate(x: Tensor, scale: Tensor, shift: Tensor, out: Tensor): Unit = {
    Ops.checkRowWise("modulate", x, out, scale, shift)
    val (input, scales, shifts) = (reader(x), reader(scale), reader(shift))
    val cols = x.shape.last
    fill(out)(i => input(i) * (1 + scales(i % cols)) + shifts(i % cols))
  }

  def gatedAdd(x: Tensor, y: Tensor, gate: Tensor): Unit = {
    Ops.checkRowWise("gatedAdd", x, y, gate)
    val (before, added, gates) = (reader(x), reader(y), reader(gate))
    val cols = x.shape.last
    val values =
      Array.tabulate(x.shape.elementCount.toInt)(i =>
        before(i) + gates(i % cols) * added(i)
      )
    fill(x)(i => values(i.toInt))
  }

  def modulateChunks(
      x: Tensor,
      table: Tensor,
      shift: Int,
      scale: Int,
      out: Tensor
  ): Unit = {
    val (_, channels, chunks) =
      Ops.checkChunks("modulateChunks", x, table, out, shift, scale)
    val (input, values) = (reader(x), reader(table))
    fill(out) { index =>
      val (r, c) = (index / channels, index % channels)
      val row = r * chunks * channels
      input(index) * (1 + values(row + scale * channels + c)) +
        values(row + shift * channels + c)
    }
  }

  def gatedAddChunk(x: Tensor, y: Tensor, table: Tensor, gate: Int): Unit = {
    val (_, channels, chunks) =
      Ops.checkChunks("gatedAddChunk", x, table, y, gate)
    val (input, other, values) = (reader(x), reader(y), reader(table))
    fill(x) { index =>
      val (r, c) = (index / channels, index % channels)
      input(index) + values(
        r * chunks * channels + gate * channels + c
      ) * other(index)
    }
  }

  def rowGatedAdd(x: Tensor, y: Tensor, gate: Tensor): Unit = {
    val (_, cols) = Ops.checkRowGatedAdd(x, y, gate)
    val (input, other, gates) = (reader(x), reader(y), reader(gate))
    fill(x)(index => input(index) + gates(index / cols) * other(index))
  }

  def maxAbs(x: Tensor): Float = {
    Ops.requireF32("maxAbs", x)
    val input = reader(x)
    (0L until x.shape.elementCount)
      .map(i => math.abs(input(i)))
      .filterNot(_.isNaN)
      .maxOption
      .getOrElse(0.0)
      .toFloat
  }
}
