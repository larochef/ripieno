package ripieno.runner

import ripieno.backend.models.{Download, Downloaded}
import ripieno.runner.ops.CpuOps
import ripieno.runner.weights.WeightSource

import java.nio.file.{Files, Path}

import utest.*

/** TensorFlow checkpoints read as TensorFlow reads them: MIDI-DDSP's, as
  * Magenta publishes them (downloaded first where the server keeps them when
  * they are not there, `Downloaded`), against each variable's shape and a
  * fingerprint of its values that TensorFlow gave
  * (`midi-ddsp/checkpoints.json`).
  */
object TensorflowCheckpointTests extends TestSuite {

  private lazy val weights = Downloaded.folder(Download.MidiDdsp)

  private val expected = ujson.read(
    Files.readString(
      Path.of(getClass.getResource("/midi-ddsp/checkpoints.json").toURI)
    )
  )

  private def check(model: String, step: Int): Unit = {
    val index = weights.resolve(s"$model/$step.index")
    val ops = new CpuOps
    val source = WeightSource.open(ops, index)
    try {
      val variables = expected(model).obj
      assert(source.names.toSet == variables.keySet.toSet)
      for ((name, wanted) <- variables) {
        val values = source.hostValues(name)
        val shape = wanted("shape").arr.map(_.num.toLong).toSeq
        assert((name, source.shape(name).dimensions.toSeq) == (name, shape))
        assert(
          (name, values.map(_.toDouble).sum) == (
            name,
            wanted("sum").num
          ) || math.abs(values.map(_.toDouble).sum - wanted("sum").num) < 1e-6
        )
        assert(
          (name, values.head.toDouble, values.last.toDouble) == (
            name,
            wanted("first").num,
            wanted("last").num
          )
        )
      }
      println(
        s"  ${variables.size} variables, ${variables.values.map(_("shape").arr.map(_.num.toLong).product).sum} values, as TensorFlow reads them"
      )
    } finally {
      source.close()
      ops.close()
    }
  }

  val tests: Tests = Tests {
    test("MIDI-DDSP's expression generator") {
      check("expression_generator", 5000)
    }
    test("MIDI-DDSP's synthesis generator") {
      check("synthesis_generator", 50000)
    }
  }
}
