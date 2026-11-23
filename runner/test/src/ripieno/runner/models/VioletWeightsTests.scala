package ripieno.runner.models

import ripieno.backend.models.{Download, Downloaded}
import ripieno.runner.formats.{StoredTensor, TorchCheckpoint}
import ripieno.runner.tensor.MappedFile

import java.lang.foreign.ValueLayout.JAVA_BYTE
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.HexFormat

import utest.*

/** VIOLET's weights read from the files its authors publish (downloaded first
  * where the server keeps them when they are not there, `Downloaded`) against
  * what its reference made of them with PyTorch: every tensor's type, shape and
  * bytes (`violet/weights.json`, their fingerprints).
  */
object VioletWeightsTests extends TestSuite {

  private lazy val violet = Downloaded.folder(Download.Violet)

  private val expected = ujson.read(
    Files.readString(
      Path.of(getClass.getResource("/violet/weights.json").toURI)
    )
  )

  private def sha256(tensor: StoredTensor): String =
    HexFormat
      .of()
      .formatHex(
        MessageDigest
          .getInstance("SHA-256")
          .digest(tensor.bytes.toArray(JAVA_BYTE))
      )

  private def check(
      model: String,
      published: String,
      tensorsOf: TorchCheckpoint => Map[String, StoredTensor]
  ): Unit = {
    val path = violet.resolve(published)
    val file = new MappedFile(path)
    try {
      val tensors =
        tensorsOf(TorchCheckpoint.read(file.segment, path.toString))
      val wanted = expected(model).obj
      assert(tensors.keySet == wanted.keySet.toSet)
      for ((name, fingerprint) <- wanted) {
        val tensor = tensors(name)
        assert(
          (
            name,
            tensor.dtype.toString,
            tensor.shape.dimensions.toSeq,
            sha256(tensor)
          ) == (
            name,
            fingerprint("dtype").str,
            fingerprint("shape").arr.map(_.num.toLong).toSeq,
            fingerprint("sha256").str
          )
        )
      }
      println(s"  ${wanted.size} tensors, identical to PyTorch's to the bit")
    } finally file.close()
  }

  val tests: Tests = Tests {
    test("the network, from its EMA snapshot pickled whole") {
      check(
        "network",
        VioletWeights.NetworkFile,
        VioletWeights.networkTensors
      )
    }
    test("the audio decoder, from its state dict archived") {
      check(
        "decoder",
        VioletWeights.DecoderFile,
        VioletWeights.decoderTensors
      )
    }
  }
}
