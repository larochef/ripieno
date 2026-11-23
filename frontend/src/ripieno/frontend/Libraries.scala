package ripieno.frontend

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import org.scalajs.dom

/** The JavaScript libraries of the page, which Vite bundles. Nothing of them is
  * typed beyond what it takes to construct them: the classes wrapping them
  * ([[Engraving]], [[Player]]) are where their calls are spelled out.
  */
object Libraries {

  /** Verovio, compiled to WebAssembly. */
  @js.native
  @JSImport("verovio/wasm", JSImport.Default)
  def createVerovioModule(): js.Promise[js.Any] = js.native

  @js.native
  @JSImport("verovio/esm", "VerovioToolkit")
  class VerovioToolkit(module: js.Any) extends js.Object

  @js.native
  @JSImport("spessasynth_lib", "WorkletSynthesizer")
  class WorkletSynthesizer(context: dom.AudioContext) extends js.Object

  @js.native
  @JSImport("spessasynth_lib", "Sequencer")
  class Sequencer(synthesizer: js.Any, options: js.Object) extends js.Object

  /** Where Vite serves the half of the synthesizer that runs on the audio
    * thread.
    */
  @js.native
  @JSImport(
    "spessasynth_lib/dist/spessasynth_processor.min.js?url",
    JSImport.Default
  )
  val processorUrl: String = js.native
}
