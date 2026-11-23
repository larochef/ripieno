package ripieno.shared.routes

import ripieno.shared.RenderedSound

import sttp.tapir.*
import sttp.tapir.json.jsoniter.*

object SoundRoutes {

  /** Where the page fetches the sound bank its synthesizer plays with. A plain
    * path and not an endpoint: the page reads it as raw bytes.
    */
  val soundBankPath = "/api/soundfont"

  /** Asks one of the server's engines, by its name, for the sound of a MIDI
    * file, which takes about as long to render as the music lasts: the answer
    * says where the rendering is, and the same MIDI always gets the same one.
    */
  val renderSound
      : PublicEndpoint[(String, Array[Byte]), String, RenderedSound, Any] =
    Routes.baseRoute.post
      .in("renderings")
      .in(query[String]("engine"))
      .in(byteArrayBody)
      .errorOut(Routes.unreadable)
      .out(jsonBody[RenderedSound])

  val renderedSound: PublicEndpoint[String, String, RenderedSound, Any] =
    Routes.baseRoute.get
      .in("renderings" / path[String]("id"))
      .errorOut(Routes.unreadable)
      .out(jsonBody[RenderedSound])

  /** Where a ready rendering is played from. A plain path: it is the source of
    * an audio element, which asks for the parts of the file it needs.
    */
  def renderedAudioPath(id: String): String = s"/api/rendered/$id.wav"
}
