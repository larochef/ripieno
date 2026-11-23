package ripieno.frontend.sound

/** Something playing the opened score, whose clock everything shown follows.
  * Times are seconds of the score at its own tempo, whatever the speed.
  */
trait ScorePlayer {
  def playing: Boolean
  def play(): Unit
  def pause(): Unit

  /** Back at the start of the score, silent. */
  def stop(): Unit
  def seek(seconds: Double): Unit
  def speed: Double
  def speed_=(rate: Double): Unit

  /** How far into the score the sound coming out of the speakers is. */
  def position: Double
  def onEnded(action: () => Unit): Unit
}
