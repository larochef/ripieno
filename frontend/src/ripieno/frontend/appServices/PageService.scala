package ripieno.frontend.appServices

import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.utils.RememberedSettings
import ripieno.shared.ScorePart

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The part the player plays, said in the commands that already show a part and
  * choose the parts heard: none, to listen to the whole score; one, shown alone
  * while the others are heard, to practise it.
  *
  * A score opens to listen to it whole. Recording plays a part: the one chosen,
  * else the one played last, the one of that name if this score has one.
  */
class PageService(bus: AppBus) {
  private var parts: List[ScorePart] = Nil
  private var path = ""
  private var chosen: Option[Int] = None
  private var guide = false
  // the name of the part chosen last, to choose it again in another score
  private var lastName: Option[String] = None

  bus.commands.events.foreach {
    case ChoosePart(part) => choose(part)
    case SetGuide(on)     =>
      guide = on
      choose(chosen)
    case RecordMyPart =>
      if (chosen.isEmpty && parts.sizeIs > 1) {
        choose(remembered)
      }
      // once the part chosen is shown: its showing is published by the
      // commands just sent, after this one is handled
      dom.window.setTimeout(() => bus.send(StartRecording), 0)
    case SetListening(listening) =>
      RememberedSettings.listening = listening
      bus.publish(ListeningSet(listening))
    case ShowSounds(on)   => bus.publish(SoundsShown(on))
    case ShowChecks(on)   => bus.publish(ChecksShown(on))
    case ShowTempi(on)    => bus.publish(TempiShown(on))
    case ShowPassages(on) => bus.publish(PassagesShown(on))
    case _                => ()
  }(using unsafeWindowOwner)

  bus.events.events.foreach {
    case opened: ScoreOpened if opened.path != path =>
      path = opened.path
      parts = opened.parts
      chosen = None
      choose(None)
    case opened: ScoreOpened => parts = opened.parts
    case _                   => ()
  }(using unsafeWindowOwner)

  // the part of the name chosen last, else the first
  private def remembered: Option[Int] =
    Option.when(parts.nonEmpty)(
      lastName
        .map(name => parts.indexWhere(_.name == name))
        .filter(_ >= 0)
        .getOrElse(0)
    )

  private def choose(part: Option[Int]): Unit = {
    chosen = part
    part.flatMap(parts.lift).foreach(found => lastName = Some(found.name))
    val all = parts.indices.toSet
    bus.send(ShowPart(part.filter(_ => parts.sizeIs > 1)))
    val heard = part.fold(all)(own => if (guide) all else all - own)
    if (heard.nonEmpty) {
      bus.send(PlayParts(heard))
    }
    bus.publish(PartChosen(part, guide))
  }
}
