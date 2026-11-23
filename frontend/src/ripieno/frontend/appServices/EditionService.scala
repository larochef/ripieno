package ripieno.frontend.appServices

import ripieno.frontend.api.ApiClient
import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.utils.AppStreams
import ripieno.shared.*

import com.raquo.laminar.api.L.*

/** The editions of the score opened: which are shown, which one edits go to,
  * and keeping them on the server as each edit is made.
  */
class EditionService(bus: AppBus, apiClient: ApiClient) {

  private var path = ""
  private var editions: List[Edition] = Nil
  private var active: Option[String] = None
  private val firstEdition = "Corrections"

  bus.commands.events.foreach {
    case OpenScore(opened)         => load(opened)
    case SetEditing(on)            => bus.publish(EditingSet(on))
    case command @ ApplyEdit(edit) => change(command)(addTo(_, edit))
    case UndoEdit                  =>
      change(UndoEdit)(edition =>
        edition.copy(edits = edition.edits.dropRight(1))
      )
    case command @ NewEdition(name)           => create(command, name)
    case command @ ShowEdition(name, visible) =>
      editions = editions.map(edition =>
        if (edition.name == name) edition.copy(visible = visible) else edition
      )
      save(command)
    case command @ UseEdition(name) =>
      active = Some(name)
      editions = editions.map(edition =>
        if (edition.name == name) edition.copy(visible = true) else edition
      )
      save(command)
    case _ => ()
  }(using unsafeWindowOwner)

  // A score that cannot have editions is still opened, without any.
  private def load(opened: String): Unit = {
    path = opened
    editions = Nil
    active = None
    AppStreams.runOnce(apiClient.editions(opened)) { result =>
      if (opened == path) {
        result.left.foreach(reason =>
          bus.publish(
            RequestFailed(
              OpenScore(opened),
              Tr(s"No editions: $reason", s"Pas de versions : $reason")
            )
          )
        )
        editions = result.fold(_ => Nil, _.editions)
        active = editions.headOption.map(_.name)
        publish()
      }
    }
  }

  private def addTo(edition: Edition, edit: Edit): Edition =
    edition.copy(edits = edition.edits :+ edit)

  // Changes the edition edits go to, making the first one if there is none.
  private def change(command: AppCommand)(changed: Edition => Edition): Unit = {
    if (active.isEmpty) {
      editions = editions :+ Edition(firstEdition, visible = true, Nil)
      active = Some(firstEdition)
    }
    editions = editions.map(edition =>
      if (active.contains(edition.name)) changed(edition) else edition
    )
    save(command)
  }

  private def create(command: AppCommand, name: String): Unit = {
    val chosen = name.trim
    if (chosen.isEmpty || editions.exists(_.name == chosen)) {
      bus.publish(
        RequestFailed(
          command,
          Tr(
            "An edition needs a name of its own",
            "Une version doit avoir un nom bien à elle"
          )
        )
      )
    } else {
      editions = editions :+ Edition(chosen, visible = true, Nil)
      active = Some(chosen)
      save(command)
    }
  }

  // Shown at once; kept on the server behind.
  private def save(command: AppCommand): Unit = {
    publish()
    val saved = path
    AppStreams.runOnce(apiClient.saveEditions(saved, Editions(editions))) {
      case Left(reason) =>
        bus.publish(
          RequestFailed(
            command,
            Tr(
              s"The edition was not kept: $reason",
              s"La version n'a pas été enregistrée : $reason"
            )
          )
        )
      case Right(_) => ()
    }
  }

  private def publish(): Unit =
    bus.publish(EditionsChanged(path, editions, active))
}
