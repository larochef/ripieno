package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.WorkDetail

import com.raquo.laminar.api.L.*

/** Above the score: the way back to its work, the movements on the sheet, the
  * part the player plays (none to listen to all of it) and the way back to the
  * full score, and the tools beside the score.
  */
class ScoreHeader(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val opened: Signal[Option[ScoreOpened]] =
    events.collect { case opened: ScoreOpened => Some(opened) }.startWith(None)

  private val chosen: Signal[PartChosen] = events
    .collect { case chosen: PartChosen => chosen }
    .startWith(PartChosen(None, guide = false))

  private val work: Signal[Option[WorkDetail]] =
    events.collect { case WorkLoaded(found) => Some(found) }.startWith(None)

  private val page: Signal[Page] =
    events.collect { case PageShown(now) => now }.startWith(Page.Home)

  private val tunerOn: Signal[Boolean] =
    events.collect { case TunerSet(on) => on }.startWith(false)

  private val metronomeOn: Signal[Boolean] =
    events.collect { case MetronomeSet(on) => on }.startWith(false)

  private val recordingCount: Signal[Int] =
    events
      .collect { case RecordingsListed(_, found) => found.size }
      .startWith(0)

  private val recordingsShown: Signal[Boolean] = events
    .collect {
      case RecordingsShown(on) => on
      case _: ScoreLoading     => false
    }
    .startWith(false)

  private val suspectCount: Signal[Int] = events
    .collect {
      case MeasuresSuspected(found) => found.size
      case _: ScoreLoading          => 0
    }
    .startWith(0)

  private val checksShown: Signal[Boolean] = events
    .collect {
      case ChecksShown(on) => on
      case _: ScoreLoading => false
    }
    .startWith(false)

  private val soundsShown: Signal[Boolean] =
    events.collect { case SoundsShown(on) => on }.startWith(false)

  private val passagesShown: Signal[Boolean] = events
    .collect {
      case PassagesShown(on) => on
      case _: ScoreLoading   => false
    }
    .startWith(false)

  // a score with neither repeat nor cut has no drawer for them
  private val hasPassages: Signal[Boolean] = events
    .collect {
      case RepeatsShown(now) => now.nonEmpty
      case _: ScoreLoading   => false
    }
    .startWith(false)
    .combineWith(
      events
        .collect {
          case CutsShown(now)  => now.nonEmpty
          case _: ScoreLoading => false
        }
        .startWith(false)
    )
    .map(_ || _)

  private val tempiShown: Signal[Boolean] = events
    .collect {
      case TempiShown(on)  => on
      case _: ScoreLoading => false
    }
    .startWith(false)

  // The way back: to the work, from which the library is a click away.
  private val back: HtmlElement = div(
    cls := "score-back",
    child <-- work.combineWith(page).map { (found, now) =>
      val target = now match {
        case Page.Score(id, _) => Page.Work(id)
        case _                 => Page.Home
      }
      a(
        cls := "score-back-link",
        href := target.query,
        title := found.fold(
          Tr("Back to the library", "Retour à la bibliothèque")
        )(w =>
          Tr(
            s"Back to ${w.title}: its movements and parts",
            s"Retour à ${w.title} : ses mouvements et ses parties"
          )
        ),
        onClick.preventDefault --> (_ => bus.send(Navigate(target))),
        "‹ ",
        found.fold(Tr("Library", "Bibliothèque"))(_.title)
      )
    },
    // what is on the sheet: the movements opened
    span(
      cls := "score-movements",
      child.text <-- work.combineWith(page).map { (found, now) =>
        (found, now) match {
          case (Some(w), Page.Score(_, ids)) if w.movements.sizeIs > 1 =>
            w.movements
              .filter(m => ids.contains(m.id))
              .map(_.title)
              .mkString(" · ")
          case _ => ""
        }
      }
    )
  )

  // No part chosen: the whole score, listened to; one: practised.
  private val partChoice: HtmlElement = div(
    cls := "part-chooser",
    hidden <-- opened.map(_.forall(_.parts.sizeIs < 2)),
    label(forId := "my-part", Tr("I play", "Je joue")),
    div(
      cls := "select is-small",
      select(
        idAttr := "my-part",
        children <-- opened.map(
          _.fold(Nil)(score =>
            option(
              value := "",
              Tr(
                "No one: I listen to all of it",
                "Aucune : j'écoute tout"
              )
            ) +:
              score.parts.zipWithIndex.map((part, index) =>
                option(value := index.toString, part.name)
              )
          )
        ),
        value <-- chosen.map(_.part.fold("")(_.toString)),
        onChange.mapToValue.map(_.toIntOption) --> (part =>
          bus.send(ChoosePart(part))
        )
      )
    ),
    // the way back to every part, said as plainly as the choice of one
    button(
      cls := "button is-small full-score",
      idAttr := "full-score",
      hidden <-- chosen.map(_.part.isEmpty),
      title := Tr(
        "See and hear every part again",
        "Revoir et réentendre toutes les parties"
      ),
      Tr("Show the full score", "Afficher la partition complète"),
      onClick --> (_ => bus.send(ChoosePart(None)))
    ),
    label(
      cls := "checkbox guide-choice",
      hidden <-- chosen.map(_.part.isEmpty),
      input(
        typ := "checkbox",
        idAttr := "guide",
        checked <-- chosen.map(_.guide),
        onChange.mapToChecked --> (on => bus.send(SetGuide(on)))
      ),
      Tr(" Hear my part too", " Entendre aussi ma partie")
    )
  )

  private def toolButton(
      name: String,
      id: String,
      on: Signal[Boolean],
      hint: String,
      toggle: Boolean => AppCommand
  ): HtmlElement = button(
    cls := "button is-small",
    idAttr := id,
    cls("is-link is-light is-selected") <-- on,
    title := hint,
    name,
    onClick.compose(_.sample(on)) --> (now => bus.send(toggle(!now)))
  )

  override lazy val node: HtmlElement = headerTag(
    cls := "score-header",
    back,
    partChoice,
    div(
      cls := "score-tools",
      button(
        cls := "button is-small is-warning is-light checks-button",
        idAttr := "checks",
        hidden <-- suspectCount.map(_ == 0),
        cls("is-selected") <-- checksShown,
        title := Tr(
          "Measures that may have been read wrong: their length or a tie looks odd",
          "Des mesures peut-être mal lues : leur durée ou une liaison semble anormale"
        ),
        child.text <-- suspectCount.map(count =>
          Tr(s"To check · $count", s"À vérifier · $count")
        ),
        onClick.compose(_.sample(checksShown)) --> (on =>
          bus.send(ShowChecks(!on))
        )
      ),
      button(
        cls := "button is-small",
        idAttr := "recordings",
        hidden <-- recordingCount.map(_ == 0),
        cls("is-link is-light is-selected") <-- recordingsShown,
        title := Tr(
          "Your recordings of this score, their reviews, and how they compare",
          "Vos enregistrements de cette partition, leurs analyses et leur comparaison"
        ),
        child.text <-- recordingCount.map(count =>
          Tr(s"Recordings · $count", s"Enregistrements · $count")
        ),
        onClick.compose(_.sample(recordingsShown)) --> (on =>
          bus.send(ShowRecordings(!on))
        )
      ),
      toolButton(
        Tr("Tuner", "Accordeur"),
        "tuner",
        tunerOn,
        Tr(
          "Listens to the note you play and says how far it is from in tune",
          "Écoute la note que vous jouez et indique de combien elle s'écarte de la justesse"
        ),
        SetTuner(_)
      ),
      toolButton(
        Tr("Metronome", "Métronome"),
        "metronome",
        metronomeOn,
        Tr(
          "Beats with the music while it plays, on its own otherwise",
          "Bat la mesure avec la musique pendant la lecture, seul sinon"
        ),
        SetMetronome(_)
      ),
      toolButton(
        Tr("Cuts and repeats", "Coupures et reprises"),
        "passages",
        passagesShown,
        Tr(
          "The repeats played and the cuts made",
          "Les reprises jouées et les coupures faites"
        ),
        ShowPassages(_)
      ).amend(hidden <-- hasPassages.map(!_)),
      toolButton(
        Tr("Tempo", "Tempo"),
        "tempi",
        tempiShown,
        Tr(
          "The score's tempi, all in view, to set them",
          "Les tempos de la partition, tous en vue, pour les régler"
        ),
        ShowTempi(_)
      ),
      toolButton(
        Tr("Sounds", "Sons"),
        "sounds",
        soundsShown,
        Tr(
          "The sound of the parts, how you listen, and the microphone",
          "Le son des parties, votre écoute et le microphone"
        ),
        ShowSounds(_)
      )
    )
  )
}
