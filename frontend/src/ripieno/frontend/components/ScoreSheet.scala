package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.{Engraving, Sounding}
import ripieno.shared.*

import scala.scalajs.js

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** The pages of what is shown of the score, one under the other. A page is only
  * engraved when it nears the window, so a long score opens as fast as a short
  * one.
  */
class ScoreSheet(bus: AppBus) extends ReactiveElement {
  private val partLabels = "g.label, g.labelAbbr"

  private var engraving: Option[Engraving] = None
  private var solo: Option[Int] = None
  private var pages: Vector[dom.Element] = Vector.empty
  private var litNotes: Seq[String] = Seq.empty
  // the class of each note a review found something to say about
  private var reviewed: Map[String, String] = Map.empty
  private var reviewTips: Map[String, String] = Map.empty
  // the colour of each note out of tune or off time: how far, and which way
  private var reviewColours: Map[String, String] = Map.empty
  // the tempo the score is written in, which a recording's beat is reckoned from
  private var writtenTempo: Option[Tempo] = None
  private var editing = false
  // the note a Shift-click selects from: the last one clicked
  private var anchor: Option[String] = None
  // the notes selected
  private var inRange: Set[String] = Set.empty
  // the notes of the suspect measure shown
  private var suspect: Set[String] = Set.empty
  private var selected: Option[String] = None
  // the notes the editions shown change
  private var edited: Set[String] = Set.empty

  private val pagesNearingTheWindow =
    js.Dynamic.newInstance(js.Dynamic.global.IntersectionObserver)(
      (entries: js.Array[js.Dynamic]) =>
        entries.foreach { entry =>
          if (entry.isIntersecting.asInstanceOf[Boolean]) {
            engrave(entry.target.asInstanceOf[dom.Element])
          }
        },
      js.Dynamic.literal(rootMargin = "800px")
    )

  private val sheet: HtmlElement =
    div(cls := "sheet", onClick --> (event => clicked(event)))

  // A click on a part's name shows that part alone, or the whole score again;
  // a click on a note, or a rest, moves playback there.
  private def clicked(event: dom.MouseEvent): Unit = {
    val target = event.target.asInstanceOf[dom.Element]
    Option(target.closest(partLabels)) match {
      case Some(label) =>
        labelIndex(label).foreach(index =>
          bus.send(ShowPart(Option.when(solo.isEmpty)(index)))
        )
      case None =>
        Option(target.closest("g.note, g.rest, g.mRest")).foreach { note =>
          if (editing) {
            bus.send(SelectNote(Some(note.id)))
          } else if (event.shiftKey) {
            // from the note clicked before to this one
            bus.send(SelectRange(anchor.getOrElse(note.id), note.id))
          } else {
            anchor = Some(note.id)
            bus.send(PlayFromNote(note.id))
          }
        }
    }
  }

  // The part a label names: the one of the staff beside it, known by the ids
  // of its notes (n<part>-<measure>-<rank>). Verovio draws a group's label (a
  // piano's, beside its two staves) before the others: their order says nothing.
  private def labelIndex(label: dom.Element): Option[Int] = {
    val box = label.getBoundingClientRect()
    val middle = (box.top + box.bottom) / 2
    def distance(staff: dom.Element): Double = {
      val extent = staff.getBoundingClientRect()
      if (middle >= extent.top && middle <= extent.bottom) 0.0
      else
        math.min(
          math.abs(middle - extent.top),
          math.abs(middle - extent.bottom)
        )
    }
    Option(label.closest("g.system"))
      .flatMap(_.querySelectorAll("g.staff").toList.minByOption(distance))
      .flatMap(
        _.querySelectorAll("[id^='n']").toList.map(_.id).collectFirst {
          case ScoreSheet.noteId(part) => part.toInt
        }
      )
  }

  private def show(
      engraving: Engraving,
      solo: Option[Int],
      revised: Boolean
  ): Unit = {
    // an edit engraves the score again: the sheet stays where it was
    val place = node.ref.scrollTop
    pagesNearingTheWindow.disconnect()
    sheet.ref.innerHTML = ""
    this.engraving = Some(engraving)
    this.solo = solo
    litNotes = Seq.empty
    reviewed = Map.empty
    reviewTips = Map.empty
    reviewColours = Map.empty
    pages = (1 to engraving.pageCount).toVector.map { number =>
      val page = dom.document.createElement("div")
      page.setAttribute("class", "page")
      page.setAttribute("data-page", number.toString)
      sheet.ref.appendChild(page)
      pagesNearingTheWindow.observe(page)
      page
    }
    if (revised) {
      // the pages up to the place are engraved at once, so that it holds
      pages
        .take(
          1 + (place / pageHeight(engraving)).toInt + 1
        )
        .foreach(engrave)
      node.ref.scrollTop = place
    } else {
      node.ref.scrollTop = 0
    }
  }

  // how tall a page shows, from its width and the engraving's page shape
  private def pageHeight(engraving: Engraving): Double =
    math.max(
      1.0,
      sheet.ref.clientWidth * Engraving.pageHeight / Engraving.pageWidth.toDouble
    )

  private def engrave(page: dom.Element): Unit = {
    if (page.childElementCount == 0) {
      engraving.foreach { engraving =>
        page.innerHTML = engraving.page(page.getAttribute("data-page").toInt)
        page.classList.add("is-engraved")
        markReviewed()
        markEdited()
        markRange()
        markSuspect()
      }
    }
  }

  // A review names the notes of the engraving shown when the recording was
  // made, the one still shown: a new one clears it.
  private def review(found: PerformanceReview): Unit = {
    val notes = found.notes
    val beatMillis = writtenTempo.fold(ScoreSheet.unknownBeatMillis)(
      ReviewWords.playedBeatMillis(found, _)
    )
    unmarkReviewed()
    reviewed = notes
      .flatMap(note => ScoreSheet.reviewClass(note).map(note.id -> _))
      .toMap
    reviewTips = notes
      .flatMap(note =>
        ReviewWords.ofNote(note, written(note.id)).map(note.id -> _)
      )
      .toMap
    reviewColours = notes.collect {
      case note if note.verdict == "intonation" && note.cents.isDefined =>
        note.id -> ReviewWords.intonationColour(note.cents.get)
      case note
          if note.verdict == "wrong" &&
            ReviewWords.higher(note, written(note.id)).isDefined =>
        note.id -> ReviewWords.wrongColour(
          ReviewWords.higher(note, written(note.id)).get
        )
      case note
          if ScoreSheet.reviewClass(note).contains("review-rhythm") &&
            note.onsetMillis.isDefined =>
        note.id -> ReviewWords.rhythmColour(note.onsetMillis.get, beatMillis)
    }.toMap
    markReviewed()
  }

  private def written(note: String): Option[Int] =
    engraving.flatMap(_.pitch(note))

  private def markRange(): Unit =
    inRange.foreach(id => noteElement(id).foreach(_.classList.add("in-range")))

  private def markSuspect(): Unit =
    suspect.foreach(id =>
      noteElement(id).foreach(_.classList.add("is-suspect"))
    )

  // Lights a suspect measure's notes and brings the first into view, its page
  // engraved first.
  private def suspectShown(shown: Option[SuspectMeasure]): Unit = {
    suspect.foreach(id =>
      noteElement(id).foreach(_.classList.remove("is-suspect"))
    )
    suspect = shown.fold(Set.empty[String])(_.notes.toSet)
    for {
      found <- shown
      engraving <- engraving
      start <- engraving.measureStart(found.measure)
    } pages.lift(engraving.soundingAt(start).page - 1).foreach(engrave)
    markSuspect()
    shown
      .flatMap(_.notes.headOption)
      .flatMap(noteElement)
      .flatMap(note => Option(note.closest("g.system")))
      .foreach(
        _.asInstanceOf[js.Dynamic].scrollIntoView(
          js.Dynamic.literal(behavior = "smooth", block = "center")
        )
      )
  }

  private def rangeSelected(selected: Option[ScoreRange]): Unit = {
    inRange.foreach(id =>
      noteElement(id).foreach(_.classList.remove("in-range"))
    )
    inRange = selected.fold(Set.empty[String])(range =>
      engraving.fold(Set.empty[String])(_.notesBetween(range.from, range.to))
    )
    markRange()
  }

  private def markEdited(): Unit = {
    edited.foreach(id => noteElement(id).foreach(_.classList.add("is-edited")))
    selected.foreach(id =>
      noteElement(id).foreach(_.classList.add("is-selected"))
    )
  }

  private def selectNote(id: Option[String]): Unit = {
    selected.foreach(id =>
      noteElement(id).foreach(_.classList.remove("is-selected"))
    )
    selected = id
    markEdited()
  }

  private def editionsShown(editions: List[Edition]): Unit = {
    edited.foreach(id =>
      noteElement(id).foreach(_.classList.remove("is-edited"))
    )
    edited = editions
      .filter(_.visible)
      .flatMap(_.edits)
      .flatMap {
        case Edit.AddNote(_, id, _, _, _, _) => List(id)
        case Edit.AddSlur(note, to)          => List(note, to)
        case other                           => List(other.note)
      }
      .toSet
    markEdited()
  }

  // A reviewed note says what is wrong with it when the pointer is over it:
  // an SVG title, which the browser shows as a tooltip.
  private def markReviewed(): Unit =
    reviewed.foreach { (id, name) =>
      noteElement(id).foreach { note =>
        note.classList.add(name)
        reviewColours
          .get(id)
          .foreach(colour =>
            note
              .asInstanceOf[js.Dynamic]
              .style
              .setProperty("--review-colour", colour)
          )
        if (note.querySelector(":scope > title.review-tip") == null) {
          reviewTips.get(id).foreach { words =>
            val tip =
              dom.document.createElementNS(ScoreSheet.svgNamespace, "title")
            tip.setAttribute("class", "review-tip")
            tip.textContent = words
            note.insertBefore(tip, note.firstChild)
          }
        }
      }
    }

  private def unmarkReviewed(): Unit =
    reviewed.foreach { (id, name) =>
      noteElement(id).foreach { note =>
        note.classList.remove(name)
        note.asInstanceOf[js.Dynamic].style.removeProperty("--review-colour")
        note
          .querySelectorAll(":scope > title.review-tip")
          .foreach(tip => note.removeChild(tip))
      }
    }

  private def noteElement(id: String): Option[dom.Element] = Option(
    dom.document.getElementById(id)
  )

  // Lights the notes that sound, and keeps the system of the measure playing
  // in the middle of what shows of the sheet: wherever it is, above or below,
  // however the music got there (played on, a repeat, a seek, a rewind) and
  // wherever a hand moved the sheet. The measure, not a note: a note held
  // over from the system before, or a part silent, would point elsewhere.
  private def follow(sounding: Sounding, seconds: Double): Unit = {
    // the rests lit too: where a part is silent, its rest is where it is
    if (sounding.played != litNotes) {
      pages.lift(sounding.page - 1).foreach(engrave)
      litNotes
        .filterNot(sounding.played.contains)
        .flatMap(noteElement)
        .foreach(_.classList.remove("is-playing"))
      sounding.played
        .flatMap(noteElement)
        .foreach(_.classList.add("is-playing"))
      litNotes = sounding.played
    }
    for {
      engraving <- engraving
      bar <- engraving.barAt(seconds * 1000)
    } {
      pages.lift(engraving.pageOf(bar.id) - 1).foreach(engrave)
      noteElement(bar.id)
        .flatMap(measure => Option(measure.closest("g.system")))
        .foreach(centre)
    }
  }

  // Scrolls the sheet so that the system is in the middle of what shows of it,
  // as near as the sheet's ends allow, unless it is there already or the sheet
  // is still moving there.
  private def centre(system: dom.Element): Unit = {
    val scroller = node.ref
    val lowest = (scroller.scrollHeight - scroller.clientHeight).toDouble
    val wanted = math.max(
      0.0,
      math.min(scroller.scrollTop + offCentre(system), lowest)
    )
    val now = dom.window.performance.now()
    if (
      math.abs(wanted - scroller.scrollTop) > ScoreSheet.centredPixels &&
      now - scrolledAt > ScoreSheet.scrollMillis
    ) {
      scrolledAt = now
      scroller
        .asInstanceOf[js.Dynamic]
        .scrollTo(js.Dynamic.literal(top = wanted, behavior = "smooth"))
    }
  }

  // when the sheet was last moved to follow the music, in milliseconds
  private var scrolledAt = -1e9

  // How far below the middle of what shows of the sheet the system's middle
  // is, in pixels: the sheet's box, within the window, above the player bar.
  private def offCentre(system: dom.Element): Double = {
    val shown = node.ref.getBoundingClientRect()
    val bar = Option(dom.document.querySelector(".player-bar"))
      .map(_.getBoundingClientRect().top)
      .getOrElse(dom.window.innerHeight.toDouble)
    val top = math.max(shown.top, 0.0)
    val bottom = math.min(math.min(shown.bottom, dom.window.innerHeight), bar)
    val box = system.getBoundingClientRect()
    (box.top + box.bottom) / 2 - (top + bottom) / 2
  }

  // A score asked for: the one shown before is taken away at once, a loader
  // showing until the new one is engraved.
  private def unload(): Unit = {
    pagesNearingTheWindow.disconnect()
    sheet.ref.innerHTML = ""
    engraving = None
    pages = Vector.empty
    litNotes = Seq.empty
    node.ref.scrollTop = 0
  }

  private val loading: Signal[Boolean] = bus.events.events
    .collect {
      case _: ScoreLoading    => true
      case _: ScoreShown      => false
      case _: ScoreUnreadable => false
    }
    .startWith(false)

  override lazy val node: HtmlElement = div(
    cls := "sheet-scroller",
    div(
      cls := "sheet-loading",
      hidden <-- loading.map(!_),
      span(cls := "sheet-loader"),
      span(Tr("Preparing the score…", "Préparation de la partition…"))
    ),
    sheet,
    bus.events.events --> {
      case _: ScoreLoading                         => unload()
      case ScoreShown(engraving, solo, _, revised) =>
        show(engraving, solo, revised)
      case EditingSet(on) =>
        editing = on
        if (!on) selectNote(None)
      case NoteSelected(note)                  => selectNote(note.map(_.id))
      case RangeSelected(selected)             => rangeSelected(selected)
      case SuspectShown(shown)                 => suspectShown(shown)
      case EditionsChanged(_, editions, _)     => editionsShown(editions)
      case PlaybackMoved(seconds, sounding, _) => follow(sounding, seconds)
      case PlaybackStopped                     => follow(Sounding.silence, 0)
      case opened: ScoreOpened           => writtenTempo = Some(opened.tempo)
      case PerformanceReviewed(_, found) => review(found)
      case _                             => ()
    }
  )
}

object ScoreSheet {
  val svgNamespace = "http://www.w3.org/2000/svg"
  // the beat a review reckons in when the score gives no tempo
  val unknownBeatMillis = 500.0
  // a smooth scroll lasts about this long: the sheet is not moved again meanwhile
  val scrollMillis = 700.0
  // a system this close to the middle is left where it is
  val centredPixels = 40.0
  // a note's id from the source: its part, measure and rank (ScoreEditing.identify, on the server)
  val noteId = """n(\d+)-\d+-\d+.*""".r

  /** How a reviewed note is shown: what is most wrong with it. */
  def reviewClass(note: ReviewedNote): Option[String] = note.verdict match {
    case "wrong"                    => Some("review-wrong")
    case "missed" | "uncertain"     => Some("review-unclear")
    case "intonation"               => Some("review-intonation")
    case _ if note.rhythm.isDefined => Some("review-rhythm")
    case _                          => None
  }
}
