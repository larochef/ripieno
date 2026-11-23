package ripieno.frontend.components

import ripieno.frontend.i18n.Tr
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.shared.ReviewSummary

import scala.scalajs.js

import com.raquo.laminar.api.L.*

/** The recordings made of the score opened, and how the comparable ones went
  * from one to the next: those of the same part, over measures in common.
  */
class RecordingHistory(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events

  private val recordings: Signal[List[ReviewSummary]] = events
    .collect { case RecordingsListed(_, found) => found }
    .startWith(Nil)

  // the recording a first click on Delete asked about
  private val deleting: Var[Option[String]] = Var(None)

  private def share(count: Int, of: Int): Double =
    if (of == 0) 0 else 100.0 * count / of

  private def measures(recording: ReviewSummary): Option[(Int, Int)] =
    for {
      from <- recording.fromMeasure.toIntOption
      to <- recording.toMeasure.toIntOption
    } yield (from, to)

  // Comparable with another: the same part, over measures they have in common.
  private def comparable(one: ReviewSummary, other: ReviewSummary): Boolean =
    one.part == other.part && ((measures(one), measures(other)) match {
      case (Some((a, b)), Some((c, d))) => a <= d && c <= b
      case _                            => true
    })

  private def when(recording: ReviewSummary): String =
    new js.Date(recording.recordedAt).toLocaleString()

  private def tempo(recording: ReviewSummary): String =
    recording.beatsPerMinute.fold("")(bpm =>
      s"${recording.beat.getOrElse("♩")} = ${math.round(bpm)}"
    )

  private def figures(recording: ReviewSummary): String = {
    val right = f"${share(recording.right, recording.notes)}%.0f %%"
    def french(count: Int, one: String, many: String) =
      s"$count ${if (count <= 1) one else many}"
    Tr(
      s"$right right · ${recording.wrong} wrong · " +
        s"${recording.intonation} out of tune · ${recording.rhythm} off time",
      s"$right correctes · ${french(recording.wrong, "fausse note", "fausses notes")} · " +
        s"${french(recording.intonation, "pas juste", "pas justes")} · ${recording.rhythm} hors du temps"
    )
  }

  // A card, the panel beside the sheet being narrow.
  private def row(recording: ReviewSummary): HtmlElement =
    div(
      cls := "history-item",
      div(
        cls := "history-when",
        List(
          when(recording),
          recording.part.getOrElse(""),
          Tr(
            s"m. ${recording.fromMeasure}–${recording.toMeasure}",
            s"mes. ${recording.fromMeasure}–${recording.toMeasure}"
          ),
          tempo(recording)
        )
          .filter(_.nonEmpty)
          .mkString(" · ")
      ),
      div(
        cls := "history-figures",
        figures(recording),
        Option.when(
          recording.clippedShare.exists(
            _ >= PerformanceReviewPanel.clippingWorthSaying
          )
        )(
          span(
            cls := "tag is-warning is-light history-tag",
            Tr("saturated", "saturé")
          )
        )
      ),
      div(
        cls := "history-actions",
        button(
          cls := "button is-small",
          Tr("Open", "Ouvrir"),
          onClick --> (_ => bus.send(OpenRecording(recording.id)))
        ),
        button(
          cls := "button is-small",
          cls("is-danger") <-- deleting.signal.map(_.contains(recording.id)),
          child.text <-- deleting.signal.map(asked =>
            if (asked.contains(recording.id)) Tr("Sure?", "Vraiment ?")
            else Tr("Delete", "Supprimer")
          ),
          onClick.compose(_.sample(deleting.signal)) --> { asked =>
            if (asked.contains(recording.id)) {
              deleting.set(None)
              bus.send(DeleteRecording(recording.id))
            } else {
              deleting.set(Some(recording.id))
            }
          }
        )
      )
    )

  // Lines across the recordings, oldest first: values from 0 to `top`.
  private def chart(
      title: String,
      series: List[(String, String, List[Double])],
      top: Double,
      unit: String
  ): SvgElement = {
    import svg.*
    val count = series.headOption.fold(0)(_._3.size)
    def across(index: Int) =
      if (count <= 1) 300.0 else 30 + index * 540.0 / (count - 1)
    def height(value: Double) =
      70 - math.max(0, math.min(top, value)) / top * 60
    svg(
      cls := "history-chart",
      viewBox := "0 0 600 90",
      text(
        x := "4",
        y := "12",
        cls := "review-tempo-label",
        Tr(
          s"$title ($unit, up to ${math.round(top)})",
          s"$title ($unit, jusqu'à ${math.round(top)})"
        )
      ),
      line(
        x1 := "30",
        x2 := "570",
        y1 := "70",
        y2 := "70",
        cls := "review-tempo-even"
      ),
      series.flatMap { (name, kind, values) =>
        List(
          polyline(
            points := values.zipWithIndex
              .map((value, index) =>
                f"${across(index)}%.1f,${height(value)}%.1f"
              )
              .mkString(" "),
            cls := s"history-line history-$kind"
          ),
          text(
            x := f"${across(values.size - 1) + 4}%.1f",
            y := f"${height(values.lastOption.getOrElse(0.0))}%.1f",
            cls := s"review-tempo-label history-$kind-label",
            f"$name ${values.lastOption.getOrElse(0.0)}%.0f"
          )
        )
      },
      (0 until count).map(index =>
        text(
          x := f"${across(index)}%.1f",
          y := "86",
          textAnchor := "middle",
          cls := "review-tempo-label",
          (index + 1).toString
        )
      )
    )
  }

  private def progress(
      latest: ReviewSummary,
      all: List[ReviewSummary]
  ): Option[HtmlElement] = {
    val compared = all.filter(comparable(latest, _)).reverse
    Option.when(compared.sizeIs >= 2) {
      val tempos = compared.flatMap(_.beatsPerMinute)
      div(
        cls := "history-progress",
        div(
          cls := "review-title",
          Tr(
            s"Progress: your ${compared.size} recordings of ${latest.part.getOrElse("this part")} over the same measures, the oldest first",
            s"Progrès : vos ${compared.size} enregistrements de ${latest.part.getOrElse("cette partie")} sur les mêmes mesures, du plus ancien au plus récent"
          )
        ),
        chart(
          Tr("Notes", "Notes"),
          List(
            (
              Tr("heard as written", "comme écrites"),
              "right",
              compared.map(r => share(r.right, r.notes))
            ),
            (
              Tr("out of tune", "pas justes"),
              "intonation",
              compared.map(r => share(r.intonation, r.notes))
            ),
            (
              Tr("off time", "hors du temps"),
              "rhythm",
              compared.map(r => share(r.rhythm, r.notes))
            )
          ),
          100,
          Tr("% of the notes", "% des notes")
        ),
        Option.when(tempos.sizeIs == compared.size)(
          chart(
            Tr("Tempo", "Tempo"),
            List((latest.beat.getOrElse("♩"), "tempo", tempos)),
            math.max(1.0, tempos.max * 1.2),
            Tr("beats a minute", "temps par minute")
          )
        )
      )
    }
  }

  private val shown: Signal[Boolean] = events
    .collect {
      case RecordingsShown(on) => on
      case _: ScoreLoading     => false
    }
    .startWith(false)

  override lazy val node: HtmlElement = div(
    cls := "recording-history",
    child.maybe <-- recordings.combineWith(shown).map { (all, on) =>
      Option.when(on && all.nonEmpty)(
        div(
          cls := "history",
          div(
            cls := "review-header",
            span(
              cls := "review-heading",
              Tr(
                s"Your recordings of this score (${all.size})",
                s"Vos enregistrements de cette partition (${all.size})"
              )
            ),
            button(
              cls := "delete",
              onClick --> (_ => bus.send(ShowRecordings(false)))
            )
          ),
          all.headOption.flatMap(progress(_, all)),
          div(cls := "history-items", all.map(row))
        )
      )
    }
  )
}
