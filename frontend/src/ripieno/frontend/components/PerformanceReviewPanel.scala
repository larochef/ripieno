package ripieno.frontend.components

import ripieno.frontend.i18n.{NoteNames, Tr}
import ripieno.frontend.model.*
import ripieno.frontend.model.AppCommand.*
import ripieno.frontend.model.AppEvent.*
import ripieno.frontend.score.Engraving
import ripieno.shared.*

import com.raquo.laminar.api.L.*

/** What the review of a recording found, measure by measure; a measure clicked
  * is where playback goes, and the recording can be heard again with the sheet
  * following it.
  */
class PerformanceReviewPanel(bus: AppBus) extends ReactiveElement {
  private val events: EventStream[AppEvent] = bus.events.events
  private val closed: EventBus[Unit] = EventBus()

  private val reviewed: Signal[Option[(String, PerformanceReview)]] =
    EventStream
      .merge(
        events.collect {
          case PerformanceReviewed(id, review) => Some((id, review))
          case _: ScoreOpened | _: ScoreShown  => None
        },
        closed.events.mapTo(None)
      )
      .startWith(None)

  private def measureLink(measure: String, text: String): HtmlElement =
    a(
      cls := "review-item",
      href := "#",
      Tr(s"m. $measure", s"mes. $measure"),
      span(cls := "review-detail", text),
      onClick.preventDefault --> (_ => bus.send(PlayFromMeasure(measure)))
    )

  private def section(
      title: String,
      kind: String,
      items: List[HtmlElement]
  ): Option[HtmlElement] =
    Option.when(items.nonEmpty)(
      div(
        cls := s"review-section review-$kind",
        div(cls := "review-title", s"$title (${items.size})"),
        div(cls := "review-items", items)
      )
    )

  private def swatch(colour: String): HtmlElement =
    span(cls := "legend-swatch", backgroundColor := colour)

  // what the colours of the notes out of tune or wrong say
  private def pitchLegend: HtmlElement = {
    def swatch(cents: Int) = this.swatch(ReviewWords.intonationColour(cents))
    div(
      cls := "review-legend",
      span(Tr("flat or lower:", "trop bas ou plus bas :")),
      swatch(-25),
      swatch(-38),
      swatch(-50),
      this.swatch(ReviewWords.wrongColour(higher = false)),
      span(Tr("sharp or higher:", "trop haut ou plus haut :")),
      swatch(25),
      swatch(38),
      swatch(50),
      this.swatch(ReviewWords.wrongColour(higher = true)),
      span(
        Tr(
          "(the darker, the further: 25 to 50 cents, then a wrong note)",
          "(plus c'est foncé, plus c'est loin : de 25 à 50 cents, puis fausse note)"
        )
      )
    )
  }

  // what the colours of the notes off time say, in shares of a beat
  private def rhythmLegend: HtmlElement = {
    val beat = 1000.0
    def swatch(share: Double) =
      this.swatch(
        ReviewWords.rhythmColour(math.round(share * beat).toInt, beat)
      )
    val (least, most) = (ReviewWords.slightlyOffTime, ReviewWords.farOffTime)
    div(
      cls := "review-legend",
      span(Tr("early:", "en avance :")),
      swatch(-least),
      swatch(-(least + most) / 2),
      swatch(-most),
      span(Tr("late:", "en retard :")),
      swatch(least),
      swatch((least + most) / 2),
      swatch(most),
      span(
        Tr(
          s"(the darker, the further: ${math.round(least * 100)} to ${math.round(most * 100)} % of a beat)",
          s"(plus c'est foncé, plus c'est loin : de ${math.round(least * 100)} à ${math.round(most * 100)} % d'un temps)"
        )
      )
    )
  }

  // one entry a measure, however many notes of it
  private def byMeasure(
      notes: List[ReviewedNote]
  )(describe: List[ReviewedNote] => String): List[HtmlElement] =
    notes
      .groupBy(_.measure)
      .toList
      .sortBy((_, inMeasure) => inMeasure.map(_.at).min)
      .map((measure, inMeasure) => measureLink(measure, describe(inMeasure)))

  // the engraving shown, which the notes reviewed are in
  private val shownEngraving: Signal[Option[Engraving]] =
    events
      .collect { case shown: ScoreShown => Some(shown.engraving) }
      .startWith(None)

  // the score's written tempo, which the recording's is reckoned against
  private val writtenTempo: Signal[Option[Tempo]] =
    events
      .collect { case opened: ScoreOpened => Some(opened.tempo) }
      .startWith(None)

  /** The tempo the player kept over the whole recording, in the written tempo's
    * beat: the dashed line of the curve.
    */
  private def overallTempo(
      review: PerformanceReview,
      written: Option[Tempo]
  ): Option[String] =
    written
      .filter(_ => review.tempo.overallRatio > 0)
      .map(tempo =>
        s"${tempo.beat.symbol} = ${math.round(tempo.beatsPerMinute / review.tempo.overallRatio)}"
      )

  private def tempoCurve(
      curve: List[TempoPoint],
      overall: Option[String]
  ): Option[SvgElement] = {
    import svg.*
    Option.when(curve.sizeIs > 2) {
      val first = curve.head.at
      val span = math.max(1.0, curve.last.at - first)
      def across(at: Double) = 4 + (at - first) / span * 592
      // from 30 % slower to 30 % faster than the player's own tempo
      def height(tempo: Double) =
        40 - math.max(-0.3, math.min(0.3, tempo - 1)) / 0.3 * 36
      svg(
        cls := "review-tempo-curve",
        viewBox := "0 0 600 80",
        line(
          x1 := "0",
          x2 := "600",
          y1 := "40",
          y2 := "40",
          cls := "review-tempo-even"
        ),
        polyline(
          points := curve
            .map(point => f"${across(point.at)}%.1f,${height(point.tempo)}%.1f")
            .mkString(" "),
          cls := "review-tempo-line"
        ),
        text(
          x := "6",
          y := "12",
          cls := "review-tempo-label",
          Tr("30 % faster", "30 % plus rapide")
        ),
        text(
          x := "6",
          y := "76",
          cls := "review-tempo-label",
          Tr("30 % slower", "30 % plus lent")
        ),
        text(
          x := "594",
          y := "36",
          textAnchor := "end",
          cls := "review-tempo-label",
          overall.fold(Tr("your tempo overall", "votre tempo global"))(value =>
            Tr(s"$value, your tempo overall", s"$value, votre tempo global")
          )
        )
      )
    }
  }

  // the recording playing back, if one is
  private val listening: Signal[Option[String]] = events
    .collect {
      case RecordingPlayedBack(review)     => Some(review)
      case ScoreSoundBack | _: ScoreOpened => None
    }
    .startWith(None)

  private val playing: Signal[Boolean] = events
    .collect {
      case PlaybackStarted                  => true
      case PlaybackPaused | PlaybackStopped => false
    }
    .startWith(false)

  // Listens to the recording, then pauses and resumes it.
  private def listenButton(
      id: String,
      review: PerformanceReview
  ): HtmlElement = {
    val state = listening
      .combineWith(playing)
      .map((heard, on) => (heard.contains(id), on))
    button(
      cls := "button is-small is-link",
      child.text <-- state.map {
        case (true, true)  => Tr("Pause", "Pause")
        case (true, false) => Tr("Resume", "Reprendre")
        case _             => Tr("Listen to it", "L'écouter")
      },
      onClick.compose(_.sample(state)) --> {
        case (true, _) => bus.send(TogglePlayback)
        case _         => bus.send(ListenToRecording(id, review.alignment))
      }
    )
  }

  private def heading(from: String, to: String, right: Int, all: Int): String =
    Tr(
      s"Your recording, measures $from to $to: $right of $all notes heard as written",
      if (right <= 1) {
        s"Votre enregistrement, mesures $from à $to : $right note sur $all entendue comme écrite"
      } else {
        s"Votre enregistrement, mesures $from à $to : $right notes sur $all entendues comme écrites"
      }
    )

  // how far the player's instrument stands from A = 440 Hz
  private def tuning(cents: Int): String = {
    val a = NoteNames.step("A")
    val hertz = f"${440 * math.pow(2, cents / 1200.0)}%.1f"
    val by = math.abs(cents)
    Tr(
      s"Your instrument stands $by cents ${
          if (cents > 0) "high" else "low"
        } ($a = $hertz Hz): " +
        "intonation is measured against that.",
      s"Votre instrument est accordé $by cents ${
          if (cents > 0) "plus haut" else "plus bas"
        } ($a = $hertz Hz) : " +
        "la justesse est mesurée par rapport à cet accord."
    )
  }

  private def content(
      id: String,
      review: PerformanceReview,
      written: Option[Tempo],
      engraving: Option[Engraving]
  ): HtmlElement = {
    val notes = review.notes
    val measures = notes.map(_.measure).distinct
    val pitch = notes.filter(note =>
      note.verdict == "wrong" || note.verdict == "intonation"
    )
    val unclear = notes.filter(note =>
      note.verdict == "missed" || note.verdict == "uncertain"
    )
    val rhythm = notes.filter(_.rhythm.isDefined)
    val drifts = review.tempo.drifts.map(drift =>
      measureLink(
        drift.fromMeasure,
        Tr(
          s"to m. ${drift.toMeasure}: up to ${drift.percent} % ${ReviewWords.drift(drift.kind)} than your tempo elsewhere",
          s"à la mes. ${drift.toMeasure} : jusqu'à ${drift.percent} % ${ReviewWords.drift(drift.kind)} que votre tempo ailleurs"
        )
      )
    )
    val dynamics =
      (review.dynamics.weakChanges ++ review.dynamics.weakWedges).map(weak =>
        measureLink(
          weak.measure,
          weak.text.getOrElse(
            Tr(
              s"${weak.from.getOrElse("")} to ${weak.to.getOrElse("")}",
              s"${weak.from.getOrElse("")} à ${weak.to.getOrElse("")}"
            )
          ) +
            Tr(
              f": heard as ${weak.heardDb}%+.1f dB",
              f" : entendu à ${weak.heardDb}%+.1f dB"
            )
        )
      )
    div(
      cls := "review",
      div(
        cls := "review-header",
        span(
          cls := "review-heading",
          heading(
            measures.headOption.getOrElse(""),
            measures.lastOption.getOrElse(""),
            notes.count(_.verdict == "right"),
            notes.size
          )
        ),
        listenButton(id, review),
        button(
          cls := "button is-small",
          hidden <-- listening.map(!_.contains(id)),
          Tr("Stop", "Arrêter"),
          onClick --> (_ => bus.send(StopPlayback))
        ),
        button(
          cls := "button is-small",
          Tr("Back to the score's sound", "Revenir au son de la partition"),
          onClick --> (_ => bus.send(BackToScoreSound))
        ),
        button(cls := "delete", onClick --> (_ => closed.emit(())))
      ),
      review.clippedShare
        .filter(_ >= PerformanceReviewPanel.clippingWorthSaying)
        .map(share =>
          div(
            cls := "notification is-warning is-light review-warning",
            Tr(
              s"The recording saturates in ${math.round(share * 100)} % of its seconds: notes are heard wrong through it. " +
                "Lower the microphone's input volume (the system's sound settings) or play further from it.",
              s"L'enregistrement sature pendant ${math.round(share * 100)} % de ses secondes : les notes y sont mal entendues. " +
                "Baissez le volume d'entrée du micro (dans les réglages son du système) ou jouez plus loin de lui."
            )
          )
        ),
      review.tuningCents
        .filter(cents =>
          math.abs(cents) >= PerformanceReviewPanel.tuningWorthSaying
        )
        .map(cents =>
          div(
            cls := "review-note",
            tuning(cents)
          )
        ),
      div(
        cls := "review-sections",
        section(
          Tr("Pitch", "Hauteur"),
          "intonation",
          byMeasure(pitch)(inMeasure =>
            inMeasure
              .sortBy(_.at)
              .flatMap(note =>
                if (note.verdict == "wrong") {
                  Some(
                    ReviewWords.wrongNote(
                      note,
                      engraving.flatMap(_.pitch(note.id))
                    )
                  )
                } else note.cents.map(ReviewWords.cents)
              )
              .mkString(", ")
          )
        ).map(_.amend(pitchLegend)),
        section(
          Tr("Rhythm", "Rythme"),
          "rhythm",
          byMeasure(rhythm)(inMeasure =>
            inMeasure
              .flatMap(note => note.rhythm.zip(note.onsetMillis))
              .map((kind, millis) => ReviewWords.offTime(kind, millis))
              .mkString(", ")
          )
        ).map(_.amend(rhythmLegend)),
        section(
          Tr("Not clearly heard", "Pas clairement entendues"),
          "unclear",
          byMeasure(unclear)(inMeasure =>
            if (inMeasure.size == 1) {
              Tr("a note missed or covered", "une note manquée ou couverte")
            } else {
              Tr(
                s"${inMeasure.size} notes missed or covered",
                s"${inMeasure.size} notes manquées ou couvertes"
              )
            }
          )
        ),
        section("Tempo", "tempo", drifts),
        section(Tr("Dynamics", "Nuances"), "dynamics", dynamics),
        Option.when(!review.dynamics.marked)(
          div(
            cls := "review-note",
            Tr(
              "The part marks no dynamics to follow.",
              "La partie n'indique aucune nuance à suivre."
            )
          )
        )
      ),
      tempoCurve(review.tempo.curve, overallTempo(review, written)).map(curve =>
        div(
          cls := "review-tempo",
          span(
            cls := "review-title",
            Tr("Your tempo along the part", "Votre tempo au fil de la partie")
          ),
          curve
        )
      )
    )
  }

  override lazy val node: HtmlElement = div(
    cls := "performance-review",
    child.maybe <-- reviewed
      .combineWith(writtenTempo, shownEngraving)
      .map((found, written, engraving) =>
        found.map((id, review) => content(id, review, written, engraving))
      )
  )
}

object PerformanceReviewPanel {
  // a few seconds clipping is a loud attack; more makes the pitches unreliable
  val clippingWorthSaying = 0.05
  val tuningWorthSaying = 5
}
