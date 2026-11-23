package ripieno.backend.reading

import ripieno.backend.reading.MusicXmlDocument.*
import ripieno.shared.*

import scala.collection.mutable

import org.w3c.dom.{Document, Element}

/** Reads what a score asks of its players that the engraver's MIDI leaves out
  * (dynamics, articulations, ornaments) and how the parts balance, as
  * instructions on its notes. Rules only for now: what they cannot tell is left
  * as written.
  */
object Conductor {

  // the velocity of each dynamic, and of a score that marks none
  private val dynamics = Map(
    "pppp" -> 20,
    "ppp" -> 30,
    "pp" -> 42,
    "p" -> 55,
    "mp" -> 68,
    "mf" -> 80,
    "f" -> 94,
    "ff" -> 108,
    "fff" -> 118,
    "ffff" -> 124
  )
  private val unmarked = 80
  // a hairpin with no dynamic after it goes this far
  private val wedgeReach = 14
  // what accents add to a note's velocity
  private val accented = Map("accent" -> 16, "strong-accent" -> 24)
  private val forced =
    Set("sf", "sfz", "sffz", "fz", "rf", "rfz", "sfp", "sfzp")
  private val forcedAccent = 24
  // a chord on a one-staff instrument (a violin's double or triple stop) is
  // struck: the bow attacks it
  private val struckChord = 16
  // the share of its length the note before a new phrase sounds
  private val breathShare = 0.5
  // what a run's note on a beat gets, near a new phrase, and how long a run's
  // notes are at most, in quarters
  private val beatAccent = 6
  // what the first of two slurred notes gets, and the second loses
  private val leaning = 8
  private val lettingGo = 6
  private val runQuarters = 0.25
  // what a note ending a section, or held under a fermata, adds to the level
  // it is reached at: sustained, its tone kept, not let fall
  private val sustainedEnd = 4
  // the share of its written length a detached note sounds
  private val detached =
    Map(
      "staccato" -> 0.5,
      "spiccato" -> 0.4,
      "staccatissimo" -> 0.3,
      "strong-accent" -> 0.8
    )
  // the share a dot sounds in a moderate movement (an Allegretto, a minuet):
  // bounced, but held
  private val moderateDot = 0.9
  // what a bow taken again (lifted, to start the same way again) adds to its
  // note's velocity: a fresh attack
  private val retakenAccent = 8
  // a bow's start: drawn from the heel, a down-bow attacks more; pushed from
  // the point, an up-bow less
  private val downBowAccent = 3
  private val upBowLightness = 3
  // an accompanying part's velocities, against the part that carries the music
  private val accompanimentShare = 0.7
  // notes sounding together on average, in a measure: a chordal part, a line
  private val chordal = 2.0
  private val line = 1.3
  // the shortest repeated note, in quarters, given a change of bow
  private val shortestRebowed = 0.5

  /** How the score (its notes given their ids, its parts their instruments) is
    * played.
    */
  def interpretation(document: Document): Interpretation = {
    val scoreParts = parts(document)
    val staves = scoreParts.map(part =>
      children(part, "measure")
        .flatMap(all(_, "attributes", "staves"))
        .flatMap(_.getTextContent.trim.toIntOption)
        .maxOption
        .getOrElse(1)
    )
    val firstTracks = staves.scanLeft(1)(_ + _)
    // where the first part sets off on a new phrase
    val phrases = Phrasing.phraseStarts(document)
    val read = scoreParts.zipWithIndex.map((part, index) =>
      readPart(
        part,
        index,
        firstTracks(index),
        staves(index),
        bowed(document, part),
        if (index == 0) phrases else Nil
      )
    )
    val accompaniments = accompanying(read)
    val softer = accompaniments.map(a => (a.part, a.measure)).toSet
    // the level of the parts carrying the music, in each measure: a part
    // accompanying never plays above it, even when only the soloist's part
    // marks a softer dynamic
    val leading = read
      .flatMap(_.notes)
      .filterNot(note => softer.contains((note.part, note.measure)))
      .groupBy(_.measure)
      .view
      .mapValues { notes =>
        val sorted = notes.map(_.play.velocity).sorted
        sorted(sorted.size / 2)
      }
      .toMap
    val notes = read.flatMap(_.notes).map { note =>
      val velocity =
        if (softer.contains((note.part, note.measure))) {
          val level = leading
            .get(note.measure)
            .fold(note.play.velocity)(math.min(note.play.velocity, _))
          math.round(level * accompanimentShare).toInt
        } else note.play.velocity
      ConductedNote(
        note.id,
        note.track,
        note.pitch,
        note.play.copy(velocity = math.max(1, math.min(127, velocity)))
      )
    }
    Interpretation(notes, accompaniments)
  }

  // a note as read, before the parts are balanced
  final private case class ReadNote(
      id: String,
      part: Int,
      measure: String,
      at: Double,
      track: Int,
      pitch: Int,
      play: NotePlay
  )

  // a part's notes, and for each measure how many notes sound at each onset
  final private case class ReadPart(
      notes: List[ReadNote],
      onsets: Map[String, List[Int]],
      staves: Int
  )

  // a note with its time in quarters from the start of the part, the key and
  // accidentals in force where it is, and the semitones its part sounds from
  // where it is written (a transposing instrument)
  final private case class TimedNote(
      element: Element,
      measure: String,
      at: Double,
      fifths: Int,
      accidentals: Map[(Char, Int), Int],
      quarters: Double,
      voice: String,
      transposed: Int,
      downbeat: Boolean
  )

  // a dynamic, a hairpin's start or end, at its time in quarters from the
  // start of the part, repeats as written
  final private case class Mark(at: Double, kind: String, text: String)

  private def readPart(
      part: Element,
      index: Int,
      firstTrack: Int,
      staves: Int,
      bowed: Boolean,
      phrases: List[Double]
  ): ReadPart = {
    val measures = children(part, "measure")
    var divisions = 1.0
    var fifths = 0
    var transposed = 0
    var measureStart = 0.0
    val marks = List.newBuilder[Mark]
    // each note with its time from the start of the part
    val timed = List.newBuilder[TimedNote]
    val onsets = Map.newBuilder[String, List[Int]]
    // slurs open in each voice, by number
    val openSlurs = mutable.Map.empty[String, Set[String]]
    val underSlur = mutable.Set.empty[Element]
    // the notes a slur starts on, none open before it
    val slurStarting = mutable.Set.empty[Element]
    // the pitches of each voice's last attack, and of the one before it
    val attacks = mutable.Map.empty[String, Set[Int]]
    val previousAttacks = mutable.Map.empty[String, Set[Int]]
    // notes repeating a pitch of the attack before them in their voice, with no
    // slur from it: a change of bow, which a player marks
    val rebowed = mutable.Set.empty[Element]
    for (measure <- measures) {
      val number = measure.getAttribute("number")
      var position = 0.0
      var longest = 0.0
      var lastStart = 0.0
      // the accidentals met so far in the measure, by step and octave
      var altered = Map.empty[(Char, Int), Int]
      val sounding = mutable.Map.empty[Double, Int]
      for (element <- children(measure)) {
        def duration =
          text(element, "duration").flatMap(_.toDoubleOption).getOrElse(0.0) /
            divisions
        element.getTagName match {
          case "attributes" =>
            text(element, "divisions")
              .flatMap(_.toDoubleOption)
              .foreach(divisions = _)
            at(element, "key", "fifths")
              .flatMap(_.getTextContent.trim.toIntOption)
              .foreach(fifths = _)
            child(element, "transpose").foreach { transpose =>
              def semitones(tag: String) =
                text(transpose, tag).flatMap(_.toIntOption).getOrElse(0)
              transposed =
                semitones("chromatic") + 12 * semitones("octave-change")
            }
          case "backup" =>
            position -= duration
          case "forward" =>
            position += duration
            longest = math.max(longest, position)
          case "direction" =>
            for (kind <- all(element, "direction-type").flatMap(children)) {
              kind.getTagName match {
                case "dynamics" =>
                  children(kind).headOption.foreach(mark =>
                    marks += Mark(
                      measureStart + position,
                      "dynamics",
                      mark.getTagName
                    )
                  )
                case "wedge" =>
                  marks += Mark(
                    measureStart + position,
                    "wedge",
                    kind.getAttribute("type")
                  )
                case "words" =>
                  marks += Mark(
                    measureStart + position,
                    "words",
                    kind.getTextContent.trim.toLowerCase
                  )
                case _ => ()
              }
            }
          case "note" =>
            val chord = has(element, "chord")
            val grace = has(element, "grace")
            val rest = has(element, "rest")
            val start = if (chord) lastStart else position
            if (!grace && !chord) {
              lastStart = position
              position += duration
              longest = math.max(longest, position)
            }
            val sounded = spelled(element).map { (step, octave, alter) =>
              altered = altered.updated((step, octave), alter)
              octave * 12 + semitones(steps.indexOf(step)) + alter
            }
            if (!rest && !grace) {
              sounding(start) = sounding.getOrElse(start, 0) + 1
              val voice = text(element, "voice").getOrElse("1")
              val open = openSlurs.getOrElse(voice, Set.empty)
              if (open.nonEmpty) underSlur += element
              val slurs = all(element, "notations", "slur")
              val started = slurs
                .filter(_.getAttribute("type") == "start")
                .map(_.getAttribute("number"))
              val stopped = slurs
                .filter(_.getAttribute("type") == "stop")
                .map(_.getAttribute("number"))
              if (stopped.nonEmpty) underSlur += element
              if (open.isEmpty && started.nonEmpty) slurStarting += element
              openSlurs(voice) = open ++ started -- stopped
              if (!chord) {
                previousAttacks(voice) = attacks.getOrElse(voice, Set.empty)
                attacks(voice) = Set.empty
              }
              attacks(voice) = attacks(voice) ++ sounded
              // a run's repeated sixteenths are played well as they are
              val long = duration >= shortestRebowed
              if (
                long && !tiedFrom(element) && !underSlur.contains(element) &&
                sounded.exists(
                  previousAttacks.getOrElse(voice, Set.empty).contains
                )
              ) rebowed += element
              timed += TimedNote(
                element,
                number,
                measureStart + start,
                fifths,
                altered,
                duration,
                voice,
                transposed,
                // a pickup measure's first note is no downbeat
                start == 0 && measure.getAttribute("implicit") != "yes"
              )
            }
          case _ => ()
        }
      }
      onsets += number -> sounding.values.toList
      measureStart += longest
    }
    val placedMarks =
      withWordHairpins(marks.result().sortBy(_.at), measureStart)
    val played = chords(timed.result())
    // the measures ending a section (a double or final bar) or the part, and
    // the last chord struck in each
    val sectionEnds = measures
      .filter(measure =>
        children(measure, "barline").exists(barline =>
          barline.getAttribute("location") != "left" &&
            text(barline, "bar-style").exists(Set("light-light", "light-heavy"))
        )
      )
      .map(_.getAttribute("number"))
      .toSet ++ measures.lastOption.map(_.getAttribute("number"))
    val ending = played
      .filter(chord => sectionEnds.contains(chord.head.measure))
      .groupBy(_.head.measure)
      .values
      .map(_.maxBy(_.head.at))
      .toSet
    // the measures either side of where a new phrase starts, the two phrases
    // told apart: their runs leaning on each beat
    val phrased = phrases
      .flatMap { start =>
        played.find(chord => math.abs(chord.head.at - start) < 1e-6).toList ++
          played.filter(_.head.at < start - 1e-6).lastOption.toList
      }
      .map(_.head.measure)
      .toSet
    val measureStarts = played
      .groupBy(_.head.measure)
      .view
      .mapValues(_.map(_.head.at).min)
      .toMap
    // two short notes under a slur of their own: the first leant on, as an
    // appoggiatura, the second let go
    def slurs(chord: List[TimedNote], kind: String) =
      chord
        .flatMap(note =>
          all(note.element, "notations", "slur").collect {
            case slur if slur.getAttribute("type") == kind =>
              slur.getAttribute("number")
          }
        )
        .toSet
    val paired = played
      .groupBy(_.head.voice)
      .values
      .flatMap { voice =>
        voice
          .zip(voice.drop(1))
          .collect {
            case (first, second)
                if first.head.quarters <= runQuarters + 1e-6 &&
                  second.head.quarters <= runQuarters + 1e-6 &&
                  slurs(first, "start")
                    .intersect(slurs(second, "stop"))
                    .nonEmpty =>
              List(
                first.head.element -> leaning,
                second.head.element -> -lettingGo
              )
          }
          .flatten
      }
      .toMap
    def onBeatInRun(chord: List[TimedNote]) = {
      val head = chord.head
      phrased.contains(head.measure) && head.quarters <= runQuarters + 1e-6 &&
      measureStarts.get(head.measure).exists { start =>
        val offset = (head.at - start) % 1.0
        offset < 1e-6 || offset > 1 - 1e-6
      }
    }
    // the dynamics as marked, shaped as a player phrases the line
    val line = played
      .groupBy(_.head.at)
      .toVector
      .sortBy(_._1)
      .flatMap { (at, together) =>
        together.flatten
          .flatMap(note => midiPitch(note.element))
          .maxOption
          .map(pitch =>
            LineNote(
              at,
              together.map(_.head.quarters).max,
              pitch,
              together.head.head.measure
            )
          )
      }
    val levelOf = Phrasing.shaped(
      at => levelAt(placedMarks, at),
      placedMarks.collect {
        case mark if mark.kind == "dynamics" && dynamics.contains(mark.text) =>
          mark.at -> dynamics(mark.text)
      },
      placedMarks.collect {
        case mark if mark.kind == "wedge" && mark.text == "stop" => mark.at
      },
      line,
      melodic = staves == 1
    )
    // each voice bowed on its own: a unison's second voice has its own stems
    val bowings =
      if (bowed) {
        val byVoice = played.zipWithIndex
          .groupBy((chord, _) => chord.head.voice)
          .values
          .flatMap { chords =>
            chords
              .map(_._2)
              .zip(
                Violinist.bowings(
                  chords.map(_._1).zipWithIndex.map { (chord, index) =>
                    // after a rest, or first: the bow is taken anew
                    val afterRest = index == 0 || {
                      val before = chords(index - 1)._1.head
                      before.at + before.quarters < chord.head.at - 1e-6
                    }
                    stringNote(chord, placedMarks, underSlur, slurStarting)
                      .copy(
                        afterRest = afterRest,
                        downbeat = chord.head.downbeat
                      )
                  }
                )
              )
          }
          .toMap
        played.indices.map(byVoice.get).toList
      } else played.map(_ => None)
    val notes = played.zip(bowings).zipWithIndex.flatMap {
      case ((chord, bowing), index) =>
        val TimedNote(_, measure, at, keyFifths, accidentals, _, _, _, _) =
          chord.head
        val signs = chord.flatMap(note => signsOf(note.element)).toSet
        val forcedHere = placedMarks.exists(mark =>
          mark.kind == "dynamics" && forced.contains(mark.text) && math.abs(
            mark.at - at
          ) < 1e-6
        )
        val retaken = bowing.exists(_.retaken)
        // a note held under a fermata ends the phrase: its chord set down, not
        // struck
        val held = chord.exists(note => fermata(note.element))
        // a note ending a section, or held under a fermata, keeps its tone: it
        // is sustained, never under the note before it (the line falling to it
        // does not soften it), unless the score writes it softer
        val before = played.lift(index - 1).map(_.head.at)
        val sustained = (held || ending.contains(chord)) &&
          before.forall(time =>
            levelAt(placedMarks, at) >= levelAt(placedMarks, time)
          )
        val level =
          if (!sustained) levelOf(at)
          else
            before.fold(levelOf(at))(time =>
              math.max(levelOf(at), levelOf(time))
            ) + sustainedEnd
        // a new phrase sets off with an attack of its own
        val setsOff = phrases.exists(start => math.abs(start - at) < 1e-6)
        val velocity =
          level + signs.toList.flatMap(accented.get).sum +
            (if (retaken || setsOff) retakenAccent else 0) +
            bowing
              .filterNot(_.joined)
              .fold(0)(each =>
                if (each.bow == Bow.Down) downBowAccent else -upBowLightness
              ) +
            (if (onBeatInRun(chord)) beatAccent else 0) +
            paired.getOrElse(chord.head.element, 0) +
            (if (forcedHere) forcedAccent else 0) +
            (if (staves == 1 && pitchesOf(chord) > 1 && !held) struckChord
             else 0)
        // the note before a new phrase gives it its breath: let go early, the
        // phrase starting in time
        val beforePhrase = phrases
          .exists(start => math.abs(start - (at + chord.head.quarters)) < 1e-6)
        val slurred = chord.exists(note => underSlur.contains(note.element))
        val paceHere = pace(placedMarks, at)
        // a moderate movement's dots apart: off the string, held, not short
        val heldDot = signs.contains("staccato") && paceHere == Pace.Moderate &&
          !slurred && chord.head.quarters > Violinist.offTheString
        val share =
          if (heldDot) moderateDot
          else signs.toList.flatMap(detached.get).minOption.getOrElse(1.0)
        val length = share * (if (beforePhrase) breathShare else 1.0)
        val loure = signs.contains("detached-legato") ||
          (signs.contains("tenuto") && slurred)
        val bowChange = chord.exists(note => rebowed.contains(note.element))
        // a string player's chord of three or four notes, broken
        val breaking =
          if (bowed)
            Violinist.broken(
              chord.map(note => midiPitch(note.element).getOrElse(0)),
              chord.head.quarters,
              followed = chord.forall(note => !fermata(note.element)) &&
                played.exists(next =>
                  next.head.voice == chord.head.voice &&
                    math.abs(next.head.at - (at + chord.head.quarters)) < 1e-6
                )
            )
          else chord.map(_ => (ChordString.Whole, 1.0))
        chord.map(_.element).zip(breaking).flatMap {
          case (note, (string, share)) =>
            for {
              id <- Option(note.getAttribute("id")).filter(_.nonEmpty)
              pitch <- midiPitch(note)
            } yield {
              val staff =
                text(note, "staff").flatMap(_.toIntOption).getOrElse(1)
              ReadNote(
                id,
                index,
                measure,
                at,
                firstTrack + staff - 1,
                pitch + chord.head.transposed,
                NotePlay(
                  math.max(1, math.min(127, velocity)),
                  length * share,
                  // a broken chord's upper notes, coming late, change bow in
                  // that time already
                  loure || (bowChange && string != ChordString.Upper) || retaken,
                  ornamentOf(
                    note,
                    keyFifths,
                    accidentals,
                    tremblement(placedMarks, at) && midiPitch(note) == chord
                      .flatMap(other => midiPitch(other.element))
                      .maxOption,
                    chord.head.quarters
                  ),
                  bowing.map(_.text),
                  string
                )
              )
            }
        }
    }
    ReadPart(notes, onsets.result(), staves)
  }

  // what a string player reads of a chord: a tie, as a slur, keeps the bow
  private def stringNote(
      chord: List[TimedNote],
      marks: List[Mark],
      underSlur: mutable.Set[Element],
      slurStarting: mutable.Set[Element]
  ): StringNote = {
    val head = chord.head
    val plucked = marks
      .filter(mark =>
        mark.kind == "words" && mark.at <= head.at + 1e-6 &&
          (mark.text.startsWith("pizz") || mark.text.startsWith("arco"))
      )
      .lastOption
      .exists(_.text.startsWith("pizz"))
    StringNote(
      chord.flatMap(note => signsOf(note.element)).toSet,
      chord.exists(note =>
        underSlur.contains(note.element) || tiedFrom(note.element)
      ),
      ornamentOf(
        head.element,
        head.fifths,
        head.accidentals,
        tremblement(marks, head.at),
        head.quarters
      ),
      plucked,
      head.quarters,
      chord.exists(note => slurStarting.contains(note.element)),
      pace(marks, head.at)
    )
  }

  // how the movement goes at `at`: the last words naming a character before
  // it say (a "Trio" names none, its movement's goes on)
  private def pace(marks: List[Mark], at: Double): Pace =
    marks
      .filter(mark => mark.kind == "words" && mark.at <= at + 1e-6)
      .flatMap(mark => TempoGuess.pace(mark.text))
      .lastOption
      .getOrElse(Pace.Moderate)

  // a "+" over a note, French for a trill (a tremblement), "(+)" when the
  // editor adds it
  private def tremblement(marks: List[Mark], at: Double): Boolean =
    marks.exists(mark =>
      mark.kind == "words" && (mark.text == "+" || mark.text == "(+)") &&
        math.abs(mark.at - at) < 1e-6
    )

  // a part bowed by its player: a violin, a viola, a cello or a double bass,
  // by the General MIDI program (41 to 44, counted from 1) the preparation
  // gives it from its name (`ScoreCleaning.instrument`)
  private def bowed(document: Document, part: Element): Boolean = {
    val id = part.getAttribute("id")
    at(document.getDocumentElement, "part-list").toList
      .flatMap(children(_, "score-part"))
      .find(_.getAttribute("id") == id)
      .exists(
        descendants(_, "midi-program")
          .flatMap(_.getTextContent.trim.toIntOption)
          .exists(program => program >= 41 && program <= 44)
      )
  }

  // a note's step, octave and alteration, as written
  private def spelled(note: Element): Option[(Char, Int, Int)] =
    for {
      written <- child(note, "pitch")
      step <- text(written, "step").flatMap(_.headOption)
      octave <- text(written, "octave").flatMap(_.toIntOption)
    } yield (
      step,
      octave,
      text(written, "alter").flatMap(_.toDoubleOption).fold(0)(_.round.toInt)
    )

  // a note's pitch as MIDI counts it
  private def midiPitch(note: Element): Option[Int] =
    spelled(note).map((step, octave, alter) =>
      (octave + 1) * 12 + semitones(steps.indexOf(step)) + alter
    )

  // a note tied from the one before it
  private def tiedFrom(note: Element): Boolean =
    children(note, "tie").exists(_.getAttribute("type") == "stop")

  private def fermata(note: Element): Boolean =
    at(note, "notations", "fermata").isDefined

  // how many different pitches a chord sounds: a unison on two strings is one
  private def pitchesOf(chord: List[TimedNote]): Int =
    chord
      .map(note =>
        child(note.element, "pitch").fold("")(
          _.getTextContent.filterNot(_.isWhitespace)
        )
      )
      .distinct
      .size

  // the notes grouped by chord: a chord's notes follow its first one
  private def chords(notes: List[TimedNote]): List[List[TimedNote]] =
    notes
      .foldLeft(List.empty[List[TimedNote]]) { (found, note) =>
        if (has(note.element, "chord") && found.nonEmpty)
          (found.head :+ note) :: found.tail
        else List(note) :: found
      }
      .reverse

  // the articulations and technical marks of a note, by their element's name
  private def signsOf(note: Element): List[String] =
    (all(note, "notations", "articulations") ++
      all(note, "notations", "technical"))
      .flatMap(children)
      .map(_.getTagName)

  private def ornamentOf(
      note: Element,
      fifths: Int,
      accidentals: Map[(Char, Int), Int],
      crossed: Boolean,
      quarters: Double
  ): Option[Ornament] =
    for {
      found <- at(note, "notations", "ornaments")
        .orElse(Option.when(crossed)(note))
      (step, octave, alter) <- spelled(note)
      ornament <- {
        val marks = descendants(found, "accidental-mark")
        def marked(placement: String): Option[Int] =
          marks
            .find(mark =>
              Option(mark.getAttribute("placement"))
                .filter(_.nonEmpty)
                .getOrElse("above") == placement
            )
            .flatMap(mark => accidentalValue(mark.getTextContent.trim))
        def neighbour(direction: Int, placement: String): Int =
          interval(
            step,
            octave,
            alter,
            direction,
            fifths,
            accidentals,
            marked(placement)
          )
        def marking(name: String) = descendants(found, name).nonEmpty
        val tremolo = descendants(found, "tremolo").headOption
          .filter(t =>
            Option(t.getAttribute("type"))
              .forall(kind => kind.isEmpty || kind == "single")
          )
          .flatMap(_.getTextContent.trim.toIntOption)
        if (marking("trill-mark")) Some(Ornament.Trill(neighbour(1, "above")))
        // a "+" on a note shorter than a quarter is a trill all along
        else if (crossed)
          Some(
            if (quarters >= 1) Ornament.Tremblement(neighbour(1, "above"))
            else Ornament.Trill(neighbour(1, "above"))
          )
        else if (marking("inverted-mordent"))
          Some(Ornament.Mordent(neighbour(1, "above")))
        else if (marking("mordent"))
          Some(Ornament.Mordent(neighbour(-1, "below")))
        else if (marking("inverted-turn"))
          Some(Ornament.Turn(neighbour(-1, "below"), neighbour(1, "above")))
        else if (marking("turn"))
          Some(Ornament.Turn(neighbour(1, "above"), neighbour(-1, "below")))
        else
          tremolo.map(strokes => Ornament.Tremolo(1.0 / math.pow(2, strokes)))
      }
    } yield ornament

  private val steps = "CDEFGAB"
  private val semitones = Vector(0, 2, 4, 5, 7, 9, 11)
  private val sharps = "FCGDAEB"

  // the semitones from a note to its diatonic neighbour above (1) or below
  // (-1): the neighbour's accidental is the ornament's, else the one met in
  // the measure, else the key's
  private def interval(
      step: Char,
      octave: Int,
      alter: Int,
      direction: Int,
      fifths: Int,
      accidentals: Map[(Char, Int), Int],
      marked: Option[Int]
  ): Int = {
    val index = steps.indexOf(step)
    val other = Math.floorMod(index + direction, 7)
    val otherOctave = octave + Math.floorDiv(index + direction, 7)
    val otherStep = steps(other)
    val keyAlter =
      if (fifths > 0 && sharps.take(fifths).contains(otherStep)) 1
      else if (fifths < 0 && sharps.reverse.take(-fifths).contains(otherStep))
        -1
      else 0
    val otherAlter = marked
      .orElse(accidentals.get((otherStep, otherOctave)))
      .getOrElse(keyAlter)
    (otherOctave * 12 + semitones(other) + otherAlter) -
      (octave * 12 + semitones(index) + alter)
  }

  private def accidentalValue(text: String): Option[Int] =
    text match {
      case "sharp"                        => Some(1)
      case "natural"                      => Some(0)
      case "flat"                         => Some(-1)
      case "double-sharp" | "sharp-sharp" => Some(2)
      case "flat-flat"                    => Some(-2)
      case _                              => None
    }

  // how far a "cresc." or "dim." with no dynamic or hairpin after it goes on,
  // in quarters
  private val wordReach = 8.0

  // "cresc." or "dim." written in words: a hairpin from there to the next
  // dynamic (or hairpin, or the end of the part, at most `wordReach`). Written
  // where a hairpin ends, it carries that hairpin on, so as not to fall back to
  // the written dynamic first.
  private def withWordHairpins(marks: List[Mark], end: Double): List[Mark] = {
    def kindOf(words: String): Option[String] =
      if (words.contains("decresc") || words.startsWith("dim")) {
        Some("diminuendo")
      } else Option.when(words.contains("cresc"))("crescendo")
    marks
      .collect {
        case mark if mark.kind == "words" =>
          kindOf(mark.text).map(mark -> _)
      }
      .flatten
      .foldLeft(marks) { case (sofar, (word, kind)) =>
        val goesTo = sofar
          .find(mark =>
            mark.at > word.at + 1e-6 &&
              ((mark.kind == "dynamics" && dynamics.contains(mark.text)) ||
                (mark.kind == "wedge" && mark.text != "stop"))
          )
          .fold(math.min(end, word.at + wordReach))(_.at)
        val ending = sofar
          .filter(mark =>
            mark.kind == "wedge" && mark.text != "stop" && mark.at < word.at
          )
          .lastOption
        val carried = sofar.find(mark =>
          mark.kind == "wedge" && mark.text == "stop" &&
            math.abs(mark.at - word.at) < 1e-6 &&
            ending.exists(_.text == kind)
        )
        val started = carried.fold(List(Mark(word.at, "wedge", kind)))(_ => Nil)
        (sofar.filterNot(carried.contains) ++ started ++
          List(Mark(goesTo, "wedge", "stop"))).sortBy(_.at)
      }
  }

  // a note's velocity from the dynamics before it, hairpins rising or falling
  // to the next dynamic
  private def levelAt(marks: List[Mark], at: Double): Int = {
    val levels = marks.filter(mark =>
      mark.kind == "dynamics" && dynamics.contains(mark.text)
    )
    def levelBefore(time: Double): Int =
      levels
        .filter(_.at <= time + 1e-6)
        .lastOption
        .fold(unmarked)(mark => dynamics(mark.text))
    val wedge = marks
      .filter(mark =>
        mark.kind == "wedge" && mark.text != "stop" && mark.at <= at + 1e-6
      )
      .lastOption
    val wedgeEnd = wedge.flatMap(start =>
      marks.find(mark =>
        mark.kind == "wedge" && mark.text == "stop" && mark.at > start.at
      )
    )
    (wedge, wedgeEnd) match {
      case (Some(start), Some(end)) if at < end.at && end.at > start.at =>
        val from = levelBefore(start.at)
        val to = levels
          .find(mark => mark.at >= end.at - 1e-6 && mark.at <= end.at + 4)
          .fold(
            from + (if (start.text == "crescendo") wedgeReach else -wedgeReach)
          )(mark => dynamics(mark.text))
        math
          .round(from + (to - from) * (at - start.at) / (end.at - start.at))
          .toInt
      // a hairpin no dynamic follows: its level kept until the next mark (a
      // next hairpin starts again from the written dynamic)
      case (Some(start), Some(_))
          if !levels.exists(mark =>
            mark.at > start.at + 1e-6 && mark.at <= at + 1e-6
          ) =>
        levelBefore(start.at) +
          (if (start.text == "crescendo") wedgeReach else -wedgeReach)
      case _ => levelBefore(at)
    }
  }

  // the parts accompanying in each measure: a keyboard (a part of several
  // staves) under a melody instrument playing (a part of one), as a sonata's
  // piano under its violin; or a part sounding chords, mostly, while another
  // plays a line. When it is not that clear, nobody
  private def accompanying(
      parts: List[ReadPart]
  ): List[AccompanyingMeasure] = {
    val measures = parts.flatMap(_.onsets.keys).distinct
    for {
      measure <- measures
      averages = parts
        .map(_.onsets.getOrElse(measure, Nil))
        .map(counts =>
          Option.when(counts.nonEmpty)(counts.sum.toDouble / counts.size)
        )
      soloing = parts
        .zip(averages)
        .exists((part, average) => part.staves == 1 && average.nonEmpty)
      lined = averages.exists(_.exists(_ <= line))
      (average, part) <- averages.zipWithIndex
      if average.nonEmpty
      if (soloing && parts(part).staves > 1) ||
        (lined && average.exists(_ >= chordal))
    } yield AccompanyingMeasure(part, measure)
  }
}
