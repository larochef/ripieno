package ripieno.backend.reading

import ripieno.shared.{ChordString, Ornament}

/** How a string player draws a note from the instrument. */
enum Technique(val name: String) {

  /** Its own bow. */
  case Detache extends Technique("detache")

  /** In the bow of the note before it, joined to it. */
  case Legato extends Technique("legato")

  /** In the bow of the note before it, a little apart from it. */
  case Loure extends Technique("loure")
  case Staccato extends Technique("staccato")

  /** Off the string. */
  case Spiccato extends Technique("spiccato")

  /** Off the string, in the bow of the note before it: dots under a slur, or
    * dots taken in one bow before a downbeat.
    */
  case FlyingSpiccato extends Technique("volant")
  case Pizzicato extends Technique("pizzicato")
  case Harmonic extends Technique("harmonic")
  case Tremolo extends Technique("tremolo")
  case Trill extends Technique("trill")
}

enum Bow(val name: String) {
  case Down extends Bow("down")
  case Up extends Bow("up")

  def opposite: Bow = if (this == Down) Up else Down
}

/** A note's bowing: its technique, the way the bow goes, and whether the bow is
  * taken again for it, lifted to start the same way as the note before.
  */
final case class Bowing(
    technique: Technique,
    bow: Bow,
    retaken: Boolean = false
) {

  /** How the engines read it, in the MIDI: "technique legato up". */
  def text: String = s"technique ${technique.name} ${bow.name}"

  /** Whether it is drawn in the bow of the note before. */
  def joined: Boolean = Bowing.inTheBow(technique)
}

object Bowing {
  def inTheBow(technique: Technique): Boolean =
    technique == Technique.Legato || technique == Technique.Loure ||
      technique == Technique.FlyingSpiccato
}

/** What a string player reads of a note (a chord, played as one): its marks
  * (articulations and technical marks, by their MusicXML names), whether a slur
  * joins it to the note before, whether one starts on it, its ornament, whether
  * it is plucked (pizz. until arco), its length in quarters, and how the
  * movement goes there (`TempoGuess.pace`).
  */
final case class StringNote(
    signs: Set[String],
    slurred: Boolean,
    ornament: Option[Ornament],
    plucked: Boolean,
    quarters: Double,
    slurStarts: Boolean = false,
    pace: Pace = Pace.Slow,
    afterRest: Boolean = false,
    downbeat: Boolean = false
)

/** The first desk of a string part: turns what the score asks into the
  * instrument's technique, note by note. The bow changes on every note no slur
  * joins to the one before, going down and up in turn unless the score marks
  * which; slurred notes stay in their bow. After a rest (or at the start) the
  * bow is taken anew so that the next downbeat comes down-bow: a pickup is
  * pushed, up-bow. Dots under a slur fly in one up-bow; dots apart that would
  * bring a downbeat up-bow are taken in one up-bow too, flying.
  */
object Violinist {

  /** How a chord of `pitches` lasting `quarters` is broken, the bow taking two
    * strings at most: each note's string, and the share of its length it is
    * held (the struck strings are let go when the late ones are well heard:
    * `Performance` times both, the break being a time, at most a share of the
    * chord). The notes under the two highest are struck first; the two highest
    * follow and are held. Of three notes, the middle one is shared by both
    * pairs and sounds from the start. When a note `followed` it at once, the
    * bow leaves the strings a sixteenth before it, so that it is heard apart
    * (held to its end, the chord swallows a note repeating its top); before a
    * rest, or held on, the chord lasts its time.
    */
  def broken(
      pitches: List[Int],
      quarters: Double,
      followed: Boolean
  ): List[(ChordString, Double)] = {
    val distinct = pitches.distinct.sorted
    // the bow takes two strings at most: every chord of three or four notes is
    // broken, however short (its break is a share of it, `Performance`)
    if (distinct.size < 3)
      pitches.map(_ => (ChordString.Whole, 1.0))
    else {
      val kept =
        if (followed) 1 - math.min(liftQuarters, quarters / 4) / quarters
        else 1.0
      val held = distinct.takeRight(2).toSet
      pitches.map(pitch =>
        if (!held.contains(pitch)) (ChordString.Lower, 1.0)
        else if (distinct.size == 3 && pitch == distinct(1))
          (ChordString.Middle, kept)
        else (ChordString.Upper, kept)
      )
    }
  }

  // how early the bow leaves a broken chord, a sixteenth
  private val liftQuarters = 0.25

  /** A staccato this short, in quarters, is played off the string. */
  val offTheString = 0.25

  def bowings(notes: List[StringNote]): List[Bowing] = {
    val all = notes.toVector
    val techniques = all.map(techniqueOf).toArray
    def marked(index: Int) =
      all(index).signs.exists(Set("down-bow", "up-bow"))
    def dotApart(index: Int) = {
      val note = all(index)
      note.signs.contains("staccato") && !note.slurred && !note.slurStarts &&
      !marked(index)
    }
    // the bow a group after a rest starts with: the next downbeat down-bow,
    // each bow taken before it turning the bow over
    def startOfGroup(index: Int): Option[Bow] = {
      val group = (index until all.size).takeWhile(next =>
        next == index || !all(next).afterRest
      )
      group.find(all(_).downbeat).map { downbeat =>
        val changes = (index + 1 to downbeat)
          .count(next => !Bowing.inTheBow(techniques(next)))
        if (changes % 2 == 0) Bow.Down else Bow.Up
      }
    }
    // the dots apart from `index` up to a downbeat, two to four of a measure
    // with no rest between: taken in one bow when they would bring it up-bow
    def dotsToDownbeat(index: Int): Option[Int] = {
      val run = (index until all.size).takeWhile(next =>
        dotApart(next) && !all(next).downbeat &&
          (next == index || !all(next).afterRest)
      )
      val downbeat = index + run.size
      Option.when(
        run.size >= 2 && run.size <= hookedAtMost && downbeat < all.size &&
          all(downbeat).downbeat && !all(downbeat).afterRest &&
          !marked(downbeat) && (index == 0 || !dotApart(index - 1))
      )(run.size)
    }
    val bowings = Array.ofDim[Bowing](all.size)
    var last = Option.empty[Bow]
    var hooked = 0
    for (index <- all.indices) {
      val note = all(index)
      val alternated = last match {
        case Some(Bow.Down) => Bow.Up
        case _              => Bow.Down
      }
      val hook = if (hooked > 0) None else dotsToDownbeat(index)
      // the run's bows, each turning the bow over, would bring its downbeat
      // up-bow
      val hooking = hook.filter(size =>
        (if (size % 2 == 0) alternated else alternated.opposite) == Bow.Up
      )
      val bow =
        if (note.signs.contains("down-bow")) Bow.Down
        else if (note.signs.contains("up-bow")) Bow.Up
        else if (hooked > 0) last.get
        else if (Bowing.inTheBow(techniques(index)) && last.nonEmpty) last.get
        // dots under a slur start up-bow, the stroke flying in it
        else if (note.slurStarts && note.signs.contains("staccato")) Bow.Up
        else if (hooking.nonEmpty) Bow.Up
        else if (note.afterRest && startOfGroup(index).nonEmpty)
          startOfGroup(index).get
        else alternated
      if (hooked > 0) {
        techniques(index) = Technique.FlyingSpiccato
        hooked -= 1
      } else
        hooking.foreach { size =>
          hooked = size - 1
          // after an up-bow, the run goes on in it
          if (last.contains(Bow.Up))
            techniques(index) = Technique.FlyingSpiccato
        }
      val joined = Bowing.inTheBow(techniques(index))
      // the bow lifted to start the same way again; after a rest it is taken
      // anew anyway
      val retaken = !joined && !note.afterRest && last.contains(bow)
      bowings(index) = Bowing(techniques(index), bow, retaken)
      last = Some(bow)
    }
    bowings.toList
  }

  // the most dots before a downbeat taken in one bow
  private val hookedAtMost = 4

  private def techniqueOf(note: StringNote): Technique = {
    val signs = note.signs
    if (
      note.plucked || signs
        .contains("pizzicato") || signs.contains("snap-pizzicato")
    )
      Technique.Pizzicato
    else if (signs.contains("harmonic")) Technique.Harmonic
    else
      note.ornament match {
        case Some(Ornament.Trill(_))   => Technique.Trill
        case Some(Ornament.Tremolo(_)) => Technique.Tremolo
        case _                         =>
          if (signs.contains("spiccato")) Technique.Spiccato
          // dots under a slur: flying, the bow bouncing in one up-bow; the
          // note the slur starts on takes it
          else if (note.slurred && signs.contains("staccato"))
            Technique.FlyingSpiccato
          else if (note.slurStarts && signs.contains("staccato"))
            Technique.Spiccato
          else if (
            signs.contains("staccato") || signs.contains("staccatissimo")
          ) {
            // apart: on the string when the movement goes slowly, off it
            // otherwise or when short (`Conductor` holds a moderate one's)
            if (note.pace != Pace.Slow || note.quarters <= offTheString)
              Technique.Spiccato
            else Technique.Staccato
          } else if (signs.contains("detached-legato")) Technique.Loure
          else if (note.slurred && signs.contains("tenuto")) Technique.Loure
          else if (note.slurred) Technique.Legato
          else Technique.Detache
      }
  }
}
