package ripieno.backend.reading

/** A note or a rest of a voice, in a measure counted from 0, lasting
  * `quarters`; a note's `pitch` (a chord's first), a rest's none.
  */
final case class PlayedNote(
    measure: Int,
    voice: String,
    quarters: Double,
    pitch: Option[String]
)

/** A tempo, in quarter notes a minute, for a part of a score that writes none:
  * from the words starting it and the rhythm it runs in. A movement is played
  * so that its quickest running notes go at about the rate its character asks
  * (an Allegro running in sixteenths near a quarter at 85, one running in
  * eighths quicker), within the range of tempi that character allows.
  */
/** How a movement goes, as a string player bows its dots: slowly on the string,
  * moderately off it and held, quickly off it and short.
  */
enum Pace {
  case Slow, Moderate, Quick
}

object TempoGuess {

  /** A character of movement: the words naming it, its range of tempi, the
    * running notes it goes at, a minute, and how it goes.
    */
  final private case class Character(
      words: List[String],
      slowest: Double,
      quickest: Double,
      notesAMinute: Double,
      pace: Pace
  )

  // the words of tempo first, the more particular before the more general
  // ("allegretto" before "allegro"); dances and forms when no tempo is given.
  // The rates climb with the words, so that each goes quicker than the one
  // before in the same rhythm: running in sixteenths, an Andante goes at a
  // quarter of 65, an Allegro 85, a Presto 120.
  private val characters = List(
    Character(List("prestissimo"), 130, 220, 540, Pace.Quick),
    Character(List("presto"), 116, 208, 480, Pace.Quick),
    Character(List("vivace", "vivo", "vif"), 96, 176, 400, Pace.Quick),
    Character(List("allegretto"), 76, 124, 320, Pace.Moderate),
    Character(List("allegr", "animé"), 80, 144, 340, Pace.Quick),
    Character(List("moderato", "modéré"), 72, 112, 300, Pace.Moderate),
    Character(List("andantino"), 66, 100, 280, Pace.Slow),
    Character(List("andant"), 60, 96, 260, Pace.Slow),
    Character(List("larghetto"), 54, 72, 240, Pace.Slow),
    Character(List("adagi"), 50, 76, 220, Pace.Slow),
    Character(List("lento", "lent"), 44, 60, 200, Pace.Slow),
    Character(List("largo"), 40, 60, 180, Pace.Slow),
    Character(List("grave"), 36, 56, 160, Pace.Slow),
    Character(List("scherzo"), 116, 200, 480, Pace.Quick),
    Character(List("gigue", "giga"), 96, 170, 400, Pace.Quick),
    Character(List("finale", "rondo"), 80, 150, 340, Pace.Quick),
    Character(List("valse"), 100, 180, 340, Pace.Quick),
    Character(List("bourrée", "gavotte"), 76, 140, 320, Pace.Moderate),
    Character(List("menuet", "minuet"), 90, 140, 300, Pace.Moderate),
    Character(List("courante"), 76, 120, 300, Pace.Moderate),
    Character(List("polonaise"), 72, 100, 280, Pace.Moderate),
    Character(List("prélude", "prelude"), 56, 100, 280, Pace.Moderate),
    Character(List("allemande"), 60, 92, 260, Pace.Moderate),
    Character(List("romance", "romanze", "aria"), 56, 84, 260, Pace.Slow),
    Character(List("sarabande", "sicilien"), 46, 70, 220, Pace.Slow)
  )

  // a score that names no character: a moderate one
  private val unnamed = Character(Nil, 60, 140, 300, Pace.Moderate)

  /** How words naming a character say the movement goes, when they name one.
    */
  def pace(words: String): Option[Pace] =
    characters
      .find(_.words.exists(words.toLowerCase.contains))
      .map(_.pace)

  // a share of its measures a rhythm must run in to set the tempo: a run of a
  // measure or two does not make the movement quick
  private val runningShare = 0.05
  private val runningMeasures = 2

  // how much quicker running notes go when the beat is a half note (2/2, alla
  // breve): an Andante in 2/2 runs its sixteenths quicker than one in 4/4
  private val halfBeatQuicker = 1.25

  /** The tempo of a part named `name`, playing `played` (its measures slowed by
    * the score's words left out), its beat a half note or not; a "Trio" goes at
    * the tempo `before` it.
    */
  def tempo(
      name: Option[String],
      played: Seq[PlayedNote],
      halfBeat: Boolean,
      before: Option[Double]
  ): Double = {
    val words = name.fold("")(_.toLowerCase)
    before.filter(_ => words.trim == "trio").getOrElse {
      val character = characters
        .find(_.words.exists(words.contains))
        .getOrElse(unnamed)
      quickestRunning(played).fold(
        (character.slowest + character.quickest) / 2
      )(value =>
        math.max(
          character.slowest,
          math.min(
            character.quickest,
            character.notesAMinute * value *
              (if (halfBeat) halfBeatQuicker else 1.0)
          )
        )
      )
    }
  }

  // The quickest rhythm the music runs in, in quarters: in each measure, the
  // shortest notes that fill half of a voice; of those, the quickest found in
  // enough measures.
  private def quickestRunning(played: Seq[PlayedNote]): Option[Double] = {
    val byMeasure = played
      .groupBy(_.measure)
      .view
      .mapValues(inMeasure =>
        inMeasure.groupBy(_.voice).values.flatMap(running).minOption
      )
      .collect { case (_, Some(value)) => value }
      .toList
    val enough =
      math.max(runningMeasures, math.ceil(byMeasure.size * runningShare).toInt)
    byMeasure
      .groupBy(value => math.round(value * 48))
      .toList
      .sortBy(_._1)
      .collectFirst { case (_, same) if same.size >= enough => same.head }
  }

  // the shortest notes that fill half of a voice's measure, if notes do
  // A note moving on: one that is neither of the two before it in its voice.
  // Repeated notes and a tremolo's two notes in turn, quick as they are
  // written, ask no quick fingers: they leave the tempo free.
  private def running(voice: Seq[PlayedNote]): Option[Double] = {
    val length = voice.map(_.quarters).sum
    val pitches = voice.flatMap(_.pitch)
    val moving = voice
      .filter(_.pitch.isDefined)
      .zipWithIndex
      .collect {
        case (note, index)
            if !pitches.slice(index - 2, index).contains(note.pitch.get) =>
          note.quarters
      }
      .filter(_ > 0)
    moving.distinct.sorted.find(value =>
      moving.filter(_ <= value + 1e-9).sum >= length / 2 - 1e-9
    )
  }
}
