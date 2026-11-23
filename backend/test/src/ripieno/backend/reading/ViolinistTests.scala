package ripieno.backend.reading

import utest.*

/** How the first desk bows dots: apart, on the string in a slow movement and
  * off it otherwise, short ones off it in any; under a slur, flying in one
  * up-bow; before a downbeat they would bring up-bow, in one up-bow too.
  */
object ViolinistTests extends TestSuite {

  // a group of four: two dots apart, two under a slur starting on the first,
  // then a short dot
  private def group(pace: Pace): List[Bowing] = {
    def dot(slurred: Boolean, starts: Boolean, quarters: Double) =
      StringNote(
        Set("staccato"),
        slurred,
        None,
        false,
        quarters,
        slurStarts = starts,
        pace = pace
      )
    Violinist.bowings(
      List(
        dot(false, false, 0.5),
        dot(false, true, 0.5),
        dot(true, false, 0.5),
        dot(false, false, 0.25)
      )
    )
  }

  val tests: Tests = Tests {
    test("a slow movement's dots: on the string apart, flying under a slur") {
      val bowings = group(Pace.Slow)
      assert(
        bowings.map(_.technique) == List(
          Technique.Staccato,
          Technique.Spiccato,
          Technique.FlyingSpiccato,
          Technique.Spiccato
        ),
        bowings.map(_.bow) == List(Bow.Down, Bow.Up, Bow.Up, Bow.Down)
      )
    }
    test("a quick movement's dots: off the string, flying under a slur") {
      val bowings = group(Pace.Quick)
      assert(
        bowings.map(_.technique) == List(
          Technique.Spiccato,
          Technique.Spiccato,
          Technique.FlyingSpiccato,
          Technique.Spiccato
        ),
        bowings.map(_.bow) == List(Bow.Down, Bow.Up, Bow.Up, Bow.Down)
      )
    }
    test("dots under a slur up-bow, even on a downbeat after a rest") {
      def dot(slurred: Boolean, starts: Boolean, afterRest: Boolean) =
        StringNote(
          Set("staccato"),
          slurred,
          None,
          false,
          0.5,
          slurStarts = starts,
          afterRest = afterRest,
          downbeat = afterRest
        )
      val bowings = Violinist.bowings(
        List(dot(false, true, true)) ++ List.fill(3)(dot(true, false, false))
      )
      assert(bowings.forall(_.bow == Bow.Up))
    }
    test("three dots before a downbeat they would bring up-bow: one up-bow") {
      def note(
          signs: Set[String],
          slurred: Boolean = false,
          starts: Boolean = false,
          downbeat: Boolean = false
      ) =
        StringNote(
          signs,
          slurred,
          None,
          false,
          0.5,
          slurStarts = starts,
          downbeat = downbeat,
          pace = Pace.Slow
        )
      val dot = note(Set("staccato"))
      // a slurred pair on the downbeat, a quarter, three dots, the downbeat
      val bowings = Violinist.bowings(
        List(
          note(Set.empty, starts = true, downbeat = true),
          note(Set.empty, slurred = true),
          note(Set.empty),
          dot,
          dot,
          dot,
          note(Set.empty, downbeat = true)
        )
      )
      assert(
        bowings.map(_.bow) == List(
          Bow.Down,
          Bow.Down,
          Bow.Up,
          Bow.Up,
          Bow.Up,
          Bow.Up,
          Bow.Down
        ),
        bowings.slice(3, 6).forall(_.technique == Technique.FlyingSpiccato),
        bowings.forall(!_.retaken)
      )
    }
    test("a pickup pushed, the downbeat after it drawn") {
      def note(
          afterRest: Boolean,
          downbeat: Boolean,
          slurred: Boolean = false
      ) =
        StringNote(
          Set.empty,
          slurred,
          None,
          false,
          0.5,
          afterRest = afterRest,
          downbeat = downbeat
        )
      // one note up to the downbeat, then two slurred into the next bar's
      // after a rest
      val bowings = Violinist.bowings(
        List(
          note(true, false),
          note(false, true),
          note(false, false),
          note(true, false),
          note(false, false, slurred = true),
          note(false, true)
        )
      )
      assert(
        bowings.map(_.bow) == List(
          Bow.Up,
          Bow.Down,
          Bow.Up,
          Bow.Up,
          Bow.Up,
          Bow.Down
        ),
        bowings.forall(!_.retaken)
      )
    }
    test("the character a movement's words name") {
      assert(
        TempoGuess.pace("Andante").contains(Pace.Slow),
        TempoGuess.pace("Romanze").contains(Pace.Slow),
        TempoGuess.pace("Allegretto").contains(Pace.Moderate),
        TempoGuess.pace("Menuetto").contains(Pace.Moderate),
        TempoGuess.pace("Allegro").contains(Pace.Quick),
        TempoGuess.pace("Rondo").contains(Pace.Quick),
        TempoGuess.pace("Trio").isEmpty
      )
    }
  }
}
