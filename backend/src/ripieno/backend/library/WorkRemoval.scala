package ripieno.backend.library

import ripieno.backend.editions.EditionStore
import ripieno.backend.review.PerformanceReviewer
import ripieno.backend.tempi.TempoStore

/** Takes a work of the player's own out of the library with all that was kept
  * of its movements: editions, tempi and recordings. Movement ids can be given
  * again to a later import, which must not find them.
  */
final class WorkRemoval(
    works: LibraryStore,
    editions: EditionStore,
    tempi: TempoStore,
    reviewer: PerformanceReviewer
) {
  def delete(work: Long): Either[String, Unit] =
    works.deleteWork(work).map { movements =>
      val gone = movements.toSet
      editions.forget(gone)
      tempi.forget(gone)
      reviewer.forget(gone)
    }
}
