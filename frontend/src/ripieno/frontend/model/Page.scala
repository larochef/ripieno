package ripieno.frontend.model

import scala.scalajs.js.URIUtils

/** What the page shows, which its address says, so that it can be linked to,
  * reloaded, and gone back from with the browser.
  */
enum Page {

  /** The library's home: what to go on with, the player's works, the catalog.
    */
  case Home

  /** The works whose names hold those words, for that instrument. */
  case Search(words: String, instrument: Option[String])

  /** The works of a collection provided together. */
  case Collection(name: String)

  /** A work: its movements and parts. */
  case Work(id: Long)

  /** Movements of a work on the sheet, one after the other. */
  case Score(work: Long, movements: List[Long])

  /** The address that shows it. */
  def query: String = this match {
    case Home                      => ""
    case Search(words, instrument) =>
      "?" + (List(s"search=${encoded(words)}") ++
        instrument.map(kind => s"instrument=${encoded(kind)}")).mkString("&")
    case Collection(name)       => s"?collection=${encoded(name)}"
    case Work(id)               => s"?work=$id"
    case Score(work, movements) =>
      s"?work=$work&movements=${movements.mkString(",")}"
  }

  private def encoded(text: String): String = URIUtils.encodeURIComponent(text)
}

object Page {

  /** The page an address shows: the library's home when it names none. */
  def of(parameters: Map[String, String]): Page = {
    val work = parameters.get("work").flatMap(_.toLongOption)
    val movements = parameters
      .get("movements")
      .map(_.split(',').toList.flatMap(_.toLongOption))
      .filter(_.nonEmpty)
    (work, movements) match {
      case (Some(id), Some(ids)) => Score(id, ids)
      case (Some(id), None)      => Work(id)
      case _                     =>
        parameters
          .get("collection")
          .map(Collection(_))
          .orElse(
            parameters
              .get("search")
              .map(Search(_, parameters.get("instrument").filter(_.nonEmpty)))
          )
          .getOrElse(Home)
    }
  }
}
