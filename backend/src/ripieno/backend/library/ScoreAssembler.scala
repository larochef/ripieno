package ripieno.backend.library

import java.io.{StringReader, StringWriter}
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

import org.w3c.dom.*
import org.xml.sax.InputSource

/** Movements as one MusicXML score: the work's parts, named as the player named
  * them, each running through the movements one after the other. A movement
  * opens on a new line with its title above it, and ends with a final bar line;
  * its measures are numbered on from the last.
  */
object ScoreAssembler {

  /** A movement: its title, its MusicXML, and which of its parts is each part
    * of the work (the part's id in the MusicXML, the work's name for it).
    */
  final case class Movement(
      title: String,
      musicXml: String,
      parts: List[(String, String)]
  )

  def assembled(movements: List[Movement], titled: Boolean): String = {
    val documents = movements.map(movement => parsed(movement.musicXml))
    val base = documents.head
    val root = base.getDocumentElement
    val workParts = movements.head.parts
    // the parts of the first movement are the work's; each movement's own are found by the mapping
    val partList = child(root, "part-list")
    val baseParts = children(root, "part")
    if (workParts.nonEmpty) {
      // the work's names on the parts
      children(partList, "score-part").foreach { scorePart =>
        workParts.find(_._1 == scorePart.getAttribute("id")).foreach {
          (_, name) =>
            val partName = child(scorePart, "part-name")
            if (partName != null) partName.setTextContent(name)
        }
      }
    }
    if (movements.sizeIs > 1 || titled) {
      // the movements after the first, appended part by part
      val partIds = baseParts.map(_.getAttribute("id"))
      var number = measures(baseParts.headOption).size
      if (titled) {
        markStart(base, baseParts, movements.head.title)
      }
      endWithFinalBar(base, baseParts)
      movements.zip(documents).drop(1).foreach { (movement, document) =>
        val own = children(document.getDocumentElement, "part")
        // the movement's part for each part of the work, by the mapping, else by position
        val matched = partIds.zipWithIndex.map { (id, index) =>
          val workName = workParts.find(_._1 == id).map(_._2)
          val byMapping = workName
            .flatMap(name => movement.parts.find(_._2 == name).map(_._1))
          byMapping
            .flatMap(xmlId => own.find(_.getAttribute("id") == xmlId))
            .orElse(own.lift(index))
        }
        val length =
          matched.flatten.headOption.fold(0)(part => measures(Some(part)).size)
        markStart(document, matched.flatten, movement.title)
        endWithFinalBar(document, matched.flatten)
        baseParts.zip(matched).foreach { (target, source) =>
          val sourceMeasures =
            source.fold(List.empty[Element])(part => measures(Some(part)))
          (0 until length).foreach { index =>
            val measure = sourceMeasures
              .lift(index)
              .map(found => base.importNode(found, true).asInstanceOf[Element])
              .getOrElse(base.createElement("measure"))
            measure.setAttribute("number", (number + index + 1).toString)
            measure.removeAttribute("implicit")
            target.appendChild(measure)
          }
        }
        number += length
      }
    }
    serialized(base)
  }

  /** The parts of a MusicXML score: their ids and names, in order. */
  def partsOf(musicXml: String): List[(String, String)] = {
    val root = parsed(musicXml).getDocumentElement
    children(child(root, "part-list"), "score-part").zipWithIndex.map {
      (part, index) =>
        val name = Option(child(part, "part-name"))
          .map(_.getTextContent.trim)
          .filter(_.nonEmpty)
          .getOrElse(s"Part ${index + 1}")
        (part.getAttribute("id"), name)
    }
  }

  /** The movement's or the work's title the score gives, if any. */
  def titleOf(musicXml: String): Option[String] =
    if (musicXml.isEmpty) None
    else {
      val root = parsed(musicXml).getDocumentElement
      Option(child(root, "movement-title"))
        .orElse(
          Option(child(root, "work")).flatMap(work =>
            Option(child(work, "work-title"))
          )
        )
        .map(_.getTextContent.trim)
        .filter(_.nonEmpty)
    }

  def measuresOf(musicXml: String): Int =
    measures(
      children(parsed(musicXml).getDocumentElement, "part").headOption
    ).size

  // A movement's title above its first measure, on a new line.
  private def markStart(
      document: Document,
      parts: List[Element],
      title: String
  ): Unit =
    parts.headOption.flatMap(part => measures(Some(part)).headOption).foreach {
      first =>
        val direction = document.createElement("direction")
        direction.setAttribute("placement", "above")
        val kind = document.createElement("direction-type")
        val words = document.createElement("words")
        words.setAttribute("font-weight", "bold")
        words.setAttribute("font-size", "14")
        words.setTextContent(title)
        kind.appendChild(words)
        direction.appendChild(kind)
        val print = document.createElement("print")
        print.setAttribute("new-system", "yes")
        parts.foreach { part =>
          measures(Some(part)).headOption.foreach { measure =>
            if (child(measure, "print") == null) {
              measure.insertBefore(print.cloneNode(true), measure.getFirstChild)
            }
          }
        }
        val attributes = child(first, "attributes")
        val after =
          if (attributes != null) attributes.getNextSibling
          else first.getFirstChild
        first.insertBefore(direction, after)
    }

  private def endWithFinalBar(document: Document, parts: List[Element]): Unit =
    parts.foreach { part =>
      measures(Some(part)).lastOption.foreach { last =>
        val right =
          children(last, "barline").find(_.getAttribute("location") != "left")
        if (right.isEmpty) {
          val barline = document.createElement("barline")
          barline.setAttribute("location", "right")
          val style = document.createElement("bar-style")
          style.setTextContent("light-heavy")
          barline.appendChild(style)
          last.appendChild(barline)
        }
      }
    }

  private def measures(part: Option[Element]): List[Element] =
    part.fold(Nil)(found => children(found, "measure"))

  private def children(parent: Element, name: String): List[Element] =
    if (parent == null) Nil
    else {
      val nodes = parent.getChildNodes
      (0 until nodes.getLength).toList
        .map(nodes.item)
        .collect {
          case element: Element if element.getTagName == name => element
        }
    }

  private def child(parent: Element, name: String): Element =
    children(parent, name).headOption.orNull

  private def parsed(musicXml: String): Document = {
    val factory = DocumentBuilderFactory.newInstance()
    // a score's DTD is never fetched
    factory.setFeature(
      "http://apache.org/xml/features/nonvalidating/load-external-dtd",
      false
    )
    factory.setFeature(
      "http://xml.org/sax/features/external-general-entities",
      false
    )
    factory.newDocumentBuilder().parse(InputSource(StringReader(musicXml)))
  }

  private def serialized(document: Document): String = {
    val transformer = TransformerFactory.newInstance().newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    val written = StringWriter()
    transformer.transform(DOMSource(document: Node), StreamResult(written))
    written.toString
  }
}
