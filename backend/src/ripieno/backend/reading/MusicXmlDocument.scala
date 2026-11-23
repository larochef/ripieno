package ripieno.backend.reading

import java.io.{StringReader, StringWriter}
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

import org.w3c.dom.*
import org.xml.sax.InputSource

/** A MusicXML score as a document, and the ways of walking it the readings
  * share: children by their tag, their text, a path of children.
  */
object MusicXmlDocument {

  def parsed(musicXml: String): Document = {
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

  def serialized(document: Document): String = {
    val transformer = TransformerFactory.newInstance().newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    val written = StringWriter()
    transformer.transform(DOMSource(document: Node), StreamResult(written))
    written.toString
  }

  /** The score's parts, in order. */
  def parts(document: Document): List[Element] =
    children(document.getDocumentElement, "part")

  def children(parent: Element): List[Element] = {
    val nodes = parent.getChildNodes
    (0 until nodes.getLength).toList.map(nodes.item).collect {
      case element: Element => element
    }
  }

  def children(parent: Element, tag: String): List[Element] =
    children(parent).filter(_.getTagName == tag)

  def child(parent: Element, tag: String): Option[Element] =
    children(parent).find(_.getTagName == tag)

  /** The element at the end of a path of children, if there is one. */
  def at(parent: Element, path: String*): Option[Element] =
    path.foldLeft(Option(parent))((found, tag) => found.flatMap(child(_, tag)))

  /** Every element at the end of a path of children. */
  def all(parent: Element, path: String*): List[Element] =
    path.foldLeft(List(parent))((found, tag) => found.flatMap(children(_, tag)))

  def has(parent: Element, tag: String): Boolean = child(parent, tag).isDefined

  /** A child's text, trimmed. */
  def text(parent: Element, tag: String): Option[String] =
    child(parent, tag).map(_.getTextContent.trim)

  /** Every element of that tag under `parent`, at any depth, in order. */
  def descendants(parent: Element, tag: String): List[Element] = {
    val nodes = parent.getElementsByTagName(tag)
    (0 until nodes.getLength).toList.map(nodes.item).collect {
      case element: Element => element
    }
  }

  /** The nearest element of that tag holding `element`, itself included. */
  def closest(element: Element, tag: String): Option[Element] =
    Iterator
      .iterate[Node](element)(_.getParentNode)
      .takeWhile(_ != null)
      .collectFirst { case found: Element if found.getTagName == tag => found }
}
