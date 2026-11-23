package ripieno.frontend.components

import com.raquo.laminar
import com.raquo.laminar.modifiers.{Modifier, RenderableNode}
import com.raquo.laminar.nodes.ChildNode.Base
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLElement

trait ReactiveElement { self =>
  lazy val node: ReactiveHtmlElement[HTMLElement]

  def apply(
      mods: Modifier[ReactiveHtmlElement[HTMLElement]]*
  ): ReactiveElement = {

    new ReactiveElement {
      override lazy val node = self.node.amend(mods*)
    }
  }
}

object ReactiveElement {
  given RenderableNode[ReactiveElement] = new RenderableNode[ReactiveElement] {
    override def asNode(value: ReactiveElement): Base = value.node

    override def asNodeSeq(
        values: laminar.Seq[ReactiveElement]
    ): laminar.Seq[Base] = values.map(asNode)

    override def asNodeOption(value: Option[ReactiveElement]): Option[Base] =
      value.map(asNode)
  }
}
