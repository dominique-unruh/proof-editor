package ui.mathview

import javafx.geometry.Bounds
import javafx.scene.layout

import cmathml._
import misc.Utils.ImplicitConversions._

import scala.collection.mutable
import scala.util.control.Breaks._
import scalafx.application.Platform
import scalafx.application.Platform.runLater
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer.{Add, Remove, Reorder, Update}
import scalafx.scene.layout._
import scalafx.scene.{Group, Node}

//trait MathHighlight extends Node {
//  def setSize(size:Bounds) : Unit
//}


class MathViewFX extends Pane {
  mathView =>
  import MathViewFX._

  val mathDoc = new MutableCMathMLDocument(CNone())

  def getInfoWithNewNode(cmml: MutableCMathML) = {
    if (cmml.node==null) cmml.node = new MathNode(cmml)
    cmml.addChangeListener(() => cmml.node.update())
    cmml
  }

  def deattachJFXNode(node:Node) = {
    val parent = node.parent.value
    if (parent!=null)
      parent.asInstanceOf[layout.Pane].getChildren.remove(node) // TODO: is there a better way?
  }


  def getNodeForEmbedding(requestingNode: MathNode, mathChild: MutableCMathML): Node = {
    val info = getInfoWithNewNode(mathChild)
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    deattachJFXNode(info.node) // TODO: is this needed?
    info.embeddedIn = requestingNode
    if (info.node.invalid) info.node.update()
    info.node
  }

  def disembed(node : MathNode, mathChild : MutableCMathML) : Unit = {
    val info = mathChild // getInfoWithNewNode(mathChild)
    if (info.embeddedIn==node) info.embeddedIn = null
  }

  def setRootNode(): Unit = {
    if (mathDoc.root.node==null) mathDoc.root.node = new MathNode(mathDoc.root)
    mathDoc.root.addChangeListener(() => mathDoc.root.node.update())
    if (mathDoc.root.embeddedIn != null) mathDoc.root.embeddedIn.invalid = true
    mathDoc.root.embeddedIn = null
    if (mathDoc.root.node.invalid) mathDoc.root.node.update()
//    children.setAll(mathDoc.root.node)
  }

  class MathNode(val math : MutableCMathML) extends Group {

    val size = ObjectProperty[Bounds](null : Bounds)
    size.onChange { (_, _, s) => }

    val embedded = new mutable.MutableList[MutableCMathML]

    def getNodeForEmbedding(mathChild: MutableCMathML): Node = {
      val node = mathView.getNodeForEmbedding(this, mathChild)
      embedded += mathChild
      node
    }

    var child: Node = null
    var invalid = true

    def update() = {
        for (n <- embedded) mathView.disembed(this, n)
        embedded.clear()
      invalid = false
      math match {
        case MApply(hd@MCSymbol("arith1", "times"), x, y) =>
          child = new BinOp("*",getNodeForEmbedding(x),getNodeForEmbedding(y))
        case MApply(hd@MCSymbol("arith1", "divide"), x, y) =>
          child = new Fraction(getNodeForEmbedding(x), getNodeForEmbedding(y))
        case MCNone() =>
          child = new Missing()
      }
      size <== child.boundsInLocal
      children.setAll(child)
    }
  }
}

object MathViewFX {
//  sealed trait CursorSide
//  final object CursorLeft extends CursorSide
//  final object CursorRight extends CursorSide
//  case class CursorPos(node:MutableCMathML, side:CursorSide)
}

//trait MathRendererContext {
//  def getNodeForEmbedding(math: MutableCMathML) : Node
//}

//trait MathRendererFactory {
//  def renderer(context:MathViewFX#MathNode, math:MutableCMathML) : Node
//}

