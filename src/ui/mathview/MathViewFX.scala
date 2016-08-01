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

  private val mathRendererFactory : MathRendererFactory = DefaultMathRendererFactory

//  def getHighlights(math:MutableCMathML) =
//    getNode(math).get.highlights

  val mathDoc = new MutableCMathMLDocument(CNone())

  val infos = new mutable.WeakHashMap[MutableCMathML,Info] // Relies on the fact that MutableCMathML.equals is reference-equality

  def setMath(m: CMathML) =
    mathDoc.setRoot(m)

  def setMath(m: MutableCMathML) =
    mathDoc.setRoot(m)

  case class Info(node : MathNode, /*var ownedBy : MathNode = null, */var embeddedIn : MathNode = null)

  def cmmlChanged(info: Info): Unit = {
//    if (info.ownedBy!=null)
//      info.ownedBy.update() // TODO: why does this preserve invariants?
    if (info.node.invalid) ()
    else if (info.embeddedIn!=null)
      info.node.update() // TODO: why does this preserve invariants?
    else if (info.node.math == mathDoc.root)
      info.node.update() // TODO: why does this preserve invariants?
    else
      info.node.invalid = true // TODO: why does this preserve invariants?
  }

  /** This will create a node (which will then own and embed other nodes)! */
  def getInfoWithNewNode(cmml: MutableCMathML) = infos.getOrElseUpdate(cmml, {
    val info = Info(node = new MathNode(cmml))
    cmml.addChangeListener(() => cmmlChanged(info))
    info
  })


  def getNode(node : MutableCMathML) = {
    infos.get(node) match {
      case None => None
      case Some(info) => assert(info.node!=null); Some(info.node)
    }
  }

  def deattachJFXNode(node:Node) = {
    val parent = node.parent.value
    if (parent!=null)
      parent.asInstanceOf[layout.Pane].getChildren.remove(node) // TODO: is there a better way?
  }


  def getNodeForEmbedding(requestingNode: MathNode, mathChild: MutableCMathML): Node = {
    val info = getInfoWithNewNode(mathChild)
    assert(info.embeddedIn ne requestingNode)
//    assert(info.ownedBy ne requestingNode)
//    if (info.ownedBy != null) info.ownedBy.invalid = true
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    deattachJFXNode(info.node) // TODO: is this needed?
//    info.ownedBy = null
    info.embeddedIn = requestingNode
    if (info.node.invalid) info.node.update()
    info.node
  }
  /** It is permissible to call [[disembed]] if you 'node' is not the embedder. In this case, nothing happens. */
  def disembed(node : MathNode, mathChild : MutableCMathML) : Unit = {
    val info = getInfoWithNewNode(mathChild)
    if (info.embeddedIn==node) info.embeddedIn = null
  }

  def setRootNode(): Unit = {
    val rootNode = {
      val info = getInfoWithNewNode(mathDoc.root)
      if (info.embeddedIn != null) info.embeddedIn.invalid = true
      info.embeddedIn = null
      if (info.node.invalid) info.node.update()
      info.node
    }
//    rootNode.printInfo()
    children.setAll(rootNode)
  }

  setRootNode()
  mathDoc.addChangeListener(() => setRootNode())

  class MathNode(val math : MutableCMathML) extends Group with MathRendererContext {
//    def rightmostChild: Option[MutableCMathML] = embedded.lastOption
//    def leftmostChild: Option[MutableCMathML] = embedded.headOption

    val size = ObjectProperty[Bounds](null : Bounds)
    size.onChange { (_, _, s) => }

    override def toString(): String = s"[MathNode: ${math}]"


    val embedded = new mutable.MutableList[MutableCMathML]

    override def getNodeForEmbedding(mathChild: MutableCMathML): Node = {
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
  sealed trait CursorSide
  final object CursorLeft extends CursorSide
  final object CursorRight extends CursorSide
  case class CursorPos(node:MutableCMathML, side:CursorSide)
}

trait MathRendererContext {
  def getNodeForEmbedding(math: MutableCMathML) : Node
}

trait MathRendererFactory {
  def renderer(context:MathViewFX#MathNode, math:MutableCMathML) : Node
}

