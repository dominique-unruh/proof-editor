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

trait MathHighlight extends Node {
  def setSize(size:Bounds) : Unit
}


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

  def own(node: MathNode, mathChild: MutableCMathML) : Unit = {
////    println(s"own($node,$mathChild)")
//    assert(node != null)
//    assert(node.math ne mathChild)
//    val info = getInfoWithNewNode(mathChild)
//    assert(info.embeddedIn ne node)
////    assert(info.ownedBy ne node)
////    if (info.ownedBy != null) info.ownedBy.invalid = true
//    if (info.embeddedIn != null) info.embeddedIn.invalid = true
////    info.ownedBy = node
//    info.embeddedIn = null
  }
  /** It is permissible to call [[disown]] if you 'node' is not the owner. In this case, nothing happens. */
//  def disown(node : MathNode, mathChild : MutableCMathML) : Unit = {
////    val info = getInfoWithNewNode(mathChild)
////    if (info.ownedBy==node) info.ownedBy = null
//  }

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
      //    if (info.ownedBy != null) info.ownedBy.invalid = true
      if (info.embeddedIn != null) info.embeddedIn.invalid = true
      //    info.ownedBy = null
      info.embeddedIn = null
      if (info.node.invalid) info.node.update()
      info.node
    }
    rootNode.printInfo()
    children.setAll(rootNode)
  }

  setRootNode()
  mathDoc.addChangeListener(() => setRootNode())

  class MathNode(val math : MutableCMathML) extends Group with MathRendererContext {
    def rightmostChild: Option[MutableCMathML] = embedded.lastOption
    def leftmostChild: Option[MutableCMathML] = embedded.headOption

    def leftOf(node: MathNode): Option[MutableCMathML] =
      embedded.indexOf(node.math) match {
        case -1 => throw new IllegalArgumentException("node is not embedded")
        case 0 => None
        case i if i>=1 => Some(embedded(i-1))
        //        case _ => sys.error("unreachable")
      }
    def rightOf(node: MathNode): Option[MutableCMathML] = {
      val lastIdx = embedded.length-1
      embedded.indexOf(node.math) match {
        case -1 => throw new IllegalArgumentException("node is not embedded")
        case `lastIdx` => None
        case i => Some(embedded(i + 1))
      }
    }

    id = Integer.toHexString(hashCode) // TODO: remove

    val highlights = new ObservableBuffer[MathHighlight]
    highlights.onChange { (_,changes) =>
      var needsUpdate = false
      for (c <- changes) c match {
        case Add(_,added) =>
          for (h <- added) h.setSize(size.value)
          needsUpdate = true
        case Remove(_,_) =>
          needsUpdate = true
        case Reorder(_,_,_) =>
        case Update(from,to) =>
          for (i <- from until to)
            highlights(i).setSize(size.value)
          needsUpdate = true
      }
      if (needsUpdate)
        updateChildren()
    }


    /*
//    @deprecated(null,null) private var _cursor : MathHighlight = null
    @deprecated("Access highlights directly",null) def setCursor(state:Option[CursorSide]) : Unit = {
      highlights.removeIf { (_:MathHighlight).isInstanceOf[MathCursor] }
//      state match {
//        case None => _cursor = null
//        case Some(side) => _cursor = new MathCursor(side); highlights += _cursor //; _cursor.setSize(size.value)
//      }
      for (side <- state) highlights += new MathCursor(side)
//      updateChildren()
    }
    */


    val size = ObjectProperty[Bounds](null : Bounds)
    size.onChange { (_, _, s) =>
      for (h <- highlights) h.setSize(s)
    }

    def printInfo() = {
//      println(this)
//      println("CMML: "+math.toCMathML.toString)
//      println("Child: "+child)
//      println("Invalid: "+invalid)
//      println("Owned: "+owned)
//      println("Embedded: "+embedded)
//      println("Info: "+getInfoWithNewNode(math))
    }

    override def toString(): String = s"[MathNode: ${math}]"

    /** Rules:
      * - If [[getNodeForEmbedding]] is called for some proper descendent x of [[math]], then [[own]](x) must be called before.
      * - If [[own]] is called for some proper descendent x of [[math]], then [[own]](x) must be called before.
      * - [[own]] must not be called for [[math]], and must only be called for descendents of [[math]]
      * - [[own]] must be called for all [[MutableCMathML]]'s that this node logically depends on (except for children that are simply embedded as subnodes)
      * - 'this' must not register itself as a listener for [[math]] or any of its descendants
      */
//    def own(mathChild: MutableCMathML) = {
//      owned += mathChild
//      mathView.own(this, mathChild)
//    }

    val owned = new mutable.MutableList[MutableCMathML]
    val embedded = new mutable.MutableList[MutableCMathML]

    /** Gets a [[Node]] containing the rendering of 'mathChild'.
      * These nodes can be added as children to 'this' (or its descendants) but must not otherwise be touched.
      */
    override def getNodeForEmbedding(mathChild: MutableCMathML): Node = {
      val node = mathView.getNodeForEmbedding(this, mathChild)
      embedded += mathChild
      node
    }

    var child: Node = null
    var invalid = true

    def setChild(n: Node) =
      if (child != n) {
        child = n
        size <== child.boundsInLocal
        updateChildren()
      }

    def updateChildren() : Unit = {
      var cs = List(child : javafx.scene.Node)
      for (h <- highlights) cs = h::cs
      children.setAll(cs : _*)
    }

    def disownAllX() = {
//      for (n <- owned) mathView.disown(this,n)
      owned.clear()
    }

    def disembedAll() = {
      for (n <- embedded) mathView.disembed(this,n)
      embedded.clear()
    }

    def update() = {
      disembedAll()
      invalid = false
      val child = mathRendererFactory.renderer(this,math)
      assert(child!=null)
      setChild(child)
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

