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


/** Invariants:
  *
  * For every [[cmathml.MutableCMathML]] (short 'math') (not necessarily descendant of [[MathViewFX!.mathDoc mathDoc]].root) there is:
  * - potentially an rendering [[MathNode]]
  * - potentially an owning [[MathNode]]
  * - potentially an embedding [[MathNode]]
  *
  * No math can have both an owning and embedding [[MathNode]].
  *
  * A math with an embedding [[MathNode]] has an rendering [[MathNode]].
  *
  * An owning or embedding [[MathNode]] of math m is rendering an ancestor of m.
  *
  * If m is embedded or owned by n, then m is a child of a math owned or rendered by n.
  *
  * Every valid [[MathNode]] renders exactly one math. No [[MathNode]] renders more than one math.
  *
  * If a valid [[MathNode]] n1 contains [[MathNode]] n2 as a scene-graph descendant (not passing through other [[MathNode]]s),
  * then n2 is valid and n1 embeds the math rendered by n2.
  *
  * If a valid [[MathNode]]'s n rendering (excluding the rendering of its descendant [[MathNode]]s) depends on a math m, then
  * n renders or owns m.
  *
  * [[MathViewFX!.mathDoc mathDoc]].root is valid.
  *
  * (there's more...)
  */
class MathViewFX extends Pane {
  mathView =>
  import MathViewFX._

  private val mathRendererFactory : MathRendererFactory = DefaultMathRendererFactory

//  def getHighlights(math:MutableCMathML) =
//    getNode(math).get.highlights

  val mathDoc = new MutableCMathMLDocument(CNone())

  val infos = new mutable.WeakHashMap[MutableCMathML,Info] // Relies on the fact that MutableCMathML.equals is reference-equality
  styleClass += "mathview"


  def setMath(m: CMathML) =
    mathDoc.setRoot(m)

  def setMath(m: MutableCMathML) =
    mathDoc.setRoot(m)

  case class Info(node : MathNode, var ownedBy : MathNode = null, var embeddedIn : MathNode = null)

  private def cmmlChanged(info: Info): Unit = {
    if (info.ownedBy!=null)
      info.ownedBy.update() // TODO: why does this preserve invariants?
    else if (info.node.invalid) ()
    else if (info.embeddedIn!=null)
      info.node.update() // TODO: why does this preserve invariants?
    else if (info.node.math == mathDoc.root)
      info.node.update() // TODO: why does this preserve invariants?
    else
      info.node.invalid = true // TODO: why does this preserve invariants?
  }

  /** This will create a node (which will then own and embed other nodes)! */
  private def getInfoWithNewNode(cmml: MutableCMathML) = infos.getOrElseUpdate(cmml, {
    val info = Info(node = new MathNode(cmml))
    cmml.addChangeListener(() => cmmlChanged(info))
    info
  })

  private def own(node: MathNode, mathChild: MutableCMathML) : Unit = {
//    println(s"own($node,$mathChild)")
    assert(node != null)
    assert(node.math ne mathChild)
    val info = getInfoWithNewNode(mathChild)
    assert(info.embeddedIn ne node)
    assert(info.ownedBy ne node)
    if (info.ownedBy != null) info.ownedBy.invalid = true
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    info.ownedBy = node
    info.embeddedIn = null
  }
  /** It is permissible to call [[disown]] if you 'node' is not the owner. In this case, nothing happens. */
  private def disown(node : MathNode, mathChild : MutableCMathML) : Unit = {
    val info = getInfoWithNewNode(mathChild)
    if (info.ownedBy==node) info.ownedBy = null
  }

  private def getNode(node : MutableCMathML) = {
    infos.get(node) match {
      case None => None
      case Some(info) => assert(info.node!=null); Some(info.node)
    }
  }

  private def deattachJFXNode(node:Node) = {
    val parent = node.parent.value
    if (parent!=null)
      parent.asInstanceOf[layout.Pane].getChildren.remove(node) // TODO: is there a better way?
  }


  private def getNodeForEmbedding(requestingNode: MathNode, mathChild: MutableCMathML): Node = {
    val info = getInfoWithNewNode(mathChild)
    assert(info.embeddedIn ne requestingNode)
    assert(info.ownedBy ne requestingNode)
    if (info.ownedBy != null) info.ownedBy.invalid = true
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    deattachJFXNode(info.node) // TODO: is this needed?
    info.ownedBy = null
    info.embeddedIn = requestingNode
    if (info.node.invalid) info.node.update()
    info.node
  }
  private def getNodeForRoot() : MathNode = {
    val info = getInfoWithNewNode(mathDoc.root)
    if (info.ownedBy != null) info.ownedBy.invalid = true
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    info.ownedBy = null
    info.embeddedIn = null
    if (info.node.invalid) info.node.update()
    info.node
  }
  /** It is permissible to call [[disembed]] if you 'node' is not the embedder. In this case, nothing happens. */
  private def disembed(node : MathNode, mathChild : MutableCMathML) : Unit = {
    val info = getInfoWithNewNode(mathChild)
    if (info.embeddedIn==node) info.embeddedIn = null
  }

  private def setRootNode(): Unit = {
    val rootNode = getNodeForRoot()
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
    override def own(mathChild: MutableCMathML) = {
      owned += mathChild
      mathView.own(this, mathChild)
    }

    private val owned = new mutable.MutableList[MutableCMathML]
    private val embedded = new mutable.MutableList[MutableCMathML]

    /** Gets a [[Node]] containing the rendering of 'mathChild'.
      * These nodes can be added as children to 'this' (or its descendants) but must not otherwise be touched.
      */
    override def getNodeForEmbedding(mathChild: MutableCMathML): Node = {
      val node = mathView.getNodeForEmbedding(this, mathChild)
      embedded += mathChild
      node
    }

    private var child: Node = null
    var invalid = true

    private def setChild(n: Node) =
      if (child != n) {
        child = n
        size <== child.boundsInLocal
        updateChildren()
      }

    private def updateChildren() : Unit = {
      var cs = List(child : javafx.scene.Node)
      for (h <- highlights) cs = h::cs
      children.setAll(cs : _*)
    }

    private def disownAll() = {
      for (n <- owned) mathView.disown(this,n)
      owned.clear()
    }

    private def disembedAll() = {
      for (n <- embedded) mathView.disembed(this,n)
      embedded.clear()
    }

    def update() = {
//      println("update",this)
      disownAll()
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
  def own(math: MutableCMathML) : Unit
  def getNodeForEmbedding(math: MutableCMathML) : Node
}

trait MathRendererFactory {
  def renderer(context:MathRendererContext, math:MutableCMathML) : Node
}

