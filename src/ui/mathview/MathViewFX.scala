package ui.mathview

import javafx.collections.ObservableList
import javafx.geometry.Bounds

import cmathml._
import ui.mathview.MathViewFX.CursorSide

import scala.collection.mutable
import scala.util.control.Breaks._
import scalafx.Includes._
import scalafx.beans.property.{ObjectProperty, Property}
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer.{Add, Remove, Reorder, Update}
import scalafx.scene.layout._
import scalafx.scene.{Group, Node}

import misc.Utils.ImplicitConversions._

trait MathHighlight extends Node {
  def setSize(size:Bounds) : Unit
}


private class MathCursor(val cursorSide: CursorSide) extends Region with MathHighlight {
  import MathViewFX._
  id = Integer.toHexString(hashCode) // TODO: remove
  styleClass += "mathCursor"
  cursorSide match {
    case CursorLeft => styleClass += "mathCursorLeft"
    case CursorRight => styleClass += "mathCursorRight"
//    case CursorSelect => styleClass += "mathCursorSelect"
  }
  override def setSize(size:Bounds) = {
    layoutX = size.minX
    layoutY = size.minY
    prefWidth = size.width
    prefHeight = size.height
    resize(size.width,size.height)
  }
//  def updateSize(): Unit = {
//    val _size = size.value
//    println("size",_size)
//    layoutX = _size.minX
//    layoutY = _size.minY
//    prefWidth = _size.width
//    prefHeight = _size.height
//    resize(_size.width,_size.height)
////    x = _size.minX
////    y = _size.minY
////    width = _size.width
////    height = _size.height
//  }
//  size.onChange(updateSize()) // TODO: this causes MathCursor not to be GC'd
//  updateSize()
//  fill = Color.LightGray
}

private class MathSelection extends Region with MathHighlight {
  id = Integer.toHexString(hashCode) // TODO: remove
  styleClass += "mathSelection"
  override def setSize(size:Bounds) = {
    layoutX = size.minX
    layoutY = size.minY
    prefWidth = size.width
    prefHeight = size.height
    resize(size.width,size.height)
  }
//  def updateSize(): Unit = {
//    val _size = size.value
//    println("size",_size)
//    layoutX = _size.minX
//    layoutY = _size.minY
//    prefWidth = _size.width
//    prefHeight = _size.height
//    resize(_size.width,_size.height)
//  }
//  size.onChange(updateSize()) // TODO: this causes MathSelection not to be GC'd
//  updateSize()
}


/** Invariants:
  *
  * For every [[MutableCMathML]] (short 'math') (not necessarily descendant of [[MathViewFX!.mathDoc mathDoc]].root) there is:
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

  def getHighlights(math:MutableCMathML) =
    getNode(math).get.highlights

  val mathDoc = new MutableCMathMLDocument(CNone())
  @deprecated(null,null) val cursorPos = ObjectProperty[CursorPos](CursorPos(mathDoc.root, CursorLeft))
  cursorPos.onChange { (_, oldPos, newPos) =>
//    if (oldPos.node ne newPos.node) {
      val oldHighlights = getHighlights(oldPos.node)
      oldHighlights.removeIf((_: MathHighlight).isInstanceOf[MathCursor])
//    }
    val newHighlights = getHighlights(newPos.node)
//    newHighlights.removeIf((_: MathHighlight).isInstanceOf[MathCursor])
    newHighlights += new MathCursor(newPos.side)
  }

  @deprecated(null,null) val selection = ObjectProperty[Option[MutableCMathML]](None)
  selection.onChange { (_, oldSel, newSel) =>
    assert(oldSel!=newSel)
    if (oldSel != newSel) {
      if (oldSel.isDefined) {
        val oldHighlights = getHighlights(oldSel.get)
        oldHighlights.removeIf((_: MathHighlight).isInstanceOf[MathSelection])
        //        getNode(oldSel.get).get.setSelection(false)
      }
      if (newSel.isDefined) {
        val newHighlights = getHighlights(newSel.get)
        newHighlights += new MathSelection()
//        getNode(newSel.get).get.setSelection(true)
      }
    }
  }

  /*TODO private */val infos = new mutable.WeakHashMap[MutableCMathML,Info] // Relies on the fact that MutableCMathML.equals is reference-equality
  styleClass += "mathview"


  def setMath(m: CMathML) = {
    mathDoc.setRoot(m)
    //    cursorNode.value = mathDoc.root; cursorSide.value = CursorLeft
    cursorPos.value = CursorPos(mathDoc.root, CursorLeft)
  }


  /*TODO private */case class Info(node : MathNode, var ownedBy : MathNode = null, var embeddedIn : MathNode = null)

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
    println(s"own($node,$mathChild)")
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

  private def getNodeForEmbedding(node: MathNode, mathChild: MutableCMathML): Node = {
    val info = getInfoWithNewNode(mathChild)
    assert(info.embeddedIn ne node)
    assert(info.ownedBy ne node)
    if (info.ownedBy != null) info.ownedBy.invalid = true
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    info.ownedBy = null
    info.embeddedIn = node
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

  def leftOf(cursor:CursorPos, jump:Boolean=false) : Option[CursorPos] = {
    val node = getNode(cursor.node).getOrElse(throw new IllegalArgumentException("cursor points to a non-visual subterm"))
    cursor.side match {
      case CursorLeft /*| CursorSelect*/ => leftOf(node) match {
        case None => embedderOf(node) match {
          case None => None
          case Some(embedder) => Some(CursorPos(embedder.math, CursorLeft))
        }
        case Some(left) => Some(CursorPos(left,CursorRight))
      }
      case CursorRight =>
        if (jump)
          Some(CursorPos(node.math,CursorLeft))
        else
          node.rightmostChild match {
            case None => Some(CursorPos(node.math,CursorLeft))
            case Some(child) => Some(CursorPos(child,CursorRight))
      }
    }
  }
  def rightOf(cursor:CursorPos, jump:Boolean=false) : Option[CursorPos] = {
    val node = getNode(cursor.node).getOrElse(throw new IllegalArgumentException("cursor points to a non-visual subterm"))
    cursor.side match {
      case CursorRight /*| CursorSelect */ => rightOf(node) match {
        case None => embedderOf(node) match {
          case None => None
          case Some(embedder) => Some(CursorPos(embedder.math, CursorRight))
        }
        case Some(right) => Some(CursorPos(right,CursorLeft))
      }
      case CursorLeft =>
        if (jump)
          Some(CursorPos(node.math,CursorRight))
        else
          node.leftmostChild match {
            case None => Some(CursorPos(node.math,CursorRight))
            case Some(child) => Some(CursorPos(child,CursorLeft))
          }
    }
  }

  private def embeddingPath(m:MutableCMathML) : List[MutableCMathML] = {
    var tmp = getNode(m)
    assert(tmp.isDefined)
    var path = Nil : List[MutableCMathML]
    while (tmp.isDefined) {
      path = tmp.get.math::path
      tmp = embedderOf(tmp.get)
    }
    path
  }

  /** Returns the smallest rendered math that contains a and b */
  def encompassingNode(a:MutableCMathML, b:MutableCMathML) : MutableCMathML = {
    var result : MutableCMathML = null
    val itA = embeddingPath(a).iterator
    val itB = embeddingPath(b).iterator
    breakable {
      while (itA.hasNext && itB.hasNext) {
        val a$ = itA.next()
        val b$ = itB.next()
        if (a$ eq b$)
          result = a$
        else
          break
      }
    }
    assert(result!=null)
    result
  }

  private def embedderOf(node:MathNode) : Option[MathNode] =
    infos.get(node.math).flatMap(x => Option(x.embeddedIn))
  /** Left sibling according to visual representation */
  private def leftOf(node:MathNode) : Option[MutableCMathML] =
  embedderOf(node).flatMap(_.leftOf(node))
  private def rightOf(node:MathNode) : Option[MutableCMathML] =
    embedderOf(node).flatMap(_.rightOf(node))


  setRootNode()
  mathDoc.addChangeListener(() => setRootNode())

  /*TODO private*/ class MathNode(val math : MutableCMathML) extends Group with MathRendererContext {
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

//    @deprecated(null,null) private var _selection : MathHighlight = null
    @deprecated("Access highlights directly",null)
    def setSelection(state:Boolean) : Unit = {
      highlights.removeIf { (_:MathHighlight).isInstanceOf[MathSelection] }
      if(state)
        highlights += new MathSelection
//      highlights.remove(_selection)
//      if (state) {
//        _selection = new MathSelection
//        highlights += _selection
////        _selection.setSize(size.value)
//      } else
//        _selection = null
////      _selection = if (state) new MathSelection() else null
////      updateChildren()
    }


    val size = ObjectProperty[Bounds](null : Bounds)
    size.onChange { (_, _, s) =>
      for (h <- highlights) h.setSize(s)
//      if (_cursor!=null) _cursor.setSize(s)
//      if (_selection!=null) _selection.setSize(s)
    }

    def printInfo() = {
      println(this)
      println("CMML: "+math.toCMathML.toString)
      println("Child: "+child)
      println("Invalid: "+invalid)
      println("Owned: "+owned)
      println("Embedded: "+embedded)
      println("Info: "+getInfoWithNewNode(math))
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
//      if (_selection!=null) cs = _selection::cs
//      if (_cursor!=null) cs = _cursor::cs
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
      println("update",this)
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
