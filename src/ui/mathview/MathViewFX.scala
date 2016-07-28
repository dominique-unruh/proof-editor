package ui.mathview

import java.io.IOException

import cmathml._

import scala.collection.mutable
import scalafx.beans.binding.Bindings
import scalafx.geometry.{Orientation, Pos}
import scalafx.scene.control.Separator
import scalafx.scene.layout.{HBox, Pane, VBox}
import scalafx.scene.text.{Font, FontPosture, Text}
import scalafx.scene.{Group, Node}
import scalafx.Includes._
import scalafx.scene.shape.Line

//trait MathNode extends Node {
//  val math : MutableCMathML
//  val mathView : MathViewFX
//}

object MathText {
  if (Font.loadFont(getClass.getResource("mathquill/font/Symbola.otf").toString,0)==null)
    throw new IOException("Cannot load Symbola font")

  val fontSize = 28.8 : Double
//  for (f <- Font.fontNames) println(f)
  val VariableFont = Font("FreeSerif",FontPosture.Italic,fontSize)
//  println(VariableFont)
  val SymbolFont = symbolFont(fontSize)
  def symbolFont(size:Double) = new Font("Symbola",size)
  println(SymbolFont)
  def text(txt:String, font:Font) = { val t = new Text(txt); t.font = font; t }
  def variableText(txt:String) = text(txt,VariableFont)
  def symbolText(txt:String) = text(txt,SymbolFont)
}

class BinOp(op:String, a:Node, b:Node) extends HBox {
  import MathText._
  alignment = Pos.Center
  val open = symbolText("(")
  val close = symbolText(")")
  val opTxt = symbolText(op)

  val innerHeight = Bindings.createDoubleBinding(
    () => math.max(opTxt.layoutBounds.get.getHeight, math.max(a.layoutBounds.get.getHeight,b.layoutBounds.get.getHeight)),
    opTxt.layoutBounds, a.layoutBounds, b.layoutBounds)

  def updateParens() = {
    val h = innerHeight.get
    val font = symbolFont(h)
    open.font = font
    close.font = font
    println("height",h)
  }

  innerHeight.onChange(updateParens())
  updateParens()

  children.addAll(open,a,opTxt,b,close)
}

class Fraction(a:Node, b:Node) extends VBox {
  alignment = Pos.Center
  val line = new Line()
  children.addAll(a, line, b)

  line.startX = 0
  line.endX <== Bindings.createDoubleBinding(
    () => math.max(a.layoutBounds.get.getWidth, b.layoutBounds.get.getWidth) + 6,
    a.layoutBounds, b.layoutBounds)
  line.strokeWidth = 2
}

class GenericApply(head:Node, args:Seq[Node])
  extends HBox {
  import MathText._
  alignment = Pos.Center
  children.addAll(head,symbolText("("))
  var first = true
  for (a <- args) {
    if (!first) children.add(symbolText(","))
    children.add(a)
    first = false
  }
  children.add(symbolText(")"))
}

class Var(math:MCI) extends Text(math.name) {
  font = MathText.VariableFont
}

class GenericSymbol(cd:String, name:String) extends Text(s"$cd.$name") {
//  font = MathText.VariableFont
}

class Missing() extends Text("\u2603") {
  font = MathText.SymbolFont
}


/** Invariants:
  *
  * For every [[MutableCMathML]] (short 'math') (not necessarily descendant of [[mathDoc]].root) there is:
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
  * [[mathDoc]].root is valid.
  *
  * (there's more...)
  */
class MathViewFX extends Pane { mathView =>
  def setMath(m: CMathML) = mathDoc.setRoot(m)

  val mathDoc = new MutableCMathMLDocument(CNone())
  private case class Info(val node : MathNode, var ownedBy : MathNode = null, var embeddedIn : MathNode = null)
  private val infos = new mutable.WeakHashMap[MutableCMathML,Info] // Relies on the fact that MutableCMathML.equals is reference-equality

  styleClass += "mathview"

  private def cmmlChanged(info: Info): Unit = {
    if (info.ownedBy!=null)
      info.ownedBy.update() // TODO: why does this preserve invariants?
    else if (info.node.invalid) ()
    else if (info.embeddedIn!=null)
      info.node.update() // TODO: why does this preserve invariants?
    else if (info.node == mathDoc.root)
      info.node.update() // TODO: why does this preserve invariants?
    else
      info.node.invalid = true // TODO: why does this preserve invariants?
  }

  private def getInfo(cmml: MutableCMathML) =infos.getOrElseUpdate(cmml, {
    val info = Info(node = new MathNode(cmml))
    cmml.addChangeListener(() => cmmlChanged(info))
    info
  })

  private def own(node: MathNode, mathChild: MutableCMathML) : Unit = {
    println(s"own(${node},${mathChild})")
    assert(node != null)
    assert(node.math ne mathChild)
    val info = getInfo(mathChild)
    assert(info.embeddedIn ne node)
    assert(info.ownedBy ne node)
    if (info.ownedBy != null) info.ownedBy.invalid = true
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    info.ownedBy = node
    info.embeddedIn = null
  }
  /** It is permissible to call [[disown]] if you 'node' is not the owner. In this case, nothing happens. */
  private def disown(node : MathNode, mathChild : MutableCMathML) : Unit = {
    val info = getInfo(mathChild)
    if (info.ownedBy==node) info.ownedBy = null
  }

  private def getNode(node: MathNode, mathChild: MutableCMathML): Node = {
    val info = getInfo(mathChild)
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
    val info = getInfo(mathDoc.root)
    if (info.ownedBy != null) info.ownedBy.invalid = true
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    info.ownedBy = null
    info.embeddedIn = null
    if (info.node.invalid) info.node.update()
    info.node
  }
  /** It is permissible to call [[disembed]] if you 'node' is not the embedder. In this case, nothing happens. */
  private def disembed(node : MathNode, mathChild : MutableCMathML) : Unit = {
    val info = getInfo(mathChild)
    if (info.embeddedIn==node) info.embeddedIn = null
  }

  private def setRootNode(): Unit = {
    val rootNode = getNodeForRoot()
    rootNode.printInfo()
    children.setAll(rootNode)
  }

  setRootNode()
  mathDoc.addChangeListener(() => setRootNode())

  private class MathNode(val math : MutableCMathML) extends Group {
    def printInfo() = {
      println(this)
      println("CMML: "+math.toCMathML.toString)
      println("Child: "+child)
      println("Invalid: "+invalid)
      println("Owned: "+owned)
      println("Embedded: "+embedded)
      println("Info: "+getInfo(math))
    }

    override def toString(): String = s"[MathNode: ${math}]"

    /** Rules:
      * - If [[getNode]] is called for some proper descendent x of [[math]], then [[own]](x) must be called before.
      * - If [[own]] is called for some proper descendent x of [[math]], then [[own]](x) must be called before.
      * - [[own]] must not be called for [[math]], and must only be called for descendents of [[math]]
      * - [[own]] must be called for all [[MutableCMathML]]'s that this node logically depends on (except for children that are simply embedded as subnodes)
      * - 'this' must not register itself as a listener for [[math]] or any of its descendants
      */
    protected def own(mathChild: MutableCMathML) = {
      owned += mathChild
      mathView.own(this, mathChild)
    }

    private val owned = new mutable.MutableList[MutableCMathML]
    private val embedded = new mutable.MutableList[MutableCMathML]

    /** Gets a [[Node]] containing the rendering of 'mathChild'.
      * These nodes can be added as children to 'this' (or its descendants) but must not otherwise be touched.
      */
    private def getNode(mathChild: MutableCMathML): Node = {
      val node = mathView.getNode(this, mathChild)
      embedded += mathChild
      node
    }

    private var child: Node = null
    var invalid = true

    private def setChild(n: Node) =
      if (child != n) {
        child = n
        children.setAll(n)
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
      println("update",this,math.toCMathML)
      disownAll()
      disembedAll()
      invalid = false
      def binop(hd:MutableCMathML,op:String,x:MutableCMathML,y:MutableCMathML) = {
        own(hd)
        setChild(new BinOp(op, getNode(x), getNode(y)))
      }
      math match {
        case m: MCI => setChild(new Var(m))
        case MApply(hd@MCSymbol("arith1", "plus"), x, y) => binop(hd,"+",x,y)
        case MApply(hd@MCSymbol("arith1", "minus"), x, y) => binop(hd,"-",x,y)
        case MApply(hd@MCSymbol("arith1", "times"), x, y) => binop(hd,"\u22c5",x,y)
        case MApply(hd@MCSymbol("arith1", "divide"), x, y) => { own(hd); setChild(new Fraction(getNode(x),getNode(y))) }
        case MCNone() => setChild(new Missing())
        case MApply(hd, args @ _*) => setChild(new GenericApply(getNode(hd),args.map(getNode(_))))
        case MCSymbol(cd,name) => setChild(new GenericSymbol(cd,name))
      }
      assert(child!=null)
    }
  }

}
