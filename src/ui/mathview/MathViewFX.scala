package ui.mathview

import java.io.IOException

import cmathml._

import scala.collection.mutable
import scalafx.scene.layout.{HBox, Pane}
import scalafx.scene.text.{Font, FontPosture, Text}
import scalafx.scene.{Group, Node}

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
  println(VariableFont)
  val SymbolFont = new Font("Symbola",fontSize)
  println(SymbolFont)
  def text(txt:String, font:Font) = { val t = new Text(txt); t.font = font; t }
  def variableText(txt:String) = text(txt,VariableFont)
  def symbolText(txt:String) = text(txt,SymbolFont)
}

class BinOp(op:String, a:Node, b:Node)
  extends HBox {
  import MathText._
  children.addAll(symbolText("("),a,symbolText(op),b,symbolText(")"))
}

class GenericApply(head:Node, args:Seq[Node])
  extends HBox {
  import MathText._
  children.addAll(head,symbolText("("))
  var first = true
  for (a <- args) {
    if (!first) children.add(symbolText(","))
    children.add(a)
  }
  children.add(symbolText(")"))
}

class Var(math:MCI) extends Text(math.v) {
  font = MathText.VariableFont
}

class GenericSymbol(cd:String, name:String) extends Text(s"$cd.$name") {
//  font = MathText.VariableFont
}

class Missing() extends Text("???") {
//  font = MathText.VariableFont
}



class MathViewFX extends Pane { mathView =>
  def setMath(m: CMathML) = mathDoc.setRoot(m)

  val mathDoc = new MutableCMathMLDocument(CNone())
  private case class Info(val node : MathNode, var ownedBy : MathNode = null, var embeddedIn : MathNode = null)
  private val infos = new mutable.WeakHashMap[MutableCMathML,Info] // Relies on the fact that MutableCMathML.equals is reference-equality

  styleClass += "mathview"

  private def cmmlChanged(info: Info): Unit = {
    if (info.ownedBy!=null)
      info.ownedBy.update()
    else if (info.node.invalid) ()
    else if (info.embeddedIn!=null)
      info.node.update()
    else if (info.node == mathDoc.root)
      info.node.update()
    else
      info.node.invalid = true
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
        case MApply(hd@MCSymbol("arith1", "times"), x, y) => binop(hd,"*",x,y)
        case MApply(hd@MCSymbol("arith1", "divide"), x, y) => binop(hd,"/",x,y)
        case MCNone() => setChild(new Missing())
        case MApply(hd, args @ _*) => setChild(new GenericApply(getNode(hd),args.map(getNode(_))))
        case MCSymbol(cd,name) => setChild(new GenericSymbol(cd,name))
      }
      assert(child!=null)
    }
  }

}
