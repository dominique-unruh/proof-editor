package ui.mathview

import java.io.IOException
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.geometry
import javafx.geometry.Bounds

import cmathml._

import scalafx.beans.binding.Bindings
import scalafx.geometry.Pos
import scalafx.scene.Node
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.shape.Line
import scalafx.scene.text.{Font, FontPosture, Text}
import scalafx.Includes._
import scalafx.application.Platform

object DefaultMathRendererFactory extends MathRendererFactory {
  override def renderer(context: MathRendererContext, math: MutableCMathML): Node = {
    def own(math:MutableCMathML) = context.own(math)
    def get(math:MutableCMathML) = context.getNodeForEmbedding(math)
    def binop(hd: MutableCMathML, op: String, x: MutableCMathML, y: MutableCMathML) = {
      own(hd)
      val x$ = get(x)
      val y$ = get(y)
      new BinOp(op, x$, y$)
    }
    def prefixop(hd: MutableCMathML, op: String, x: MutableCMathML) = {
      own(hd)
      new PrefixOp(op, get(x))
    }

    math match {
      case m: MCI => new Var(m)
      case MApply(hd@MCSymbol("relation1", "eq"), x, y) => binop(hd, "=", x, y)
      case MApply(hd@MCSymbol("arith1", "unary_minus"), x) => prefixop(hd, "\u2212", x)
      case MApply(hd@MCSymbol("arith1", "plus"), x, y) => binop(hd, "+", x, y)
      case MApply(hd@MCSymbol("arith1", "minus"), x, y) => binop(hd, "\u2212", x, y)
      case MApply(hd@MCSymbol("arith1", "times"), x, y) => binop(hd, "\u22c5", x, y)
      case MApply(hd@MCSymbol("arith1", "divide"), x, y) => {
        own(hd); (new Fraction(get(x), get(y)))
      }
      case MCNone() => new Missing()
      case MApply(hd, args@_*) => (new GenericApply(get(hd), args.map(get(_))))
      case MCSymbol(cd, name) => (new GenericSymbol(cd, name))
      case m @ MCN(num) => new Num(m)
    }
  }
}


object MathText {
  if (Font.loadFont(getClass.getResource("mathquill/font/Symbola.otf").toString,0)==null)
    throw new IOException("Cannot load Symbola font")

  val fontSize = 22 : Double
  //  for (f <- Font.fontNames) println(f)
  val VariableFont = Font("FreeSerif",FontPosture.Italic,fontSize)
  //  println(VariableFont)
  val SymbolFont = symbolFont(fontSize)
  def symbolFont(size:Double) = new Font("Symbola",size)
  val NumberFont = SymbolFont
  def text(txt:String, font:Font) = { val t = new Text(txt); t.font = font; t }
  def variableText(txt:String) = text(txt,VariableFont)
  def symbolText(txt:String) = text(txt,SymbolFont)
}

class BinOp(op:String, a:javafx.scene.Node, b:javafx.scene.Node) extends javafx.scene.layout.HBox {
  import MathText._
  setId(Integer.toHexString(hashCode())) // TODO: remove
  setAlignment(geometry.Pos.CENTER)
  val open = symbolText("(")
  val close = symbolText(")")
  val opTxt = symbolText(op)

  setFillHeight(false)

  override def layoutChildren(): Unit = {
    val h = math.max(opTxt.prefHeight(-1),math.max(a.prefHeight(-1),b.prefHeight(-1)))
    val font = symbolFont(h)
    open.font = font
    close.font = font
    super.layoutChildren()
  }

//  val innerHeight = Bindings.createDoubleBinding(
//    () => math.max(opTxt.layoutBounds.get.getHeight, math.max(a.layoutBounds.get.getHeight,b.layoutBounds.get.getHeight)),
//    opTxt.layoutBounds, a.layoutBounds, b.layoutBounds)
//
//  def updateParens() = {
//    val h = innerHeight.get
//    val font = symbolFont(h)
//    open.font = font
//    close.font = font
//    println("height",h,getParent, a, a.layoutBounds.get.getHeight, b, b.layoutBounds.get.getHeight)
//  }
//
//  innerHeight.onChange(updateParens()) // Delayed action. I think otherwise we end up changing bounds within a recomputation of bounds, leading to spurious errors?
//  updateParens()

  getChildren.addAll(open,a,opTxt,b,close)
}

class PrefixOp(op:String, a:Node) extends HBox {
  import MathText._
  id = Integer.toHexString(hashCode()) // TODO: remove
  alignment = Pos.Center
  val open = symbolText("(")
  val close = symbolText(")")
  val opTxt = symbolText(op)

  val innerHeight = Bindings.createDoubleBinding(
    () => math.max(opTxt.layoutBounds.get.getHeight, a.layoutBounds.get.getHeight),
    opTxt.layoutBounds, a.layoutBounds)

  def updateParens() = {
    val h = innerHeight.get
    val font = symbolFont(h)
    open.font = font
    close.font = font
    println("height",h)
  }

  innerHeight.onChange(updateParens())
  updateParens()

  children.addAll(open,opTxt,a,close)
}


class Fraction(a:Node, b:Node) extends javafx.scene.layout.VBox {
  setId(Integer.toHexString(hashCode())) // TODO: remove
  setAlignment(Pos.Center)
  val line = new Line()
  getChildren.addAll(a, line, b)

  line.startX = 0
  line.strokeWidth = 2

  override def layoutChildren(): Unit = {
    line.endX = math.max(a.prefWidth(-1), b.prefHeight(-1)) + 6
    super.layoutChildren()
  }

//  @inline def updateLine() =
//    line.endX = math.max(a.layoutBounds.get.getWidth, b.layoutBounds.get.getWidth) + 6
//  private val updateLineCL = new ChangeListener[Bounds] {
//    override def changed(observable: ObservableValue[_ <: Bounds], oldValue: Bounds, newValue: Bounds): Unit =
//      updateLine()
//  }
//  // TODO: this Fraction class cannot be garbage collected until a and b are garbage collected. We should remove the change listener
//  a.layoutBounds.addListener(updateLineCL)
//  b.layoutBounds.addListener(updateLineCL)
//  updateLine()

  // This leads to weird errors (see TODO: error url), this is why we use the "updateLine" listener instead
  //  line.endX <== Bindings.createDoubleBinding(
  //    () => math.max(a.layoutBounds.get.getWidth, b.layoutBounds.get.getWidth) + 6,
  //    a.layoutBounds, b.layoutBounds)

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
  id = Integer.toHexString(hashCode()) // TODO: remove
  font = MathText.VariableFont
}

class Num(math:MCN) extends Text(math.n.toString) {
  id = Integer.toHexString(hashCode()) // TODO: remove
  font = MathText.NumberFont
}

class GenericSymbol(cd:String, name:String) extends Text(s"$cd.$name") {
  //  font = MathText.VariableFont
}

class Missing() extends Text("\u2603") {
  id = Integer.toHexString(hashCode()) // TODO: remove
  font = MathText.SymbolFont
}
