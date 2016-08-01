package ui.mathview

import java.io.IOException

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
//    def own(math:MutableCMathML) = /*context.own(math)*/ ()
    def get(math:MutableCMathML) = context.getNodeForEmbedding(math)
    def binop(hd: MutableCMathML, op: String, x: MutableCMathML, y: MutableCMathML) = {
//      own(hd)
      val x$ = get(x)
      val y$ = get(y)
      new BinOp(op, x$, y$)
    }

    math match {
      case m: MCI => new Var(m)
      case MApply(hd@MCSymbol("arith1", "times"), x, y) => binop(hd, "\u22c5", x, y)
      case MApply(hd@MCSymbol("arith1", "divide"), x, y) => {
        /*own(hd);*/ (new Fraction(get(x), get(y)))
      }
      case MCNone() => new Missing()
//      case MApply(hd, args@_*) => (new GenericApply(get(hd), args.map(get(_))))
//      case MCSymbol(cd, name) => (new GenericSymbol(cd, name))
//      case m @ MCN(num) => new Num(m)
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

class BinOp(op:String, a:Node, b:Node) extends HBox {
  import MathText._
  id = Integer.toHexString(hashCode()) // TODO: remove
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
  }

  innerHeight.onChange((updateParens())) // Delayed action. I think otherwise we end up changing bounds within a recomputation of bounds, leading to spurious errors?
  updateParens()

  children.addAll(open,a,opTxt,b,close)
}

class Fraction(a:Node, b:Node) extends VBox {
  id = Integer.toHexString(hashCode()) // TODO: remove
  alignment = Pos.Center
  val line = new Line()
  children.addAll(a, line, b)

  line.startX = 0
  line.endX <== Bindings.createDoubleBinding(
    () => math.max(a.layoutBounds.get.getWidth, b.layoutBounds.get.getWidth) + 6,
    a.layoutBounds, b.layoutBounds)
  line.strokeWidth = 2
}


class Var(math:MCI) extends Text(math.name) {
  id = Integer.toHexString(hashCode()) // TODO: remove
  font = MathText.VariableFont
}

class Missing() extends Text("\u2603") {
  id = Integer.toHexString(hashCode()) // TODO: remove
  font = MathText.SymbolFont
}
