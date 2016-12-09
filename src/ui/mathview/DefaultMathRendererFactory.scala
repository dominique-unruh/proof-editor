package ui.mathview

import java.io.IOException
import javafx.geometry

import cmathml.CMathML.{arith1, fns1, internal, interval1}
import cmathml._
import misc.Utils

import scala.collection.mutable
import scalafx.Includes._
import scalafx.beans.binding.Bindings
import scalafx.geometry.Pos
import javafx.scene.Node

import cmathml.MutableCMathML.m_arith1

import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.text.{Font, FontPosture, Text}

object DefaultMathRendererFactory extends MathRendererFactory {
  override def renderer(context: MathRendererContext, math: MutableCMathML): Node = {
    def own(math:MutableCMathML*) = for (m<-math) context.own(m)
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
    def quant(hd: MutableCMathML, op:String, vs: Seq[MCILike], body:MutableCMathML) = {
      own(hd)
      new Quantifier(op, vs.map(get(_):javafx.scene.Node), get(body))
    }

    math match {
      case m: MCI => new Var(m)
      case MApply(hd@MCSymbol(internal.backslashName.cd,internal.backslashName.name), cs@MCS(name)) =>
        own(hd); own(cs); new BackslashName(name)
      case MApply(hd@MCSymbol("relation1", "eq"), x, y) => binop(hd, "=", x, y)
      case MApply(hd@MCSymbol("relation1", "neq"), x, y) => binop(hd, "≠", x, y)
      case MApply(hd@MCSymbol("relation1", "lt"), x, y) => binop(hd, "<", x, y)
      case MApply(hd@MCSymbol("relation1", "gt"), x, y) => binop(hd, ">", x, y)
      case MApply(hd@MCSymbol("relation1", "leq"), x, y) => binop(hd, "≤", x, y)
      case MApply(hd@MCSymbol("relation1", "geq"), x, y) => binop(hd, "≥", x, y)
      case MApply(hd@MCSymbol("arith1", "unary_minus"), x) => prefixop(hd, "\u2212", x)
      case MApply(hd@MCSymbol("arith1", "plus"), x, y) => binop(hd, "+", x, y)
      case MApply(hd@MCSymbol("arith1", "minus"), x, y) => binop(hd, "\u2212", x, y)
      case MApply(hd@MCSymbol("arith1", "times"), x, y) => binop(hd, "\u22c5", x, y)
      case MApply(hd@MCSymbol("arith1", "divide"), x, y) => own(hd); new Fraction(get(x), get(y))
      case MApply(hd@MCSymbol("arith1", "power"), x, y) => own(hd); new Power(get(x), get(y))
      case MApply(hd@MCSymbol("logic1", "and"), x, y) => binop(hd, "∧", x, y)
      case MApply(hd@MCSymbol("logic1", "or"), x, y) => binop(hd, "∨", x, y)
      case MApply(hd@MCSymbol("logic1", "equivalent"), x, y) => binop(hd, "⇔", x, y)
      case MApply(hd@MCSymbol("logic1", "xor"), x, y) => binop(hd, "⊕", x, y)
      case MApply(hd@MCSymbol("logic1", "implies"), x, y) => binop(hd, "⇒", x, y)
      case MApply(hd@MCSymbol("logic1", "not"), x) => prefixop(hd, "¬", x)
      case MApply(MCSymbol(internal.formulaRef.cd,internal.formulaRef.name),MCN(i)) => new Text(s"($i)")
      case MApply(hd@MCSymbol(arith1.sum.cd,arith1.sum.name),
                  int@MApply(hd2@MCSymbol(interval1.integer_interval.cd,interval1.integer_interval.name),start,end),
                  lam@MBind(hd3@MCSymbol(fns1.lambda.cd,fns1.lambda.name),Seq(x),body)) =>
        own(hd,int,hd2,lam,hd3)
        new Sum(get(x),get(start),get(end),get(body))
      case MCSymbol("logic1","true") => new SFSymbol("true")
      case MCSymbol("logic1","false") => new SFSymbol("false")
      case MBind(hd@MCSymbol("quant1","forall"), vs, body) => quant(hd, "∀", vs, body)
      case MBind(hd@MCSymbol("quant1","exists"), vs, body) => quant(hd, "∃", vs, body)
      case MCNone() => new Missing()
      case MApply(hd, args@_*) => new GenericApply(get(hd), args.map(get))
      case MBind(hd, vars, arg) => new GenericBind(get(hd), vars.map(get(_)), get(arg))
      case MCSymbol(cd, name) => new GenericSymbol(cd, name)
      case MCS(str) => new MathString(str)
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
  val SFFont = Font("FreeSans",fontSize)
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
    val font = symbolFont(h+1)
    open.font = font
    close.font = font
    super.layoutChildren()
  }

  getChildren.addAll(open,a,opTxt,b,close)
}

class Sum(x:javafx.scene.Node, start:javafx.scene.Node, end:javafx.scene.Node, body:javafx.scene.Node) extends javafx.scene.layout.HBox {
  import MathText._
  setId(Integer.toHexString(hashCode())) // TODO: remove
  setAlignment(geometry.Pos.CENTER)
  val open = symbolText("sum(")
  val close = symbolText(")")
  val eqSign = symbolText("=")
  val dots = symbolText("..")
  val comma = symbolText(",")
  val inner : List[javafx.scene.Node] = List(x,eqSign,start,dots,end,comma,body)

  setFillHeight(false)

  override def layoutChildren(): Unit = {
    val h = Utils.max(inner.map(_.prefHeight(-1)) : _*)
    val font = symbolFont(h+1)
    open.font = font
    close.font = font
    super.layoutChildren()
  }

  getChildren.addAll(open,x,eqSign,start,dots,end,comma,body,close)
}


class Quantifier(op:String, vars:Seq[javafx.scene.Node], body:javafx.scene.Node) extends javafx.scene.layout.HBox {
  import MathText._
  setId(Integer.toHexString(hashCode())) // TODO: remove
  setAlignment(geometry.Pos.CENTER)
  val open = symbolText("(")
  val close = symbolText(")")
  val inner = mutable.MutableList[javafx.scene.Node]()
  inner += symbolText(op)
  var first = true
  for (v <- vars) {
    if (!first) inner += symbolText(",")
    inner += v
    first = false
  }
  inner += symbolText(".")
  inner += body

  setFillHeight(false)

  override def layoutChildren(): Unit = {
    val h = Utils.max(inner.map(_.prefHeight(-1)) :_*)
    val font = symbolFont(h+1)
    open.font = font
    close.font = font
    super.layoutChildren()
  }

  getChildren.add(open)
  getChildren.addAll(inner:_*)
  getChildren.add(close)
}

class Power(a:javafx.scene.Node, b:javafx.scene.Node) extends javafx.scene.layout.HBox {
  setId(Integer.toHexString(hashCode())) // TODO: remove
  setAlignment(geometry.Pos.CENTER)

  setFillHeight(false)
  b.scaleX = 0.8
  b.scaleY = 0.8

  getChildren.addAll(a,b)

  override def layoutChildren(): Unit = {
    val aHeight = a.prefHeight(-1)
    b.translateY = -0.4 * aHeight
    super.layoutChildren()
  }
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
//    println("height",h)
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
    line.endX = math.max(a.prefWidth(-1), b.prefWidth(-1)) + 6
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

class GenericBind(head:Node, vars:Seq[Node], arg:Node)
  extends HBox {
  import MathText._
  alignment = Pos.Center
  children.addAll(head,symbolText("["))
  var first = true
  for (a <- vars) {
    if (!first) children.add(symbolText(","))
    children.add(a)
    first = false
  }
  children.addAll(symbolText("→"),arg,symbolText("]"))
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
  font = MathText.VariableFont
}

class SFSymbol(name:String) extends Text(name) {
  font = MathText.SFFont
}

class MathString(str:String) extends Text(s"“$str”") {
  font = MathText.SFFont
}

class BackslashName(name:String) extends Text(s"\\$name") {
  font = MathText.SFFont
}

class Missing() extends Rectangle {
//  id = Integer.toHexString(hashCode()) // TODO: remove
//  font = MathText.SymbolFont
  width = Missing.width
  height = Missing.height
//  stroke = Color.Blue
  fill = Missing.color
}
object Missing {
  private val color = Color.rgb(1,1,0,0.2)
  private val (width,height) = {
    val t = new Text("x")
    t.font = MathText.SymbolFont
    val bounds = t.layoutBounds.value
    (bounds.width, bounds.height*0.8)
  }
}