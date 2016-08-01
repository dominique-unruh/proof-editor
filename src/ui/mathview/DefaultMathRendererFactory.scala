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


object MathText {
//  Font.loadFont(getClass.getResource("mathquill/font/Symbola.otf").toString,0)

  val fontSize = 22 : Double
  //  for (f <- Font.fontNames) println(f)
  val VariableFont = Font("FreeSerif",FontPosture.Italic,fontSize)
  //  println(VariableFont)
  val SymbolFont = symbolFont(fontSize)
  def symbolFont(size:Double) = new Font("Symbola",size)
  val NumberFont = SymbolFont
  def text(txt:String, font:Font) = { val t = new Text(txt); t.font = font; t }
  def symbolText(txt:String) = text(txt,SymbolFont)
}

class BinOp(op:String, a:Node, b:Node) extends HBox {
  a.layoutBounds.onChange({})
  b.layoutBounds.onChange({})
  children.addAll(a,b)
}

class Fraction(a:Node, b:Node) extends VBox {
  id = Integer.toHexString(hashCode()) // TODO: remove
  alignment = Pos.Center
  val line = new Line()
  children.addAll(a, line, b)

  a.layoutBounds.onChange({})
  b.layoutBounds.onChange({})

  val innerWidth = Bindings.createDoubleBinding(
    () =>
      b.layoutBounds.get.getWidth,
    b.layoutBounds)

  line.startX = 0
  line.endX <== innerWidth
  line.strokeWidth = 2
}
