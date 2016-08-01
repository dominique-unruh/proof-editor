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

//
//object MathText {
////  Font.loadFont(getClass.getResource("mathquill/font/Symbola.otf").toString,0)
//
//  val fontSize = 22 : Double
//  //  for (f <- Font.fontNames) println(f)
//  val VariableFont = Font("FreeSerif",FontPosture.Italic,fontSize)
//  //  println(VariableFont)
//  val SymbolFont = symbolFont(fontSize)
//  def symbolFont(size:Double) = new Font("Symbola",size)
//  val NumberFont = SymbolFont
//  def text(txt:String, font:Font) = { val t = new Text(txt); t.font = font; t }
//  def symbolText(txt:String) = text(txt,SymbolFont)
//}
