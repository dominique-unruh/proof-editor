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

//trait MathHighlight extends Node {
//  def setSize(size:Bounds) : Unit
//}


class MathViewFX extends Pane {
}

object MathViewFX {
//  sealed trait CursorSide
//  final object CursorLeft extends CursorSide
//  final object CursorRight extends CursorSide
//  case class CursorPos(node:MutableCMathML, side:CursorSide)
}

//trait MathRendererContext {
//  def getNodeForEmbedding(math: MutableCMathML) : Node
//}

//trait MathRendererFactory {
//  def renderer(context:MathViewFX#MathNode, math:MutableCMathML) : Node
//}

