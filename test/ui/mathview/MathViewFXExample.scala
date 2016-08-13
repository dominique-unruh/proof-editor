package ui.mathview

import javafx.animation.{KeyFrame, Timeline}
import javafx.event.ActionEvent
import javafx.scene.input.MouseEvent
import javafx.util

import cmathml.CMathML._
import cmathml._
import misc.Utils.ImplicitConversions._
import ui.TestFxApp
import ui.mathview.MathViewFX.{CursorPos, CursorRight}

import scalafx.scene.control.Label
import scalafx.scene.layout.VBox

object MathViewFXExample {
  def after(time : Double, action : => Unit) = {
    val timeline = new Timeline(
      new KeyFrame(util.Duration.millis(time*1000), (_:ActionEvent) => action))
    timeline.play()
  }

  def main(args: Array[String]) = {
    TestFxApp.run {
      TestFxApp.useTestAppCss()

      val mw = new MathEdit()

      val x = new MCI("x")
      val y = new MCI("y")
      val xToY = new MApply(arith1.power,x,y)
      val a = new MCI("a")
      val b = new MCI("b")
      val aDivB = new MApply(arith1.divide,a,b)
      val xyPlusAb = new MApply(arith1.plus,xToY,aDivB)

      val root = xyPlusAb
      if (root.isAttached) root.replaceWith(new MCNone)
      mw.setMath(root)

      mw.editable.value = Some(aDivB)
      mw.cursorPos.value = CursorPos(mw.editable.value.get,CursorRight)

      val box = new VBox(new Label("X"),new Label("I"),mw)

      mw.onMouseClicked = { e:MouseEvent => println("mouse click",e) }

      box
    }
  }
}
