package ui.mathview

import misc.Utils.ImplicitConversions._
import javafx.animation.{KeyFrame, Timeline}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.ActionEvent
import javafx.geometry.Bounds
import javafx.scene.layout.Pane
import javafx.scene.text.Text
import javafx.util

import cmathml.CMathML.{divide, minus, plus, times}
import cmathml.MutableCMathML.fromCMathML
import cmathml._
import org.scalatest.FunSuite
import ui.TestFxApp
import ui.mathview.MathViewFX.{CursorLeft, CursorPos, CursorRight}

import scala.sys
import scalafx.scene.control.{Control, Label}
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

      def listener = new ChangeListener[Bounds] {
        override def changed(observable: ObservableValue[_ <: Bounds], oldValue: Bounds, newValue: Bounds): Unit =
          println("change")
      }

      val p1 = new Pane()
      p1.layoutBoundsProperty().addListener(listener)
      val p2 = new Pane()
      p2.layoutBoundsProperty().addListener(listener)
      p1.getChildren.add(p2)
      val t2 = new Text("t2")
      p2.getChildren.add(t2)
      val p3 = new Pane()
      p3.layoutBoundsProperty().addListener(listener)
      val t3 = new Text("t2")
      p3.getChildren.add(t2)
      p2.getChildren.add(p3)
      p3.getChildren.remove(p2)

//      sys.exit()

      val mw = new MathEdit()
//      val m = plus(minus(CNone(),CI("y")),times(CI("z"),CI("w")))
//      mw.setMath(m)

      var s1 = new MCSymbol(plus)
      var s2 = new MCSymbol(divide)
      var s3 : MCSymbol = new MCSymbol(times)
      var x = new MCI("x")
      var y = new MCI("y")
      var z = new MCI("z")
      val h1 = new MCI("h1")
      val h2 = new MCI("h2")
      val binop = new MApply(times,h1,h2)
      val r = new MCI("r")
      var w = new MApply(divide,new MCI("q"),binop) //new MCI("w")
      var a1 = new MApply(s2,x,y)
      var a2 = new MApply(s3,z,w)
      //      var r = new MApply(s1,a1,a2)

      val root = a2

      if (root.isAttached) root.replaceWith(new MCNone)
      mw.setMath(root)


      //      mw.mathDoc.root match {
      //        case r$ @ MApply(h1 : MCSymbol, a1$ @ MApply(h2 : MCSymbol, _*), MApply(h3 : MCSymbol, zz : MCI, ww : MCI)) =>
      //          { r = r$; s1 = h1; s2 = h2; s3 = h3; z = zz; w = ww; a1 = a1$ }
      //      }

      mw.editable.value = Some(mw.mathDoc.root)

      //      mw.cursorPos.value = CursorPos(r,CursorRight)

//      r.replaceWith(binop)
      val u = new MCI("u")
      h1.replaceWith(u) // TODO why does this throw an exception?

      sys.exit()

      val box = new VBox(new Label("X"),new Label("I"),mw)

      box
    }
  }
}
