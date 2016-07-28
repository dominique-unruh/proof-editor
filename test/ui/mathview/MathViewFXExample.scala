package ui.mathview

import misc.Utils.JavaFXImplicits._
import javafx.animation.{KeyFrame, Timeline}
import javafx.event.ActionEvent
import javafx.util

import cmathml.CMathML.{minus, plus, times}
import cmathml._
import org.scalatest.FunSuite
import ui.TestFxApp

import scalafx.scene.control.Label
import scalafx.scene.layout.VBox

object MathViewFXExample {
  def main(args: Array[String]) = {
    TestFxApp.run {
      val mw = new MathViewFX()
      val m = plus(minus(CI("x"),CI("y")),times(CI("z"),CI("w")))
      mw.setMath(m)

      var s1 : MCSymbol = null
      var s2 : MCSymbol = null
      var s3 : MCSymbol = null

      mw.mathDoc.root match {
        case MApply(h1 : MCSymbol, MApply(h2 : MCSymbol, _*), MApply(h3 : MCSymbol, _*)) =>
          { s1 = h1; s2 = h2; s3 = h3 }
      }

      val box = new VBox(new Label("X"),new Label("I"),mw)

      s1.name = "meh"
      s2.name = "la"
      s3.name = "lu"

//      s1.name = "minus"
//      s2.name = "divide"

      val timeline = new Timeline(
        new KeyFrame(util.Duration.millis(2000), (_:ActionEvent) => s1.name = "divide"),
        new KeyFrame(util.Duration.millis(4000), (_:ActionEvent) => s2.name = "divide"),
        new KeyFrame(util.Duration.millis(6000), (_:ActionEvent) => s3.name = "divide"),
        new KeyFrame(util.Duration.millis(8000), (_:ActionEvent) => s3.parent.asInstanceOf[MApply].setArgs(MCI("a"),MCI("b")))
      )
      timeline.play()

      box
    }
  }
}
