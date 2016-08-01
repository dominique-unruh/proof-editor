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
  def main(args: Array[String]) = {
    TestFxApp.run {
      val mw = new MathViewFX()

      var s3 : MCSymbol = new MCSymbol(times)
      var z = new MCI("z")
      val h1 = new MCI("h1")
      val h2 = new MCI("h2")
      val binop = new MApply(times,h1,h2)
      var w = new MApply(divide,new MCI("q"),binop)
      var a2 = new MApply(s3,z,w)

      mw.setMath(a2)

      val u = new MCI("u")
      h1.replaceWith(u) // TODO why does this throw an exception?

      sys.exit()
    }
  }
}
