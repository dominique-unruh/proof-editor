package ui.mathview

import misc.Utils.JavaFXImplicits._
import javafx.animation.{KeyFrame, Timeline}
import javafx.event.ActionEvent
import javafx.util

import cmathml.CMathML.{divide, minus, plus, times}
import cmathml._
import org.scalatest.FunSuite
import ui.TestFxApp
import ui.mathview.MathViewFX.{CursorLeft, CursorPos}

import scala.sys
import scalafx.scene.control.{Control, Label}
import scalafx.scene.layout.VBox

object MathViewFXExample {


  def main(args: Array[String]) = {
    TestFxApp.run {
      TestFxApp.useTestAppCss()
      val mwx = new MathEdit()


      val mw = new MathEdit()
      val m = plus(minus(CNone(),CI("y")),times(CI("z"),CI("w")))
      mw.setMath(m)

      var s1 : MCSymbol = null
      var s2 : MCSymbol = null
      var s3 : MCSymbol = null
      var z : MCI = null
      var w : MCI = null

      mw.mathDoc.root match {
        case MApply(h1 : MCSymbol, MApply(h2 : MCSymbol, _*), MApply(h3 : MCSymbol, zz : MCI, ww : MCI)) =>
          { s1 = h1; s2 = h2; s3 = h3; z = zz; w = ww }
      }

      val box = new VBox(/*mwx,*/new Label("X"),new Label("I"),mw)

//      s1.name = "meh"
//      s2.name = "la"
//      s3.name = "lu"

//      s1.name = "minus"
//      s2.name = "divide"

      val timeline = new Timeline(
        new KeyFrame(util.Duration.millis(2000), (_:ActionEvent) => s1.name = "divide"),
        new KeyFrame(util.Duration.millis(4000), (_:ActionEvent) => s2.name = "divide"),
        new KeyFrame(util.Duration.millis(16000), (_:ActionEvent) => s3.name = "divide")
//        new KeyFrame(util.Duration.millis(8000), (_:ActionEvent) => s3.parent.asInstanceOf[MApply].setArgs(MCI("a"),MCI("b")))
      )
//      timeline.play()

      var count = 0
      val timeline2 = new Timeline(
        new KeyFrame(util.Duration.millis(1000), {(_:ActionEvent) => count += 1; z.name = "z" * count }))
      timeline2.setCycleCount(1000)
//      timeline2.play()

//      mw.setMath(CNone())

      mw.cursorPos.value = CursorPos(z,CursorLeft)

      def pr() =
        mw.mathDoc.root match {
          case MApply(h1 : MCSymbol, MApply(h2 : MCSymbol, _*), a3 @ MApply(h3 : MCSymbol, zz : MCI, ww : MCI)) =>
            println(s"****************** $zz - ${mw.infos.get(zz) match {case Some(i) => i.node; case None => "<missing>"}}")
        }
      pr()
      z.name = "zz"
      pr()
      s3.name="divide"
      pr()
//      sys.exit(1)

//      mw.requestFocus()

      box
    }
  }
}
