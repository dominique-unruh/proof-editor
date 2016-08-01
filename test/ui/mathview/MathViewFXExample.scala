package ui.mathview

import java.lang.System._
import java.lang.Thread._

import misc.Utils.ImplicitConversions._
import javafx.animation.{KeyFrame, Timeline}
import javafx.application.Application
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.ActionEvent
import javafx.geometry.Bounds
import javafx.scene.{Node, Scene}
import javafx.scene.layout.Pane
import javafx.scene.text.Text
import javafx.scene.web.WebView
import javafx.stage.Stage
import javafx.util

import cmathml.CMathML.{divide, minus, plus, times}
import cmathml.MutableCMathML.fromCMathML
import cmathml._
import com.sun.javafx.webkit.WebConsoleListener
import org.scalatest.FunSuite
import ui.mathview.MathViewFX.{CursorLeft, CursorPos, CursorRight}

import scala.sys
import scalafx.scene.control.{Control, Label}
import scalafx.scene.layout.VBox


class TestFxApp extends Application {
  override def start(st: Stage): Unit = {
    import TestFxApp._

    stage = st

    WebConsoleListener.setDefaultListener(new WebConsoleListener {
      override def messageAdded(webView: WebView, message: String, lineNumber: Int, sourceId: String): Unit =
        out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message)})


    pane = new Pane
    stage.setScene(new Scene(TestFxApp.pane,640,480))
    //    if (TestFxApp.showThisNode!=null) TestFxApp.pane.getChildren.add(TestFxApp.showThisNode)
    stage.setTitle(name)
    //    pane.setBackground(new Background(new BackgroundFill(Paint.valueOf("red"), null, null)))
    if (callMe!=null) callMe()
    //    stage.getScene.addMnemonic(new Mnemonic(pane,
    //      new KeyCodeCombination(KeyCode.Q, KeyCombination.SHORTCUT_DOWN)))
    stage.show()
  }
}
object TestFxApp {
  //  var showThisNode : Node = null
  var pane : Pane = null
  var callMe : () => Unit = null
  var name : String = "TestFxApp"
  //  val testAppCss = getClass().getResource("/testapp/testapp.css").toExternalForm()
  var stage : Stage = null
  //  def useTestAppCss() = stage.getScene.getStylesheets.add(testAppCss)


  def run(init: =>Node) = {
    callMe = { () => val node = init; pane.getChildren.add(node) }
    name = currentThread.getStackTrace()(2).getClassName
    Application.launch(classOf[TestFxApp])
  }
}


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
