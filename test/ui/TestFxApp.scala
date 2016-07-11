package ui

import java.lang.System._
import java.lang.Thread._
import javafx.application.Application
import javafx.event.EventType
import javafx.scene.input._
import javafx.scene.{Node, Scene}
import javafx.scene.layout.{AnchorPane, Background, BackgroundFill, Pane}
import javafx.scene.paint.Paint
import javafx.scene.web.WebView
import javafx.stage.Stage

import com.sun.javafx.webkit.WebConsoleListener

class TestFxApp extends Application {
  override def start(stage: Stage): Unit = {
    import TestFxApp._

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

  def run(init: =>Node) = {
    callMe = { () => val node = init; pane.getChildren.add(node) }
    name = currentThread.getStackTrace()(2).getClassName
    Application.launch(classOf[TestFxApp])
  }
}
