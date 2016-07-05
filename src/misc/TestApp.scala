package misc

import java.lang.System.out
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.scene.Scene
import javafx.scene.control.ScrollPane.ScrollBarPolicy
import javafx.scene.control.{Button, ScrollPane, ToolBar}
import javafx.scene.layout.BorderPane
import javafx.scene.text.{Text, TextFlow}
import javafx.scene.web.WebView
import javafx.stage.Stage

import com.sun.javafx.webkit.WebConsoleListener
import mathview.{MQLatex, MathViewMQ}
import misc.Utils.JavaFXImplicits._
import cmathml._

object TestApp {
  def main(args: Array[String]) = Application.launch(classOf[TestApp], args:_*)
}

class TestApp extends Application {
  def start(primaryStage: Stage) {
    primaryStage.setTitle("Hello World! (Scala)")
    WebConsoleListener.setDefaultListener((webView: WebView, message: String, lineNumber: Int, sourceId: String) =>
        out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message))
    val scroll = new ScrollPane()
    val root = new BorderPane()
    root.setCenter(scroll)
    val btnNew = new Button("New formula")
    val toolbar = new ToolBar(btnNew)
    root.setTop(toolbar)
    val text = new TextFlow()
    scroll.setContent(text)
    scroll.setVbarPolicy(ScrollBarPolicy.ALWAYS)
//    scroll.setHbarPolicy(ScrollBarPolicy.NEVER)
    scroll.setFitToWidth(true)
    val cmml1 = Apply(CSymbol("arith1","plus"),CI("x"),CI("y"))
    val cmml = Apply(CSymbol("arith1","minus"),
      cmml1,
      Apply(CSymbol("arith1","plus"),CI("x2"),CI("y2")))
//    val pmml = MathView.cmathmlToMathjax(cmml)
//    val math = new MathView(pmml)
    text.getChildren.add(new Text("Hello "))
//    val tex = MQLatex.cmathmlToLatex(cmml)
    val math = new MathViewMQ()
    math.setMath(cmml)
    text.getChildren.add(math)
    text.getChildren.add(new Text(" and "))
//    val tex1 = MQLatex.cmathmlToLatex(cmml1)
    val math1 = new MathViewMQ()
    math1.setMath(cmml1)
    text.getChildren.add(math1)
    text.getChildren.add(new Text("..."))

    btnNew.setOnAction((event:ActionEvent) => { print("button"); math.setMath(CN(1))})


    primaryStage.setScene(new Scene(root, 800, 250))
    println("about to load css",getClass().getResource("/testapp.css"))
    primaryStage.getScene.getStylesheets.add(getClass().getResource("/testapp.css").toExternalForm())
    primaryStage.show
  }
}
