/**
  * Created by unruh on 7/3/16.
  */

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
import mathview.MQLatex
import misc.Utils.JavaFXImplicits._

object TestApp {
  def main(args: Array[String]) = Application.launch(classOf[TestApp], args:_*)
}

class TestApp extends Application {
  def start(primaryStage: Stage) {

    print(MQLatex.parseLatex("3+4"))
//    System.exit(1)

    primaryStage.setTitle("Hello World! (Scala)")
    WebConsoleListener.setDefaultListener((webView: WebView, message: String, lineNumber: Int, sourceId: String) =>
        out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message))
    val scroll = new ScrollPane()
    val root = new BorderPane()
    root.setCenter(scroll)
    val btnNew = new Button("New formula")
    btnNew.setOnAction((event:ActionEvent) => print("button"))
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
    val tex = MQLatex.cmathmlToLatex(cmml)
    val math = new MathViewMQ(tex)
    text.getChildren.add(math)
    text.getChildren.add(new Text(" and "))
    val tex1 = MQLatex.cmathmlToLatex(cmml1)
    val math1 = new MathViewMQ(tex1)
    text.getChildren.add(math1)
    text.getChildren.add(new Text("..."))
    primaryStage.setScene(new Scene(root, 800, 250))
    println("about to load css",getClass().getResource("/testapp.css"))
    primaryStage.getScene.getStylesheets.add(getClass().getResource("/testapp.css").toExternalForm())
    primaryStage.show
  }
}
