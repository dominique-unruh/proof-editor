package misc

import java.lang.System.out
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.scene.Scene
import javafx.scene.control.ScrollPane.ScrollBarPolicy
import javafx.scene.control.{Button, ScrollPane, ToolBar}
import javafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
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
      Apply(CSymbol("arith1","divide"),CI("a"),CI("b")))
//    val pmml = MathView.cmathmlToMathjax(cmml)
//    val math = new MathView(pmml)
    text.getChildren.add(new Text("Hello "))
//    val tex = MQLatex.cmathmlToLatex(cmml)
    val math = new MathViewMQ()
    math.setMath(cmml,None)
    text.getChildren.add(math)
    text.getChildren.add(new Text(" and "))
//    val tex1 = MQLatex.cmathmlToLatex(cmml1)
    val math1 = new MathViewMQ()
    math1.setMath(cmml1,None)
    text.getChildren.add(math1)
    text.getChildren.add(new Text("..."))

    primaryStage.setScene(new Scene(root, 800, 250))

    val editAt = Path.make(1)
    btnNew.setOnAction((event:ActionEvent) => math.setMath(cmml1,Some(editAt.toPathRev)))
    math.addEditedListener(m => {println("edited",m); math.setMath(cmml1.replace(editAt,m))})

    val btnCopy = new Button("Copy selection");
    toolbar.getItems.add(btnCopy)
    btnCopy.setOnAction((_:ActionEvent) => {
      val sel = math.getSelection
      if (!sel.isEmpty) {
        val m = math.getMath.subterm(sel.get)
        math.setMath(math.getMath.replace(sel.get, CN(123)))
        val newmath = new MathViewMQ()
        newmath.setMath(m)
        text.getChildren.add(new Text("\nNew math:\n"))
        text.getChildren.add(newmath)
      } else
        text.getChildren.add(new Text("\nNothing selected.\n"))
      ()
    })
    primaryStage.getScene.getAccelerators.put(
      new KeyCodeCombination(KeyCode.C, KeyCombination.SHORTCUT_DOWN), {() => btnCopy.fire()})


//    println("XXX",editAt,cmml1.replace(editAt,CN(33)))

    println("about to load css",getClass().getResource("/testapp.css"))
    primaryStage.getScene.getStylesheets.add(getClass().getResource("/testapp.css").toExternalForm())
    primaryStage.show
  }
}
