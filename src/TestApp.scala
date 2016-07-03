/**
  * Created by unruh on 7/3/16.
  */

import java.lang.System.out
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.StackPane
import javafx.scene.web.WebView
import javafx.stage.Stage

import com.sun.javafx.webkit.WebConsoleListener

object TestApp {
  def main(args: Array[String]) = Application.launch(classOf[TestApp], args:_*)
}

class TestApp extends Application {
  def start(primaryStage: Stage) {
    primaryStage.setTitle("Hello World! (Scala)")
    WebConsoleListener.setDefaultListener(new WebConsoleListener() {
      def messageAdded(webView: WebView, message: String, lineNumber: Int, sourceId: String) {
        out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message)
      }
    })
    val root = new StackPane
    val cmml = Apply(CSymbol("arith1","minus"),
      Apply(CSymbol("arith1","plus"),CI("x"),CI("y")),
      Apply(CSymbol("arith1","plus"),CI("x2"),CI("y2")))
    val pmml = MathView.cmathmlToMathjax(cmml)
    // "<mrow path=all><mi class=leaf path=x>x</mi><mo class=leaf path=plus>+</mo><mi path=z class=leaf>z</mi></mrow>"
    print(pmml)
    val math = new MathView(pmml)
    root.getChildren.add(math)
    primaryStage.setScene(new Scene(root, 800, 250))
    primaryStage.show
  }
}
