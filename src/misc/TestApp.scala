package misc

import java.io.{PrintWriter, StringWriter}
import java.lang.Boolean
import java.lang.System.out
import java.lang.Thread.UncaughtExceptionHandler
import java.lang.reflect.InvocationTargetException
import java.util.logging.{FileHandler, Level, Logger}
import javafx.application.Application
import javafx.beans.value
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.ScrollPane.ScrollBarPolicy
import javafx.scene.control._
import javafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import javafx.scene.layout.{BorderPane, VBox}
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
  val cmml1 = Apply(CSymbol("arith1","plus"),CI("x"),CI("y"))
  val cmml = Apply(CSymbol("arith1","minus"),
    cmml1,
    Apply(CSymbol("arith1","divide"),CI("a"),CI("b")))
  val editAt = Path.make(1)
  val math1 = new MathViewMQ()
  val math = new MathViewMQ()

  @FXML
  private var formulaList = null : VBox
  @FXML
  private var logArea = null : TextArea

  @FXML
  private def newFormula(event : ActionEvent) : Unit = {
    math.setMath(cmml1,Some(editAt.toPathRev))
  }

  @FXML
  private def editSelection(event : ActionEvent) : Unit = {
    println("edit selection")
    error("not implemented")
  }

  @FXML
  private def newFromSelection(event : ActionEvent) : Unit = {
    println("new from selection")
    val sel = math.getSelection
    if (!sel.isEmpty) {
      val m = math.getMath.subterm(sel.get)
      math.setMath(math.getMath.replace(sel.get, CN(123)))
      val newmath = new MathViewMQ()
      newmath.setMath(m)
      formulaList.getChildren.add(newmath)
    } else {
      formulaList.getChildren.add(new Label("No selection"))
    }
  }

  def log(msg:String, numLines:Int) = {
    var idx = 0
    for (i <- 1 to numLines)
      if (idx != -1) idx = msg.indexOf('\n',idx)+1
    val msg2 = if (idx == -1) msg else msg.substring(0,idx)
    logArea.appendText(msg2)
//    if (!msg2.endsWith("\n")) logArea.appendText("\n")
  }

  def actualException(e:Throwable) : Throwable =
    if (e.getCause != null) actualException(e.getCause) else e

  def start(primaryStage: Stage) {
    val loader = new FXMLLoader(getClass().getResource("testapp.fxml"))
    loader.setController(this)
    val fxml : Parent = loader.load()
    println("formulaList",formulaList)

//    val handler = new FileHandler("logfile.txt")
//    Logger.getLogger("").addHandler(handler)
    Logger.getLogger("").log(Level.WARNING,"logging test")
    Thread.currentThread().setUncaughtExceptionHandler({(t: Thread, e: Throwable) =>
      val e2 = actualException(e)
      e2.printStackTrace()
      val sw = new StringWriter()
      e2.printStackTrace(new PrintWriter(sw))
      log(sw.getBuffer.toString,5)})



    primaryStage.setScene(new Scene(fxml, 800, 600))

    primaryStage.setTitle("Proof editor")
    WebConsoleListener.setDefaultListener((webView: WebView, message: String, lineNumber: Int, sourceId: String) =>
        out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message))

    math.setMath(cmml,None)
    formulaList.getChildren.add(math)
    math1.setMath(cmml1,None)
    formulaList.getChildren.add(math1)

    math.addEditedListener(m => {println("edited",m); math.setMath(cmml1.replace(editAt,m))})

    primaryStage.getScene.getStylesheets.add(getClass().getResource("/testapp.css").toExternalForm())
    primaryStage.show
  }
}
