package misc

import java.io.{PrintWriter, StringWriter}
import java.lang.Boolean
import java.lang.System.out
import java.lang.Thread.UncaughtExceptionHandler
import java.lang.reflect.InvocationTargetException
import java.util.logging.{FileHandler, Level, Logger}
import javafx.application.{Application, Platform}
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

import scala.collection.mutable

object TestApp {
  def main(args: Array[String]) = Application.launch(classOf[TestApp], args:_*)
}


class TestApp extends Application {
  val cmml1 = Apply(CSymbol("arith1","plus"),CI("x"),CI("y"))
  val cmml = Apply(CSymbol("arith1","minus"),
    cmml1,
    Apply(CSymbol("arith1","divide"),CI("a"),CI("b")))
//  val editAt = Path.make(1)
//  val math1 = new MathViewMQ()
//  val math = new MathViewMQ()

  @FXML
  private var formulaList = null : VBox
  @FXML
  private var logArea = null : TextArea

  @FXML
  private def newFormula(event : ActionEvent) : Unit = {
    addMath(CI("x"),Some(Path.empty))
  }

  @FXML
  private def quit(event: ActionEvent) : Unit = {
    Platform.exit()
  }

  @FXML
  private def editSelection(event : ActionEvent) : Unit = {
    val math = currentlySelectedMath
    if (math==null)  { log("No selected mathview"); return }
    val sel = math.getSelection
    if (sel.isEmpty) { log("No selection"); return }
    math.setMath(math.getMath,Some(sel.get))
  }

  @FXML
  private def newFromSelection(event : ActionEvent) : Unit = {
    val math = currentlySelectedMath
    if (math==null)  { log("No selected mathview"); return }
    val sel = math.getSelection
    if (sel.isEmpty) { log("No selection"); return }
    val m = math.getMath.subterm(sel.get)
//    math.setMath(math.getMath.replace(sel.get, CN(123)))
    val newmath = new MathViewMQ()
    newmath.setMath(m)
    formulaList.getChildren.add(newmath)
  }

  private var currentlySelectedMath : MathViewMQ = null

  private val mathviews = new mutable.MutableList[MathViewMQ]
  def addMath(math: CMathML, editPath: Option[Path]=None) = {
    if (!editPath.isEmpty) math.subterm(editPath.get) // Make sure the editPath is valid
    val mw = new MathViewMQ()
    mw.setMath(math,editPath)
    mathviews += mw
    mw.selectedProperty.addListener((selected:Boolean) => currentlySelectedMath = mw)
    mw.addEditedListener(m => {println("edited",m,mw);
      mw.setMath(mw.getMath.replace(mw.editPath.get,m))})
    formulaList.getChildren.add(mw)
    mw
  }


  def log(msg:String, numLines:Int = -1) = {
    var msg2 : String = msg
    if (numLines>=0) {
      var idx = 0
      for (i <- 1 to numLines)
        if (idx != -1) idx = msg2.indexOf('\n', idx) + 1
      msg2 = if (idx == -1) msg2 else msg2.substring(0, idx)
    }
    logArea.appendText(msg2)
    logArea.appendText("\n")
  }

  def actualException(e:Throwable) : Throwable =
    if (e.getCause != null) actualException(e.getCause) else e

  def start(primaryStage: Stage) {
    val loader = new FXMLLoader(getClass().getResource("testapp.fxml"))
    loader.setController(this)
    val fxml : Parent = loader.load()
    println("formulaList",formulaList)

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

    val math = addMath(cmml,None)
    val math1 = addMath(cmml1,None)

//    math.setMath(cmml,None)
//    formulaList.getChildren.add(math)
//    math1.setMath(cmml1,None)
//    formulaList.getChildren.add(math1)

//    val date = new DatePicker()
//    formulaList.getChildren.add(date)
//    date.focusedProperty().addListener(new ChangeListener[Boolean] {
//    override def changed(observable: ObservableValue[_ <: Boolean], oldValue: Boolean, newValue: Boolean): Unit =
//      println("date picker focus: "+newValue)
//  })

    primaryStage.getScene.getStylesheets.add(getClass().getResource("/testapp.css").toExternalForm())
    primaryStage.show
  }
}
