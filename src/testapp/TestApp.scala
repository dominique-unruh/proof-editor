package testapp
import java.io.{PrintWriter, StringWriter}
import java.lang.Boolean
import java.lang.System.out
import java.util.logging.{Level, Logger}
import javafx.application.{Application, Platform}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.beans.{InvalidationListener, Observable}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.geometry.Insets
import javafx.scene.control._
import javafx.scene.layout.{HBox, Pane, VBox}
import javafx.scene.web.WebView
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import cmathml._
import com.sun.javafx.webkit.WebConsoleListener
import mathview.MathViewMQ
import misc.GetterSetterProperty
import misc.Utils.JavaFXImplicits._
import theory.Formula
import trafo.{FormulaQ, IdentityTransformation, Question, TrafoInstance}
import ui.Interactor.{Editor, EditorFactory}
import ui.{ConnectingLine, Interactor}
import z3.Z3

import scala.collection.mutable
import scala.reflect.runtime.universe._

object TestApp {
  def main(args: Array[String]) = Application.launch(classOf[TestApp], args:_*)
}


class TestApp extends Application {
  val examples = List(
    CMathML.equal(CMathML.plus(CI("x"), CI("y")), CMathML.plus(CI("y"), CN(-1)))
  )

  @FXML private var formulaList = null: VBox
  @FXML private var logArea = null: TextArea
  @FXML private var interactor = null: Interactor[TrafoInstance]
  @FXML protected[this] var overlay = null : Pane
//  private def getOverlay() = overlay

  private def errorPopup(msg: String): Unit =
    new Alert(Alert.AlertType.ERROR, msg).showAndWait()

  @FXML private def idTrafo(event: ActionEvent): Unit = {
    interactor.setInteraction(new IdentityTransformation().createInteractive)
//    errorPopup("Not implemented")
  }

  @FXML
  private def newFormula(event: ActionEvent): Unit = {
    addMath(CI("x"), Some(Path.empty))
  }

  @FXML
  private def quit(event: ActionEvent): Unit = {
    Platform.exit()
  }

  @FXML
  private def editSelection(event: ActionEvent): Unit = {
    val math = currentlySelectedMath
    if (math == null) {
      log("No selected mathview"); return
    }
    val sel = math.getSelection
    if (sel.isEmpty) {
      log("No selection"); return
    }
    math.setMath(math.getMath, Some(sel.get))
  }

  @FXML
  private def newFromSelection(event: ActionEvent): Unit = {
    val math = currentlySelectedMath
    if (math == null) {
      log("No selected mathview"); return
    }
    val sel = math.getSelection
    if (sel.isEmpty) {
      log("No selection"); return
    }
    val m = math.getMath.subterm(sel.get)
    addMath(m)
  }

  /** Only invoke methods in JavaFX thread! */
  private lazy val z3 = new Z3(Map())

  @FXML
  private def simplify(event: ActionEvent): Unit = {
    val math = currentlySelectedMath
    if (math == null) {
      log("No selected mathview"); return
    }
    val expr = z3.fromCMathML(math.getMath)
    val simp = expr.simplify
    val simp2 = z3.toCMathML(simp)
    addMath(simp2)
  }

  @FXML
  private def deleteFormula(event: ActionEvent): Unit = {
    val math = currentlySelectedMath
    if (math == null) {
      log("No selected mathview"); return
    }
    formulaList.getChildren.removeAll(math)
  }

  private var currentlySelectedMath: MathViewMQ = null

  private val mathviews = new mutable.MutableList[MathViewMQ]

  def addMath(math: CMathML, editPath: Option[Path] = None) = {
    if (!editPath.isEmpty) math.subterm(editPath.get) // Make sure the editPath is valid
    val mw = new MathViewMQ()
    mw.setMath(math, editPath)
    mathviews += mw
    mw.selectedProperty.addListener((selected: Boolean) => currentlySelectedMath = mw)
    mw.addEditedListener(m => {
      println("edited", m, mw);
      mw.setMath(mw.getMath.replace(mw.editPath.get, m))
    })
    formulaList.getChildren.add(mw)
    mw
  }


  def log(msg: String, numLines: Int = -1) = {
    var msg2: String = msg
    if (numLines >= 0) {
      var idx = 0
      for (i <- 1 to numLines)
        if (idx != -1) idx = msg2.indexOf('\n', idx) + 1
      msg2 = if (idx == -1) msg2 else msg2.substring(0, idx)
    }
    logArea.appendText(msg2)
    logArea.appendText("\n")
  }

  def actualException(e: Throwable): Throwable =
    if (e.getCause != null) actualException(e.getCause) else e

  def start(primaryStage: Stage) {
    val fxmlSrc = getClass().getResource("/testapp/testapp.fxml")
    assert(fxmlSrc != null)
    val loader = new FXMLLoader(fxmlSrc)
    loader.setController(this)
    val fxml: Parent = loader.load()
    println("formulaList", formulaList)

    Logger.getLogger("").log(Level.WARNING, "logging test")
    Thread.currentThread().setUncaughtExceptionHandler({ (t: Thread, e: Throwable) =>
      val e2 = actualException(e)
      e2.printStackTrace()
      val sw = new StringWriter()
      e2.printStackTrace(new PrintWriter(sw))
      log(sw.getBuffer.toString, 5)
    })

    primaryStage.setScene(new Scene(fxml, 800, 600))
    primaryStage.setTitle("Proof editor")
    WebConsoleListener.setDefaultListener((webView: WebView, message: String, lineNumber: Int, sourceId: String) =>
      out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message))

    interactor.setEditorFactory(editorFactory)

    for (m <- examples) addMath(m, None)

    primaryStage.getScene.getStylesheets.add(getClass().getResource("/testapp/testapp.css").toExternalForm())
    primaryStage.show

    idTrafo(null)
  }

  // TODO: move to Utils
  def logProperty[T](name : String, prop : ObservableValue[T], force:Boolean=false) = {
    if (force) prop.addListener(new ChangeListener[T] {
      override def changed(observable: ObservableValue[_ <: T], oldValue: T, newValue: T): Unit =
        println(name+": "+prop)
    })
    prop.addListener(new InvalidationListener {
      override def invalidated(observable: Observable): Unit = {
        println(name+" invalidated")
    }})
    println(name+" initial: "+prop)
  }


  class FormulaEditor extends HBox with Editor[Option[Formula]] {
    override val editedType: TypeTag[Option[Formula]] = typeTag[Option[Formula]]
    override val questionType = typeTag[FormulaQ]
    val pickButton = new Button("Pick")
    pickButton.setPadding(Insets.EMPTY)
    val clearButton = new Button("Clear")
    clearButton.setPadding(Insets.EMPTY)
    val mathview = new MathViewMQ()
    var formula : Option[Formula] = None
    val line = new ConnectingLine(this, overlay)
    line.setLeft(mathview)

    override val valueProperty: GetterSetterProperty[Option[Formula]] = new GetterSetterProperty[Option[Formula]] {
      override protected def getter: Option[Formula] = formula
      override protected def setter(value: Option[Formula]): Unit = {
        formula = value
        if (formula.isEmpty) mathview.setVisible(false)
        else { mathview.setVisible(true); mathview.setMath(formula.get.math) }
      }
    }

    mathview.setMath(CN(0)) // Otherwise the mathview will not be resized to something small
    mathview.setVisible(false)
    getChildren.addAll(new VBox(1,pickButton,clearButton),mathview)
    clearButton.addEventHandler(ActionEvent.ACTION, {
      (_:ActionEvent) =>
        formula = None
        mathview.setVisible(false)
        line.rightProperty.set(null)
    })
    pickButton.addEventHandler(ActionEvent.ACTION, { (_:ActionEvent) =>
      if (currentlySelectedMath==null) {
        formula = None
        mathview.setVisible(false)
      } else {
        formula = Some(Formula(id = System.identityHashCode(currentlySelectedMath), math = currentlySelectedMath.getMath)) // TODO: should come from current theory!
        mathview.setVisible(true)
        mathview.setMath(formula.get.math)
      }
      line.setRight(currentlySelectedMath)
      valueProperty.fireValueChangedEvent()
    })
  }


  val editorFactory = new EditorFactory {
    override def create[T<:AnyRef](q: Question[T]): Editor[T] = {
      if (q.questionType==typeTag[FormulaQ])
        cast(q.answerType, new FormulaEditor())
      else
        Interactor.defaultEditorFactory.create(q)
    }
  }
}
