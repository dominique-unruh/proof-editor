package testapp
import java.io.{PrintWriter, StringWriter}
import java.lang.System.out
import java.util.IllegalFormatCodePointException
import javafx.application.{Application, Platform}
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader}
import javafx.geometry.Insets
import javafx.scene.control._
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.web.WebView
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import cmathml._
import com.sun.javafx.webkit.WebConsoleListener
import misc.GetterSetterProperty
import misc.Utils.ImplicitConversions._
import testapp.TestApp.TrafoChoice
import theory.Formula
import trafo._
import ui.Interactor.{Editor, EditorFactory}
import ui.mathview.MathEdit
import ui.{ConnectingLine, Interactor, TheoryView}
import z3.Z3

import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit
import scalafx.application.JFXApp

object TestApp {
  def main(args: Array[String]) = Application.launch(classOf[TestApp], args:_*)
  final case class TrafoChoice(label:String, trafo:Transformation) {
    override def toString = label
  }

}


class TestApp extends Application {
  val examples = List(
    CMathML.equal(CMathML.plus(CI("x"), CI("y")), CMathML.plus(CI("y"), CN(-1))),
    CI("x").negate()
  )

  val transformations = FXCollections.observableArrayList(
    TrafoChoice("Simplify formula", new SimplifyTrafo),
    TrafoChoice("Equality check", new CheckEqualTrafo)
  )



  @FXML protected[this] var theoryView = null : TheoryView
  @FXML protected[this] var logArea = null: TextArea
  @FXML protected[this] var interactor = null: Interactor[TrafoInstance]
  @FXML protected[this] var overlay = null : javafx.scene.layout.Pane
  @FXML protected[this] var trafoChoice = null : ChoiceBox[TrafoChoice]
  @FXML protected[this] var useButton = null : Button

  private def errorPopup(msg: String): Unit =
    new Alert(Alert.AlertType.ERROR, msg).showAndWait()

  @FXML private def useButtonClicked(event: ActionEvent) = {
    theoryView.addTrafoInstance(interactor.result.get.get)
  }

  @FXML
  private def quit(event: ActionEvent): Unit = {
    Platform.exit()
  }

//  @FXML
//  private def editSelection(event: ActionEvent): Unit = {
//    val math = theoryView.selectedMathEdit
//    if (math == null) {
//      log("No selected ui.mathview"); return
//    }
//    val sel = math.selection.value
//    if (sel.isEmpty) {
//      log("No selection"); return
//    }
//    math.setMath(math.mathDoc.toCMathML, Some(sel.get))
//  }

  @FXML
  private def newFromSelection(event: ActionEvent): Unit = {
    val math = theoryView.selectedMathEdit
    if (math == null) {
      log("No selected ui.mathview"); return
    }
    val sel = math.selection.value
    if (sel.isEmpty) {
      log("No selection"); return
    }
    val m = math.selection.value.get.toCMathML
    theoryView.addFormula(Formula(m))
  }

  /** Only invoke methods in JavaFX thread! */
  private lazy val z3 = new Z3(Map())

  @FXML
  private def simplify(event: ActionEvent): Unit = {
    val math = theoryView.selectedMathEdit
    if (math == null) {
      log("No selected ui.mathview"); return
    }
    val expr = z3.fromCMathML(math.mathDoc.toCMathML)
    val simp = expr.simplify
    val simp2 = simp.toCMathML
    theoryView.addFormula(Formula(simp2))
  }

  @FXML
  private def deleteFormula(event: ActionEvent): Unit = {
    val math = theoryView.selectedFormula
    if (math == null) {
      errorPopup("No formula selected")
    }
    theoryView.deleteFormula(math)
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

//    Logger.getLogger("").log(Level.WARNING, "logging test")
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

    println(theoryView)
    for (m <- examples) theoryView.addFormula(Formula(m))

    primaryStage.getScene.getStylesheets.add(getClass().getResource("testapp.css").toExternalForm())

    useButton.setDisable(true)
    interactor.result.addListener(new ChangeListener[Option[TrafoInstance]] {
      override def changed(observable: ObservableValue[_ <: Option[TrafoInstance]], oldValue: Option[TrafoInstance], newValue: Option[TrafoInstance]): Unit = {
//        println("interactor result change: "+newValue)
        useButton.setDisable(newValue.isEmpty)
    }})

    trafoChoice.setItems(transformations)
    trafoChoice.getSelectionModel.selectedItemProperty.addListener(new ChangeListener[TrafoChoice] {
      override def changed(observable: ObservableValue[_ <: TrafoChoice], oldValue: TrafoChoice, newValue: TrafoChoice): Unit =
        interactor.setInteraction(newValue.trafo.createInteractive)
    })
    trafoChoice.getSelectionModel.select(0)

//    copyTrafo(null)
    primaryStage.show
  }

  class FormulaEditor extends HBox with Editor[Option[Formula]] {
    override val editedType: TypeTag[Option[Formula]] = typeTag[Option[Formula]]
    override val questionType = typeTag[FormulaQ]
    val pickButton = new Button("Pick")
    pickButton.setPadding(Insets.EMPTY)
    val clearButton = new Button("Clear")
    clearButton.setPadding(Insets.EMPTY)
    val mathedit = new MathEdit()
    var formula : Option[Formula] = None
    val line = new ConnectingLine(this, overlay)
    line.setLeft(mathedit)

    override val valueProperty: GetterSetterProperty[Option[Formula]] = new GetterSetterProperty[Option[Formula]] {
      override protected def getter: Option[Formula] = formula
      override protected def setter(value: Option[Formula]): Unit = {
        formula = value
        if (formula.isEmpty) mathedit.setVisible(false)
        else { mathedit.setVisible(true); mathedit.setMath(formula.get.math) }
      }
    }

//    mathedit.setMath(CN(0)) // Otherwise the ui.mathview will not be resized to something small
    mathedit.setVisible(false)
    getChildren.addAll(new VBox(1,pickButton,clearButton),mathedit)
    clearButton.addEventHandler(ActionEvent.ACTION, {
      (_:ActionEvent) =>
        formula = None
        mathedit.setVisible(false)
        line.rightProperty.set(null)
        valueProperty.fireValueChangedEvent()
    })
    pickButton.addEventHandler(ActionEvent.ACTION, { (_:ActionEvent) =>
      if (theoryView.selectedFormula==null) {
        formula = None
        mathedit.setVisible(false)
      } else {
        formula = Some(theoryView.selectedFormula)
        mathedit.setVisible(true)
        mathedit.setMath(formula.get.math)
      }
      line.setRight(theoryView.selectedMathEdit)
      valueProperty.fireValueChangedEvent()
    })
  }

  class ShowFormula(q:ShowFormulaQ) extends HBox with Editor[BoxedUnit] {
    override val editedType: TypeTag[BoxedUnit] = typeTag[BoxedUnit]
    override val questionType = typeTag[ShowFormulaQ]
    val mathedit = new MathEdit()
    var formula : Option[Formula] = None
    override val valueProperty = new SimpleObjectProperty[BoxedUnit](BoxedUnit.UNIT)
    mathedit.setMath(q.formula.math)
    getChildren.add(mathedit)
  }


  val editorFactory = new EditorFactory {
    override def create[T<:AnyRef](q: Question[T]): Editor[T] = {
      if (q.questionType==typeTag[FormulaQ]) {
        val q2 = q.asInstanceOf[FormulaQ]
        val edit = /*if (q2.newFormula != null) new FreshFormulaGenerator(q2)
        else*/ new FormulaEditor()
        cast(q.answerType, edit)
      } else if (q.questionType==typeTag[ShowFormulaQ]) {
        val edit = new ShowFormula(q.asInstanceOf[ShowFormulaQ])
        cast(q.answerType, edit)
      } else
        Interactor.defaultEditorFactory.create(q)
    }
  }
}
