package testapp
import java.io.{File, FileOutputStream, PrintWriter, StringWriter}
import java.lang.System.out
import java.nio.channels.{Channels, FileChannel}
import java.util.IllegalFormatCodePointException
import javafx.application.{Application, Platform}
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.Insets
import javafx.scene.control._
import javafx.scene.layout.{HBox, StackPane, VBox}
import javafx.scene.web.WebView
import javafx.scene.{Group, Parent, Scene}
import javafx.stage.{Stage, WindowEvent}

import scalafx.Includes._
import cmathml._
import com.sun.javafx.webkit.WebConsoleListener
import misc.GetterSetterProperty
import misc.Utils.ImplicitConversions._
import testapp.TestApp.TrafoChoice
import theory.{Formula, Theory}
import trafo._
import ui.Interactor.{Editor, EditorFactory}
import ui.mathview.MathEdit
import ui.{ConnectingLine, Interactor, TheoryView}
import z3.Z3
import misc.Utils.ImplicitConversions._

import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit
import scala.util.control.Exception._
import scala.xml.{Utility, XML}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene
import scalafx.scene.{control, layout}
import scalafx.scene.layout.{Pane, Priority}

object Launch {
  def main(args: Array[String]) = (new TestApp).main(args)
}

object TestApp {
  final case class TrafoChoice(label:String, trafo:Transformation) {
    override def toString = label
  }

  val examples = List(
    CMathML.equal(CMathML.plus(CI("x"), CI("y")), CMathML.plus(CI("y"), CN(-1))),
    CI("x").negate()
  )
}


class TestApp extends JFXApp {
  stage = new PrimaryStage {
    width = 800
    height = 600
    title = "Proof Editor"
    onHidden = handle(saveTheory())
    scene = new scalafx.scene.Scene(new StackPane) {
      stylesheets += getClass.getResource("testapp.css").toExternalForm
      content = new layout.VBox {
        fillWidth = true
        children = List(
          menubar,
          toolbar,
          new control.SplitPane { // dividerPositions="0.33, 0.66" VBox.vgrow="ALWAYS">
            vgrow = Priority.Always
            items += interactorPane
            items += theoryPane
            dividerPositions = 0.2
          })
      }
      content += overlay
    }
  }

  lazy val theoryPane = new control.ScrollPane {
    fitToWidth = true
    content = theoryView
  }

  lazy val interactorPane = new control.ScrollPane {
    fitToWidth = true
    content = new layout.VBox(
      trafoChoice,
      interactor,
      useButton)
  }

  lazy val useButton = new control.Button("Use") {
    disable = true
    onAction = handle {useButtonClicked()}
  }


  lazy val menubar = new control.MenuBar {
    menus += new control.Menu("_File")
  }
  lazy val toolbar = new control.ToolBar {
    content = List(
      new control.Button("New from selection") { onAction = handle(newFromSelection()) },
      new control.Button("Delete") { onAction = handle(deleteFormula()) }
    )}

  private lazy val transformations = ObservableBuffer(
    TrafoChoice("Simplify formula", new SimplifyTrafo),
    TrafoChoice("Equality check", new CheckEqualTrafo)
  )



//  @FXML protected[this] var theoryView = null : TheoryView
//  @FXML protected[this] var logArea = null: TextArea
  private lazy val interactor = new Interactor[TrafoInstance] {
  setEditorFactory(editorFactory)
  result.addListener(new ChangeListener[Option[TrafoInstance]] {
    override def changed(observable: ObservableValue[_ <: Option[TrafoInstance]], oldValue: Option[TrafoInstance], newValue: Option[TrafoInstance]): Unit = {
      //        println("interactor result change: "+newValue)
      useButton.setDisable(newValue.isEmpty)
    }})
}
  private lazy val overlay = new Pane() { mouseTransparent = true }
//  @FXML protected[this] var trafoChoice = null : ChoiceBox[TrafoChoice]
//  @FXML protected[this] var useButton = null : Button
//  @FXML protected[this] var rootPane = null : StackPane

  private lazy val theoryView = new TheoryView

  private lazy val trafoChoice = new control.ChoiceBox[TrafoChoice] {
    items = transformations
    selectionModel.value.selectedItemProperty.addListener(new ChangeListener[TrafoChoice] {
      override def changed(observable: ObservableValue[_ <: TrafoChoice], oldValue: TrafoChoice, newValue: TrafoChoice): Unit =
        interactor.setInteraction(newValue.trafo.createInteractive)})
    selectionModel.value.select(0)
  }


  private def errorPopup(msg: String): Unit =
    new Alert(Alert.AlertType.ERROR, msg).showAndWait()

  private def okCancelPopup(msg: String): Boolean =
    new Alert(Alert.AlertType.CONFIRMATION, msg).showAndWait().get == ButtonType.OK

  private def useButtonClicked() : Unit = {
    theoryView.theory.addTrafoInstance(interactor.result.get.get)
  }

//  @FXML
//  private def quit(event: ActionEvent): Unit = {
//    Platform.exit()
//  }

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

  private def newFromSelection(): Unit = {
    theoryView.selectedMathEdit match {
      case None =>
      case Some(math) =>
        val sel = math.selection.value
        if (sel.isEmpty) {
          return
        }
        val m = math.selection.value.get.toCMathML
        theoryView.theory.addFormula(Formula(m))
    }
  }

  /** Only invoke methods in JavaFX thread! */
  private val z3 = new Z3(Map())

//  private def simplify(event: ActionEvent): Unit = {
//    theoryView.selectedMathEdit match {
//      case None =>
//      case Some(math) =>
//        val expr = z3.fromCMathML(math.mathDoc.toCMathML)
//        val simp = expr.simplify
//        val simp2 = simp.toCMathML
//        theoryView.theory.addFormula(Formula(simp2))
//  }}

  private def deleteFormula(): Unit = {
    theoryView.selectedFormula match {
      case None => errorPopup("No formula selected")
      case Some(formula) => theoryView.theory.deleteFormula(formula)
  }}

//  def log(msg: String, numLines: Int = -1) = {
//    var msg2: String = msg
//    if (numLines >= 0) {
//      var idx = 0
//      for (i <- 1 to numLines)
//        if (idx != -1) idx = msg2.indexOf('\n', idx) + 1
//      msg2 = if (idx == -1) msg2 else msg2.substring(0, idx)
//    }
//    logArea.appendText(msg2)
//    logArea.appendText("\n")
//  }

  def actualException(e: Throwable): Throwable =
    if (e.getCause != null) actualException(e.getCause) else e

  def saveTheory(): Unit = {
//    println("saving",new File(".").getAbsolutePath)
    val xml = theoryView.theory.getTheory.toXML
//    val str = new scala.xml.PrettyPrinter(80,4).format(xml)
//    val fos = new FileOutputStream("theory.xml")
//    val w = Channels.newWriter(fos.getChannel(), "UTF-8")
//    ultimately(w.close()) {
//      w.write(str) }
    XML.save("theory.xml",xml,enc="UTF-8",xmlDecl=true)
  }

  def loadTheory(): Unit = {
    try {
      val xml = XML.loadFile("theory.xml")
      val thy = Theory.fromXML(xml)
      theoryView.theory.setTheory(thy)
    } catch { case e:Exception =>
      e.printStackTrace()
      val newThy = okCancelPopup(
        """Loading the theory from the previous session failed.
          |Start with a fresh theory?
          |(Looses the data from the previous session.)""".stripMargin)
      if (!newThy) sys.exit(1)
      for (m <- TestApp.examples) theoryView.theory.addFormula(Formula(m))
    }
  }

  Thread.currentThread().setUncaughtExceptionHandler({ (t: Thread, e: Throwable) =>
    val e2 = actualException(e)
    e2.printStackTrace()
    val sw = new StringWriter()
    e2.printStackTrace(new PrintWriter(sw))
  })

  loadTheory()

//  def start(primaryStage: Stage) {

//    primaryStage.setScene(new Scene(fxml, 800, 600))
//    primaryStage.setTitle("Proof editor")
//    WebConsoleListener.setDefaultListener((webView: WebView, message: String, lineNumber: Int, sourceId: String) =>
//      out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message))

//    rootPane.getChildren.add(overlay)


//    for (m <- examples) theoryView.theory.addFormula(Formula(m))


//    primaryStage.getScene.getStylesheets.add(getClass().getResource("testapp.css").toExternalForm())

//    useButton.setDisable(true)
//    interactor.result.addListener(new ChangeListener[Option[TrafoInstance]] {
//      override def changed(observable: ObservableValue[_ <: Option[TrafoInstance]], oldValue: Option[TrafoInstance], newValue: Option[TrafoInstance]): Unit = {
////        println("interactor result change: "+newValue)
//        useButton.setDisable(newValue.isEmpty)
//    }})


//    copyTrafo(null)
//    primaryStage.show
//  }

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
      theoryView.selectedFormula match {
      case None =>
        formula = None
        line.rightProperty.set(null)
        mathedit.setVisible(false)
      case Some(form) =>
        formula = Some(form)
        mathedit.setVisible(true)
        mathedit.setMath(form.math)
        line.setRight(theoryView.selectedMathEdit.get)
      }
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


  lazy val editorFactory = new EditorFactory {
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
