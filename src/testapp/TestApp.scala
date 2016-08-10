package testapp
import java.io.{PrintWriter, StringWriter}
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.ActionEvent
import javafx.geometry.Insets
import javafx.scene.control._
import javafx.scene.layout.{HBox, StackPane, VBox}

import cmathml._
import misc.Utils.ImplicitConversions._
import misc.{GetterSetterProperty, Log}
import testapp.TestApp.TrafoChoice
import theory.{Formula, Theory}
import trafo._
import ui.Interactor.{Editor, EditorFactory}
import ui.mathview.MathEdit
import ui.{ConnectingLine, Interactor, TheoryView}
import z3.Z3

import scala.reflect.runtime.universe._
import scala.runtime.BoxedUnit
import scala.xml.XML
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene.input.KeyCombination
import scalafx.scene.layout.{Pane, Priority}
import scalafx.scene.{control, layout}

object Launch {
  def main(args: Array[String]) = (new TestApp).main(args)
}

object TestApp {
//  Z3 // To make sure we notice missing libraries right away

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


  lazy val newFormulaEdit = new MathEdit {
    editable.value = Some(mathDoc.root) }

  def insertNewFormula() : Unit = {
    val math = newFormulaEdit.mathDoc.root.toCMathML
    if (!math.isValidMath) {
      errorPopup("The formula you entered is not valid math")
      return
    }
    theoryView.theory.addFormula(Formula(math))
  }

  private val z3 = new Z3

  lazy val theoryPane = new layout.VBox {
    children = List(
      new layout.HBox {
        this.children = List(
          new control.Label("New formula:"),
          newFormulaEdit,
          new control.Button("Insert") {
            onAction = handle(insertNewFormula())
          })},
      new control.ScrollPane {
        fitToWidth = true
        content = theoryView
      })
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
    menus = List(new control.Menu("_File") {
      items = List(new control.MenuItem("_Quit") {
        onAction = handle(stage.hide())
        accelerator = KeyCombination("Shortcut+Q") //new KeyCodeCombination(KeyCode.Q,KeyCombination.ShortcutDown)
      })
    })}

  lazy val toolbar = new control.ToolBar {
    content = List(
      new control.Button("New from selection") { onAction = handle(newFromSelection()) },
      new control.Button("Delete") { onAction = handle(deleteFormula()) }
    )}

  private lazy val transformations = ObservableBuffer(
    TrafoChoice("Extract subterm", new SubtermTrafo),
    TrafoChoice("Edit formula", new EditFormulaTrafo),
    TrafoChoice("Simplify formula", new SimplifyTrafo)
  )



  private lazy val interactor = new Interactor[TrafoInstance] {
  setEditorFactory(editorFactory)
  result.addListener(new ChangeListener[Option[TrafoInstance]] {
    override def changed(observable: ObservableValue[_ <: Option[TrafoInstance]], oldValue: Option[TrafoInstance], newValue: Option[TrafoInstance]): Unit = {
      useButton.setDisable(newValue.isEmpty)
    }})
  }
  private lazy val overlay = new Pane() { mouseTransparent = true }

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

//  private val z3 = new Z3(Map())

  private def deleteFormula(): Unit = {
    theoryView.selectedFormula match {
      case None => errorPopup("No formula selected")
      case Some(formula) => theoryView.theory.deleteFormula(formula)
  }}

  def actualException(e: Throwable): Throwable =
    if (e.getCause != null) actualException(e.getCause) else e

  def saveTheory(): Unit = {
    Log.info("saving theory")
    val xml = theoryView.theory.getTheory.toXML
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

  abstract class GenericFormulaEditor extends HBox {
    val pickButton = new Button("Pick")
    pickButton.setPadding(Insets.EMPTY)
    val clearButton = new Button("Clear")
    clearButton.setPadding(Insets.EMPTY)
    val mathedit = new MathEdit()
    private var _formula : Option[Formula] = None
    def formula : Option[Formula] = _formula
    private var _selection : Path = Path.empty
    val line = new ConnectingLine(this, overlay)
    line.setLeft(mathedit)

    def formulaChanged()
    def selectionChanged() = {}

    def formula_=(value : Option[Formula]) = value match {
      case None =>
        _formula = None
        mathedit.setVisible(false)
        line.rightProperty.set(null)
      case Some(form) =>
        _formula = value
        mathedit.setVisible(true)
        mathedit.setMath(form.math)
        line.setRight(theoryView.selectedMathEdit.get)
    }

    def selection = _selection
    var _updatingSelection = false
    def selection_=(sel: Path) = {
      _selection = sel
      try {
        _updatingSelection = true
        mathedit.selection.value = Some(mathedit.mathDoc.subterm(sel))
      } finally {
        _updatingSelection = false
      }
    }

    mathedit.selection.onChange { (_,_,sel) => sel match {
      case None =>
      case Some(sel) =>
        if (!_updatingSelection) {
          _selection = sel.getPath
          selectionChanged()
        }
    }}

    mathedit.setVisible(false)
    getChildren.addAll(new VBox(1,pickButton,clearButton),mathedit)
    clearButton.addEventHandler(ActionEvent.ACTION, {
      (_:ActionEvent) =>
        formula = None
        formulaChanged()
    })
    pickButton.addEventHandler(ActionEvent.ACTION, { (_:ActionEvent) =>
      theoryView.selectedFormula match {
        case None =>
          formula = None
        case Some(form) =>
          formula = Some(form)
          selection = Path.empty
      }
      formulaChanged()
    })
  }

  class FormulaEditor extends GenericFormulaEditor with Editor[Option[Formula]] {
    override val editedType: TypeTag[Option[Formula]] = typeTag[Option[Formula]]
    override val questionType = typeTag[FormulaQ]
    override val valueProperty: GetterSetterProperty[Option[Formula]] = new GetterSetterProperty[Option[Formula]] {
      override protected def getter: Option[Formula] = formula
      override protected def setter(value: Option[Formula]): Unit = formula = value
    }

    override def formulaChanged() = valueProperty.fireValueChangedEvent()
  }

  class FormulaSubtermEditor extends GenericFormulaEditor with Editor[Option[(Formula,Path)]] {
    override val editedType = typeTag[Option[(Formula,Path)]]
    override val questionType = typeTag[FormulaSubtermQ]

    override val valueProperty: GetterSetterProperty[Option[(Formula,Path)]] = new GetterSetterProperty[Option[(Formula,Path)]] {
      override protected def getter = formula.map((_,selection))
      override protected def setter(value: Option[(Formula,Path)]): Unit = value match {
        case None => formula = None
        case Some((form,path)) =>
          formula = Some(form)
          selection = path
      }
    }

    override def formulaChanged() = valueProperty.fireValueChangedEvent()
    override def selectionChanged() = valueProperty.fireValueChangedEvent()
  }

  class MathEditor extends HBox with Editor[CMathML] {
    override val editedType: TypeTag[CMathML] = typeTag[CMathML]
    override val questionType = typeTag[MathQ]
    val mathedit = new MathEdit()
    var math : CMathML = CNone()

    override val valueProperty: GetterSetterProperty[CMathML] = new GetterSetterProperty[CMathML] {
      override protected def getter: CMathML = math
      override protected def setter(value: CMathML): Unit = {
        math = value
        mathedit.setMathEditable(value)
      }
    }

    valueProperty.value = CNone()

    mathedit.onChange { () => // TODO: for efficiency reasons, we should update only occasionally!
      math = mathedit.mathDoc.root.toCMathML
      valueProperty.fireValueChangedEvent()
    }

    getChildren.addAll(mathedit)
  }

  class ShowFormula(q:ShowFormulaQ) extends HBox with Editor[BoxedUnit] {
    Log.debug("new ShowFormula",q)
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
        val edit = new FormulaEditor()
        cast(q.answerType, edit)
      } else if (q.questionType==typeTag[FormulaSubtermQ]) {
          val q2 = q.asInstanceOf[FormulaSubtermQ]
          val edit = new FormulaSubtermEditor()
          cast(q.answerType, edit)
      } else if (q.questionType==typeTag[ShowFormulaQ]) {
        val edit = new ShowFormula(q.asInstanceOf[ShowFormulaQ])
        cast(q.answerType, edit)
      } else if (q.questionType==typeTag[MathQ]) {
        val edit = new MathEditor()
        cast(q.answerType, edit)
      } else
        Interactor.defaultEditorFactory.create(q)
    }
  }
}
