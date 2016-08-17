package ui

import javafx.event
import javafx.scene.control.Label
import javafx.scene.input.{MouseButton, MouseEvent}
import javafx.scene.layout.HBox

import cmathml.CMathML.internal
import cmathml.{CI, CN, Logic}
import misc.Log
import misc.Utils.ImplicitConversions._
import theory.{Formula, MutableTheory}
import trafo.TrafoInstance
import ui.TheoryView.{FormulaBox, TrafoBox}
import ui.mathview.MathEdit

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.input
import scalafx.scene.layout.VBox
import scalafx.Includes._
import scalafx.event.EventType

class TheoryView extends VBox {
  spacing = 10
  fillWidth = false
  padding = Insets(10)

  val theory = new MutableTheory()
  private var selectedFormulaId = None : Option[Int]
  private val nodes = new mutable.HashMap[Int,FormulaBox]

  def selectedFormula = selectedFormulaId.map(theory.getTheory.formulas(_))
  def selectedMathEdit = selectedFormulaId.map(nodes(_).mathEdit)
  def getMathEditForFormula(formula:Formula) = nodes.get(formula.id)

  theory.addListener(new MutableTheory.Listener {
    override def formulaAdded(formula: Formula): Unit = addFormulaToGUI(formula)
    override def transformationAdded(trafo: TrafoInstance, newFormulas: Seq[Formula]): Unit = {
      children += new TrafoBox(trafo)
      for (f <- newFormulas) addFormulaToGUI(f)
    }
    override def formulaUpdated(newFormula: Formula, oldFormula: Formula): Unit = {
      assert(newFormula.id==oldFormula.id)
      val edit = nodes(newFormula.id)
      edit.mathEdit.setMath(newFormula.math)
    }
    override def theoryCleared(): Unit = {
      children.clear()
      selectedFormulaId = None
      nodes.clear
    }
    override def formulaDeleted(formula: Formula): Unit = {
      val id = formula.id
      if (selectedFormulaId.contains(id)) selectedFormulaId = None
      children.remove(nodes(id))
      nodes -= id
    }
  })

  private val doubleClickListeners = ListBuffer[Formula => Unit]()
  def addDoubleClickListener(listener : Formula => Unit) = doubleClickListeners += listener

  private def addFormulaToGUI(form:Formula) = {
    val id = form.id
    assert(!nodes.contains(id),"formula with id "+id+" added twice")
//    val mathedit = new MathEdit()
    val formulaBox = new FormulaBox(id)
    formulaBox.mathEdit.setMath(form.math)
    formulaBox.mathEdit.focused.onChange { (_, oldVal,newVal) =>
      if (newVal) selectedFormulaId = Some(id) }
    formulaBox.mathEdit.addEventHandler(MouseEvent.MOUSE_CLICKED, { e:MouseEvent =>
      if (e.getClickCount==2 && e.getButton==MouseButton.PRIMARY)
        for (l<-doubleClickListeners)
          try l(theory.getTheory.formulas(id))
          catch { case e:Throwable => Log.stackTrace("in doubleClickListener",e) }
    })
    nodes.update(id,formulaBox)
    children.add(formulaBox)
  }
}

object TheoryView {
  private[TheoryView] class FormulaBox(val id:Int) extends HBox {
    val mathEdit = new MathEdit()
    alignmentProperty.setValue(Pos.CenterLeft)
    getChildren.addAll(new Label(s"($id) "),mathEdit)
  }
  private[TheoryView] class TrafoBox(val trafo: TrafoInstance) extends HBox {
    val mathEdit = new MathEdit()
    alignmentProperty.setValue(Pos.CenterLeft)
    val ids = trafo.formulas.map(f => internal.formulaRef(f.id))
    val rel = Logic.instantiateLambda(trafo.relation.relatingFormula, ids :_*)
    mathEdit.setMath(rel)
    getChildren.addAll(new Label(s"By ${trafo.shortDescription} we have: "),mathEdit)
  }
}
