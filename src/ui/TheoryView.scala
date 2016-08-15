package ui

import javafx.event
import javafx.scene.input.{MouseButton, MouseEvent}

import misc.Log
import misc.Utils.ImplicitConversions._
import theory.{Formula, MutableTheory}
import ui.mathview.MathEdit

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalafx.geometry.Insets
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
  private val nodes = new mutable.HashMap[Int,MathEdit]

  def selectedFormula = selectedFormulaId.map(theory.getTheory.formulas(_))
  def selectedMathEdit = selectedFormulaId.map(nodes)
  def getMathEditForFormula(formula:Formula) = nodes.get(formula.id)

  theory.addListener(new MutableTheory.Listener {
    override def formulaAdded(formula: Formula): Unit = addFormulaToGUI(formula)
    override def formulaUpdated(newFormula: Formula, oldFormula: Formula): Unit = {
      assert(newFormula.id==oldFormula.id)
      val edit = nodes(newFormula.id)
      edit.setMath(newFormula.math)
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
    val mathedit = new MathEdit()
    mathedit.setMath(form.math)
    mathedit.focused.onChange { (_, oldVal,newVal) =>
      if (newVal) selectedFormulaId = Some(id) }
    mathedit.addEventHandler(MouseEvent.MOUSE_CLICKED, { e:MouseEvent =>
      if (e.getClickCount==2 && e.getButton==MouseButton.PRIMARY)
        for (l<-doubleClickListeners)
          try l(theory.getTheory.formulas(id))
          catch { case e:Throwable => Log.stackTrace("in doubleClickListener",e) }
    })
    nodes.update(id,mathedit)
    children.add(mathedit)
  }
}