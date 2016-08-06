package ui

import theory.{Formula, MutableTheory}
import ui.mathview.MathEdit

import scala.collection.mutable
import scalafx.geometry.Insets
import scalafx.scene.layout.VBox

class TheoryView extends VBox {
  spacing = 10
  fillWidth = false
  padding = Insets(10)

  val theory = new MutableTheory()
  private var selectedFormulaId = None : Option[Int]
  private val nodes = new mutable.HashMap[Int,MathEdit]

  def selectedFormula = selectedFormulaId.map(theory.getTheory.formulas(_))
  def selectedMathEdit = selectedFormulaId.map(nodes)
  def getMathEditForFormula(formula:Formula) = nodes(formula.id)

  theory.addListener(new MutableTheory.Listener {
    override def formulaAdded(formula: Formula): Unit = addFormulaToGUI(formula)
    override def formulaUpdated(newFormula: Formula, oldFormula: Formula): Unit = {
      assert(newFormula.id==oldFormula.id)
      val edit = nodes(newFormula.id)
      edit.setMath(newFormula.math)
    }
    override def theoryCleared(): Unit = {
      children.clear
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

  private def addFormulaToGUI(form:Formula) = {
//    println("addFormulaToGUI",form)
    val id = form.id
    assert(!nodes.contains(id),"formula with id "+id+" added twice")
    val mathedit = new MathEdit()
    mathedit.setMath(form.math)
    mathedit.focused.onChange { (_, oldVal,newVal) =>
//      if (oldVal) selectedFormulaId = None
      if (newVal) selectedFormulaId = Some(id) }
    nodes.update(id,mathedit)
    children.add(mathedit)
  }

//  def addTrafoInstance(trafo: TrafoInstance) = {
//    val newFormulas = theory.addTrafoInstance(trafo)
////    for (f <- newFormulas) addFormulaToGUI(f)
//  }
}

