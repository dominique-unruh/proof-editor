package ui

import java.lang.Boolean
import javafx.beans.value
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.layout.VBox

import theory.{Formula, MutableTheory, Theory}
import ui.mathview.MathView
import misc.Utils.ImplicitConversions._
import trafo.TrafoInstance

import scala.collection.mutable

class TheoryView extends javafx.scene.layout.VBox {

  setSpacing(10)
  val theory = new MutableTheory()
  private var _selectedMathView = null : MathView
  private var _selectedFormula = null : Formula

  // TODO deprecate -> replace by getNodeForFormula
  def selectedMathView = _selectedMathView

  def selectedFormula = _selectedFormula : Formula

  def deleteFormula(formula:Formula) = {
    ???
  }

  def getNodeForFormula(formula:Formula) = ???

  def addFormula(formula:Formula) = {
    val form = theory.addFormula(formula)
    addFormulaToGUI(form)
  }

  private def addFormulaToGUI(form:Formula) = {
    val mathview = new MathView()
    mathview.setMath(form.math)
    // TODO use ScalaFX
    mathview.selectedProperty.onChange { (_, _, newValue) =>
        if (newValue) {
          _selectedMathView = mathview
          _selectedFormula = form // TODO: the formula might get updated! lookup current one
        }}
    getChildren.add(mathview)
  }

  def addTrafoInstance(trafo: TrafoInstance) = {
    val newFormulas = theory.addTrafoInstance(trafo)
    for (f <- newFormulas) addFormulaToGUI(f)
  }
}

