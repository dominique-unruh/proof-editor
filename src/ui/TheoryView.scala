package ui

import java.lang.Boolean
import javafx.beans.value
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.layout.VBox

import theory.{Formula, Theory}
import ui.mathview.MathView
import misc.Utils.JavaFXImplicits._
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

// TODO: move to theory.MutableTheory

class MutableTheory {

  import scala.collection.JavaConversions._
  import MutableTheory._
  private var theory = Theory()
  private var listeners = new java.util.LinkedList[Listener]

  def addListener(listener : Listener) = listeners.add(listener)
  def removeListener(listener : Listener) = listeners.remove(listener)

  def addFormula(formula:Formula) : Formula = {
    val (thy2,form2) = theory.addFormula(formula)
    theory = thy2
    for (l <- listeners) l.formulaAdded(form2)
    form2
  }

  def addTrafoInstance(trafo: TrafoInstance) : Seq[Formula] = {
    val (thy2,formulas) = theory.addTrafoInstance(trafo)
    for (f <- formulas) for (l <- listeners) l.formulaAdded(f)
    formulas
  }

  def updateFormula(formula:Formula) : (Formula,Formula) = {
    val (thy2,newForm,oldForm) = theory.updateFormula(formula)
    for (l <- listeners) l.formulaUpdated(newForm,oldForm)
    (newForm,oldForm)
  }


  /** Returns the current state of the theory as an immutable value */
  def getTheory = theory
}

object MutableTheory {
  trait Listener {
    def formulaAdded(formula:Formula) : Unit
    def formulaUpdated(newFormula:Formula, oldFormula:Formula) : Unit
  }
}
