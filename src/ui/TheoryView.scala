package ui

import java.lang.Boolean
import javafx.beans.value
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.layout.VBox

import theory.{Formula, MutableTheory, Theory}
import ui.mathview.{MathEdit, MathView}
import misc.Utils.ImplicitConversions._
import trafo.TrafoInstance

import scala.collection.mutable
import scalafx.scene.control.{TextField, TextInputControl}

class TheoryView extends javafx.scene.layout.VBox {

  setSpacing(10)
  val theory = new MutableTheory()
  private var _selectedMathEdit = null : MathEdit
  private var _selectedFormula = null : Formula

  // TODO deprecate -> replace by getNodeForFormula
  def selectedMathEdit = _selectedMathEdit

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
    val mathedit = new MathEdit()
    mathedit.setMath(form.math)
    mathedit.focused.onChange {
      _selectedMathEdit = mathedit
      _selectedFormula = form // TODO: the formula might get updated! lookup current one
    }
//    mathedit.selection.onChange { (_, _, newValue) =>
//        if (newValue.isDefined) {
//          _selectedMathEdit = mathedit
//          _selectedFormula = form // TODO: the formula might get updated! lookup current one
//        }}
    getChildren.add(mathedit)
  }

  def addTrafoInstance(trafo: TrafoInstance) = {
    val newFormulas = theory.addTrafoInstance(trafo)
    for (f <- newFormulas) addFormulaToGUI(f)
  }
}

