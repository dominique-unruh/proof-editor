package ui

import java.lang.Boolean
import javafx.beans.value
import javafx.beans.value.{ChangeListener, ObservableValue}

import javafx.scene.layout.VBox
import theory.{Formula, Theory}
import ui.mathview.MathView
import misc.Utils.JavaFXImplicits._

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
    val mathview = new MathView()
    mathview.setMath(form.math)
    // TODO use ScalaFX
    mathview.selectedProperty.addListener(new ChangeListener[Boolean] {
      override def changed(observable: ObservableValue[_ <: Boolean], oldValue: Boolean, newValue: Boolean): Unit =
        if (newValue) {
          _selectedMathView = mathview
          _selectedFormula = form // TODO: the formula might get updated! lookup current one
        }
    })
//    mathview.addEditedListener(m => {
//      println("edited", m, mw);
//      mw.setMath(mw.getMath.replace(mw.editPath.get, m))
//    })
    getChildren.add(mathview)
  }
}


class MutableTheory {
  import scala.collection.JavaConversions._
  import MutableTheory._
  private var theory = Theory()
  private var listeners = new java.util.LinkedList[Listener]

  def addListener(listener : Listener) = listeners.add(listener)
  def removeListener(listener : Listener) = listeners.remove(listener)

  def addFormula(formula:Formula) = {
    val (thy2,form2) = theory.addFormula(formula)
    theory = thy2
    for (l <- listeners) l.formulaAdded(form2)
    form2
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
