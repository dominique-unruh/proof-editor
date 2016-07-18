package theory

import trafo.TrafoInstance

/**
  * Created by unruh on 7/18/16.
  */
class MutableTheory {

  import MutableTheory._

  import scala.collection.JavaConversions._
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