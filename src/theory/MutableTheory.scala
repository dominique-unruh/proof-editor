package theory

import misc.Log
import trafo.TrafoInstance

/** Thread safe. */
class MutableTheory {

  import MutableTheory._

  import scala.collection.JavaConversions._
  private var theory = Theory()
  private var listeners = new java.util.LinkedList[Listener]

  def addListener(listener : Listener) = synchronized { listeners.add(listener) }
  def removeListener(listener : Listener) = synchronized { listeners.remove(listener) }

  def addFormula(formula:Formula) : Formula = synchronized {
    val (thy2,form2) = theory.addFormula(formula)
    theory = thy2
    for (l <- listeners) l.formulaAdded(form2)
    form2
  }

  def deleteFormula(formula: Formula): Unit = synchronized {
    val (thy2,form2) = theory.deleteFormula(formula)
    theory = thy2
    for (l <- listeners) l.formulaDeleted(form2)
  }

  def addTrafoInstance(trafo: TrafoInstance) : Seq[Formula] = synchronized {
    Log.debug("pre add TI",theory.transformations)
    val (thy2,trafo2,formulas) = theory.addTrafoInstance(trafo)
    theory = thy2
    Log.debug("post add TI",theory.transformations)
    Log.debug("trafo added",trafo2.id,trafo2)
    for (f <- formulas) for (l <- listeners) l.formulaAdded(f)
    formulas
  }

  def updateFormula(formula:Formula) : (Formula,Formula) = synchronized {
    val (thy2,newForm,oldForm) = theory.updateFormula(formula)
    for (l <- listeners) l.formulaUpdated(newForm,oldForm)
    (newForm,oldForm)
  }

  def setTheory(thy: Theory): Unit = synchronized {
    theory = thy
    for (l <- listeners) l.theoryCleared()
    for (f <- theory.formulas.values.toSeq.sortBy(_.id))
      for (l <- listeners)
        l.formulaAdded(f)
  }

  /** Returns the current state of the theory as an immutable value */
  def getTheory = theory
}

object MutableTheory {
  trait Listener {
    def formulaDeleted(formula: Formula) : Unit
    def theoryCleared() : Unit
    def formulaAdded(formula:Formula) : Unit
    def formulaUpdated(newFormula:Formula, oldFormula:Formula) : Unit
  }
}