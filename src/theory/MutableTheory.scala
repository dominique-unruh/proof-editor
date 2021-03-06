package theory

import misc.{Log, Utils}
import theory.Theory.{FormulaId, TrafoId}
import trafo.TrafoInstance

import scala.collection.mutable

/** Thread safe. */
class MutableTheory {
  import MutableTheory._

  private var theory = Theory()
  private var listeners = mutable.ListBuffer[Listener]()

  def addListener(listener : Listener) = synchronized { listeners += listener }
  def removeListener(listener : Listener) = synchronized { listeners -= listener }

  def addAxiom(formula:Formula) : Formula = synchronized {
    val (thy2,form2) = theory.addAxiom(formula)
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
    val (thy2,trafo2,formulas) = theory.addTrafoInstance(trafo)
    theory = thy2
    Utils.invokeListeners[Listener](listeners,_.transformationAdded(trafo2,formulas))
    formulas
  }

//  def updateFormula(formula:Formula) : (Formula,Formula) = synchronized {
//    val (thy2,newForm,oldForm) = theory.updateFormula(formula)
//    for (l <- listeners) l.formulaUpdated(newForm,oldForm)
//    (newForm,oldForm)
//  }

  def setTheory(thy: Theory): Unit = synchronized {
    theory = thy
    for (l <- listeners) l.theoryCleared()
    val ids = mutable.HashSet[Int]()
    ids ++= thy.formulas.values.map(_.id.id)
    ids ++= thy.transformations.values.map(_.id.id)
    val seenFormulas = mutable.HashSet[FormulaId]()
    for (id <- ids.toSeq.sorted) {
      for (t <- thy.transformations.get(TrafoId(id))) {
        val newFormulas = t.formulas.filter(f => !seenFormulas.contains(f.id))
        Utils.invokeListeners[Listener](listeners, _.transformationAdded(t, newFormulas))
        seenFormulas ++= t.formulas.map(_.id)
      }
      for (f <- thy.formulas.get(FormulaId(id)) if !seenFormulas.contains(FormulaId(id))) {
        seenFormulas += FormulaId(id)
        Utils.invokeListeners[Listener](listeners, _.formulaAdded(f))
      }
    }
//    for (t <- theory.transformations.values.toSeq.sortBy(_.id)) {
//      val newFormulas = t.formulas.filter(f => !seen.contains(f.id))
//      Utils.invokeListeners[Listener](listeners, _.transformationAdded(t, newFormulas))
//      seen ++= t.formulas.map(_.id)
//    }
//    for (f <- theory.formulas.values.toSeq.sortBy(_.id) if !seen.contains(f.id))
//      Utils.invokeListeners[Listener](listeners,_.formulaAdded(f))
//    for (t <- theory.transformations.values.toSeq.sortBy(_.id))
//      Utils.invokeListeners[Listener](listeners,_.transformationAdded(t,Nil))
  }

  /** Returns the current state of the theory as an immutable value */
  def getTheory = theory
}

object MutableTheory {
  trait Listener {
    def formulaDeleted(formula: Formula) : Unit
    def theoryCleared() : Unit
    def formulaAdded(formula:Formula) : Unit
    def transformationAdded(trafo:TrafoInstance, newFormulas:Seq[Formula]) : Unit
    def formulaUpdated(newFormula:Formula, oldFormula:Formula) : Unit
  }
}