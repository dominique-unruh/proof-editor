package theory

import cmathml.CMathML
import trafo.TrafoInstance

import scala.collection.mutable.ListBuffer

case class Theory(val counter : Int, val formulas : Map[Int,Formula]) {

  def addFormula(formula:Formula) : (Theory,Formula) = {
    assert(formula.id==Formula.NO_ID)
    val formula2 = formula.copy(id=counter)
//    val ref = FormulaRef(counter, formula)
    val thy = copy(counter = counter+1, formulas = formulas.updated(counter, formula2))
    (thy, formula2)
  }

  def addTrafoInstance(trafo: TrafoInstance) : (Theory,Seq[Formula]) = {
    val formulas = trafo.formulas
    var theory = this
    var newFormulas = ListBuffer() : ListBuffer[Formula]
    for (f <- formulas) {
      if (f.id==Formula.NO_ID) {
        val (thy, newFormula) = theory.addFormula(f)
        newFormulas += newFormula
        theory = thy
      } else {
        assert(isMember(f))
      }
    }
    (theory,newFormulas.toSeq)
  }

  def isMember(formula:Formula) : Boolean = {
    val own = formulas.get(formula.id)
    own.isDefined && own.get == formula
  }

  /**
    *
    * @param formula
    * @return (thy,newFormula,oldFormula): thy is the new theory, oldFormula is the formula that was replaced,
    *         newFormula is the just added formula (newFormula may or may not be equal to the parameter formula,
    *         but the logical content is guaranteed to be the same)
    */
  def updateFormula(formula:Formula) : (Theory,Formula,Formula) = {
    val oldFormula = formulas.getOrElse(formula.id, throw new IllegalArgumentException("trying to update non-existing formula"))
    val thy2 = copy(formulas = formulas.updated(formula.id, formula))
    (thy2,formula,oldFormula)
  }
}
object Theory {
  def apply() : Theory = Theory(0,Map.empty)
}

case class Formula(val id : Int = Formula.NO_ID, val math : CMathML) {
  import Formula._
  def detach: Formula = copy(id=NO_ID)

}
object Formula {
  def apply(math : CMathML) = new Formula(id=NO_ID, math=math)
  val NO_ID = -1
}

//case class FormulaRef(val id : Int, val formula : Formula) {
//  def math = formula.math
//}
