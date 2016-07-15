package theory

import cmathml.CMathML

case class Theory(val counter : Int, val formulas : Map[Int,Formula]) {
  def addFormula(formula:Formula) : (Theory,Formula) = {
    val formula2 = formula.copy(id=counter)
//    val ref = FormulaRef(counter, formula)
    val thy = copy(counter = counter+1, formulas = formulas.updated(counter, formula2))
    (thy, formula2)
  }
}
object Theory {
  def apply() : Theory = Theory(0,Map.empty)
}

case class Formula(val id : Int = Formula.NO_ID, val math : CMathML) {

}
object Formula {
  def apply(math : CMathML) = new Formula(id=NO_ID, math=math)
  val NO_ID = -1
}

//case class FormulaRef(val id : Int, val formula : Formula) {
//  def math = formula.math
//}
