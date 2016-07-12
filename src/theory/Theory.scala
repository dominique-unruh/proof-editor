package theory

import cmathml.CMathML

case class Theory(val counter : Int, val formulas : Map[Int,Formula]) {
  def addFormula(formula:Formula) : (Theory,FormulaRef) = {
    val ref = FormulaRef(counter, formula)
    val thy = copy(counter = counter + 1, formulas = formulas.updated(counter, formula))
    (thy, ref)
  }
}
object Theory {
  def apply() : Theory = Theory(0,Map.empty)
}

case class Formula(val math : CMathML) {

}

case class FormulaRef(val id : Int, val formula : Formula) {
  def math = formula.math
}
