package trafo


import Interaction._
import theory.Formula
import CheckEqualTrafo._
import z3.Z3

// TODO: remove eventually

class CheckEqualTrafo() extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { a <- ask("a", new FormulaQ(<span>A formula</span>))
          b <- ask("b", new FormulaQ(<span>An identical formula</span>))
          res <- if (a.isDefined && !b.isEmpty) {
                    if (Z3.default.isEqual(a.get.math, b.get.math) != Some(true))
                      failWith("noteq", <span>Both formulas must be equal (logically equivalent)</span>)
                    else if (a.get.id == b.get.id)
                      failWith("identical", <span>Please select two separate (but equivalent) formulas</span>)
                    else
                      returnval(new Instance(a.get, b.get))
                 } else fail
        } yield res
}

object CheckEqualTrafo {
  class Instance(a: Formula, b: Formula) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = Z3.default.isEqual(a.math, b.math).contains(true) //a.math == b.math
  }

}