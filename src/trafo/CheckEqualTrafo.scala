package trafo


import Interaction._
import theory.Formula
import CheckEqualTrafo._

// TODO: remove eventually

class CheckEqualTrafo() extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { a <- ask("a", new FormulaQ(<span>A formula</span>))
          b <- ask("b", new FormulaQ(<span>An identical formula</span>))
          res <- if (!a.isEmpty && !b.isEmpty) {
                    if (a.get.math != b.get.math)
                      failWith("noteq", <span>Both formulas must be identical</span>)
                    else if (a.get.id == b.get.id)
                      failWith("identical", <span>Please select two separate (but identical) formulas</span>)
                    else
                      returnval(new Instance(a.get, b.get))
                 } else fail
        } yield res
}

object CheckEqualTrafo {

  class Instance(a: Formula, b: Formula) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = a.math == b.math
  }

}