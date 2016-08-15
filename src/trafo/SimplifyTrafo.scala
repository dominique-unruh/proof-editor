package trafo

import Interaction._
import SimplifyTrafo._
import relation.{Equality, Implication, Relation}
import theory.Formula
import z3.Z3

import scala.runtime.BoxedUnit


class SimplifyTrafo extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { a <- ask("a", new FormulaQ(<span>A formula</span>))
          b = a.map {a => Formula(Z3.default.synchronized {Z3.default.fromCMathML(a.math).simplify.toCMathML})}
          _ <- if (a.isDefined) ask("b", new ShowFormulaQ(<span>This will be the result</span>, b.get))
               else returnval(BoxedUnit.UNIT)
          res <-
            if (a.isEmpty)
              fail
            else
              returnval(new Instance(a.get, b.get))
    } yield res
}


object SimplifyTrafo {

  class Instance(a: Formula, b: Formula) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = a.math == b.math
    override val relation: Relation = Equality
  }

}