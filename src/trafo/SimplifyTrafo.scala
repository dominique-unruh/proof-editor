package trafo

import Interaction._
import SimplifyTrafo._
import theory.Theory.NO_ID
import theory.{Formula, Theory}
import relation.{Equality, Implication, Relation}
import theory.Formula
import z3.Z3

import scala.runtime.BoxedUnit
import scala.xml.Elem


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
              returnval(Instance(a.get, b.get))
    } yield res
}


object SimplifyTrafo {

  case class Instance(a: Formula, b: Formula, id : Int = NO_ID) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = a.math == b.math
    override def toXML: Elem = ???
    override def update(id: Int, formulas: Seq[Formula]): TrafoInstance = formulas match {
      case Seq(a2,b2) => Instance(a2,b2,id)
      case _ => sys.error("update with wrong number of formulas")
    }
    override val relation: Relation = Equality
  }

}