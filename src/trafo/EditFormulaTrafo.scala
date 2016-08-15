package trafo


import Interaction._
import theory.{Formula, Theory}
import EditFormulaTrafo._
import cmathml.CMathML
import theory.Theory.NO_ID
import z3.Z3

import scala.collection.mutable.ListBuffer
import scala.xml.Elem

// TODO: remove eventually

class EditFormulaTrafo() extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { a <- ask("a", new FormulaQ(<span>A formula</span>))
          b <- ask("b", new MathQ(<span>An identical formula</span>))
          res <- if (a.isDefined) {
                    if (!b.isValidMath)
                      failWith("not-valid", <span>The second formula is not valid math</span>)
                    else if (!Z3.default.isEqual(a.get.math, b).contains(true))
                      failWith("noteq", <span>Both formulas must be equal (logically equivalent)</span>)
                    else
                      returnval(new Instance(a.get, Formula(b)))
                 } else fail
          _ <- ask("confirm", new ShowFormulaQ(<span>This is what will be inserted</span>, Formula(b)))
        } yield res
}

object EditFormulaTrafo {
  class Instance(a: Formula, b: Formula, val id : Int = NO_ID) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = Z3.default.isEqual(a.math, b.math).contains(true) //a.math == b.math
    override def toXML: Elem = ???
    override def update(id: Int, formulas: Seq[Formula]): TrafoInstance = formulas match {
      case Seq(a2,b2) => new Instance(a2,b2,id)
      case _ => sys.error("update with wrong number of formulas")
    }
  }
}