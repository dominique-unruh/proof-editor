package trafo

import Interaction._
import CopyTrafo._
import theory.Formula

import scala.runtime.BoxedUnit

// TODO: remove eventually

class CopyTrafo extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { a <- ask("a", new FormulaQ(<span>A formula</span>))
          b = a.map(_.detach)
          _ <- if (a.isDefined) ask("b", new ShowFormulaQ(<span>This will be the result</span>, b.get))
               else returnval(BoxedUnit.UNIT)
          res <-
            if (a.isEmpty)
              fail
//            else if (b.isEmpty)
//              failWith("b-empty", <span>Should not happen! b is empty</span>)
//            else if (a.get.math != b.get.math)
//              failWith("noteq", <span>Both formulas must be identical</span>)
//            else if (a.get.id == b.get.id)
//              {println(a.get.id,b.get.id); failWith("identical", <span>Please select two separate (but identical) formulas</span>)} // TODO remove?
//            else if (b.get.id != Formula.NO_ID)
//              failWith("notfresh", <span>Expecting a fresh formula</span>)
            else
              returnval(new Instance(a.get, b.get))
    } yield res

}


object CopyTrafo {

  class Instance(a: Formula, b: Formula) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = a.math == b.math
  }

}