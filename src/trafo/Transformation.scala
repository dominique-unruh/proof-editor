package trafo

import cmathml.CMathML
import theory.{Formula, FormulaRef}

import scala.collection.immutable.Vector

/** A [[Transformation]] models a way how one gets formulas from other formulas.
  * Each [[Transformation]]-class is associated with [[TrafoInstance]]-classes which encode particular instantiations of the transformation.
  *
  */
abstract class Transformation {
  def createInteractive : Interaction[TrafoInstance]
}

/** Immutable class that represents the relationship between various formulas.
  * The precise relationship depends on the subclass of [[TrafoInstance]], but
  * it must only refer to the formulas listed in [[formulas]].
  */
abstract class TrafoInstance {
  val formulas : IndexedSeq[FormulaRef]
  val isValid  : Boolean
}

import Interaction._

class IdentityTransformation() extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { a <- ask("a", new FormulaQ(<span>A formula</span>))
          b <- ask("b", new FormulaQ(<span>An identical formula</span>))
          res <- if (!a.isEmpty && !b.isEmpty) {
                    if (a.get != b.get)
                      failWith("noteq", <span>Both formulas must be identical</span>)
                    else
                      returnval(new IdentityTrafoInstance(a.get, b.get))
                 } else fail
        } yield res
}


class IdentityTrafoInstance(a : FormulaRef, b : FormulaRef) extends TrafoInstance {
  override val formulas = Vector(a,b)
  override lazy val isValid = a.math == b.math
}
