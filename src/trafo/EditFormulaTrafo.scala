package trafo


import Interaction._
import theory.{Formula, Theory}
import EditFormulaTrafo._
import cmathml.CMathML
import misc.Utils
import theory.Theory.{NO_ID, NO_T_ID, TrafoId}
import relation.{Equality, Implication, Relation}
import z3.Z3

import scala.collection.mutable.ListBuffer
import scala.xml.Elem

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
  class Instance(a: Formula, b: Formula, val id : TrafoId = NO_T_ID) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = Z3.default.isEqual(a.math, b.math).contains(true) //a.math == b.math

    override def toXML: Elem = <editFormula id={id.toString}>
      {a.toXML}
      {b.toXML}
    </editFormula>

    override def update(id: TrafoId, formulas: Seq[Formula]): TrafoInstance = formulas match {
      case Seq(a2,b2) => new Instance(a2,b2,id)
      case _ => sys.error("update with wrong number of formulas")
    }
    override val relation: Relation = Equality()
    override val shortDescription: String = "automated comparison"
  }

  def fromXML(xml:Elem) = {
    val id = TrafoId(xml.attribute("id").get.text)
    Utils.elementsIn(xml) match {
      case Seq(a,b) => new Instance(Formula.fromXML(a),Formula.fromXML(b),id)
    }
  }
}