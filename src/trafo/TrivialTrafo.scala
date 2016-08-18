package trafo

import misc.Utils
import relation.{Relation, Trivial}
import theory.Formula
import theory.Theory._
import trafo.Interaction._
import trafo.TrivialTrafo.Instance
import z3.Z3

import scala.xml.Elem

class TrivialTrafo() extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] = interaction {
    val a = ask("a", new MathQ(<span>A trivially true formula</span>)).each
    if (!a.isValidMath)
      failWith[Unit]("not-valid", <span>The formula is not valid math</span>).each
    if (!Z3.default.isTrue(a).contains(true))
      failWith[Unit]("not-true", <span>The formula is not (obviously) true</span>).each
    val inst = new Instance(Formula(a))
    assert(inst.isValid)
    inst
  }
}

object TrivialTrafo {
  def fromXML(xml:Elem) = {
    val id = xml.attribute("id").get.text.toInt
    Utils.elementsIn(xml) match {
      case Seq(a) => new Instance(Formula.fromXML(a),id)
    }
  }

  class Instance(a: Formula, val id : Int = NO_ID) extends TrafoInstance {
    override val formulas = Vector(a)
    override lazy val isValid = Z3.default.isTrue(a.math).contains(true)
    override def toXML: Elem = <trivial id={id.toString}>{a.toXML}</trivial>

    override def update(id: Int, formulas: Seq[Formula]): TrafoInstance = formulas match {
      case Seq(a2) => new Instance(a2,id)
      case _ => sys.error("update with wrong number of formulas")
    }
    override val relation: Relation = Trivial
    override val shortDescription: String = "trivial"
  }
}