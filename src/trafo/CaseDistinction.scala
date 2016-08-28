package trafo

import cmathml.CMathML.logic1
import cmathml.{CMathML, CNone}
import misc.{Log, Utils}
import relation.{Implication, OneOf, Relation}
import theory.Theory.TrafoId
import theory.{Formula, Theory}
import trafo.CaseDistinction.Instance
import trafo.Interaction._
import z3.Z3

import scala.xml.Elem
import scalaz.Scalaz._

class CaseDistinction extends Transformation {
  def askUntil[T<:AnyRef](id:String, until: T => Boolean, question: Int => Question[T], startWith : Int = 0) : Interaction[List[T]] = interaction {
    val answer = ask[T](id+startWith, question(startWith)).each
    val answers =
      if (until(answer)) Nil
      else answer :: askUntil(id, until, question, startWith+1).each
    answers
  }
  override def createInteractive: Interaction[TrafoInstance] = interaction {
    val form1 = ask("form1", new MathQ(<span>First case</span>)).each
    if (form1.isInstanceOf[CNone]) fail[Unit].each
    val further = askUntil("form-", (_:CMathML).isInstanceOf[CNone],
      i => new MathQ(<span>{i}. case (or leave empty)</span>), startWith=2).each
    val cases = form1::further

    Log.debug("cases",cases)
    val complete = Z3.default.isTrue(logic1.or(cases:_*)).contains(true)
    Log.debug("complete",complete)

//    if (!complete)
//      failWith[Unit]("not complete", <span>Cases incomplete</span>).each

    val completedCases = if (complete) cases
    else {
      val extra1 = logic1.and(cases.map(logic1.not(_)) : _*)
      val extra = Z3.default.fromCMathML(extra1).simplify.toCMathML
      ask("extra", new ShowFormulaQ(<span>The follow case will be added:</span>, Formula(extra))).each
      extra :: cases
    }

    Instance(completedCases.map(Formula(_)))
  }
}

object CaseDistinction {
  def fromXML(xml:Elem) = {
    val id = TrafoId(xml.attribute("id").get.text)
    val formulas = Utils.elementsIn(xml).map(Formula.fromXML)
    Instance(formulas,id)
  }

  case class Instance(cases: Seq[Formula], id: TrafoId = Theory.NO_T_ID) extends TrafoInstance {
    override val formulas = Vector(cases: _*)
    override val shortDescription: String = "case distinction"
    override lazy val isValid: Boolean =
      Z3.default.isTrue(logic1.or(formulas.map(_.math): _*)).contains(true)
    override def toXML: Elem = <caseDistinction id={id.toString}>
      {Utils.xmlAddNewlines(formulas.map(_.toXML))}
    </caseDistinction>

    override def update(id: TrafoId, formulas: Seq[Formula]): TrafoInstance = Instance(formulas, id)
    override val relation: Relation = OneOf(cases.length)
  }
}