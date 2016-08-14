package theory

import cmathml.{CMathML, Path}
import trafo.TrafoInstance

import scala.collection.mutable.ListBuffer
import scala.xml.{Comment, Elem}

final case class Theory(counter : Int,
                        /** Invariants:
                    * - for any (i->f) in this map, f.id==i.
                    * - for any (i->f), i<[[Theory!.counter counter]] */
                        formulas : Map[Int,Formula]) {
  def deleteFormula(formula: Formula) : (Theory,Formula) = {
    val form2 = formulas(formula.id)
    val thy2 = copy(formulas = formulas - formula.id)
    (thy2,form2)
  }

  def toXML = {
    def formulaNL(f:Formula) = Seq(f.toXML,scala.xml.Text("\n"))
    val sortedFormulas = formulas.values.toSeq.sortBy(_.id)
    <theory xmlns="http://unruh.de/proof-editor" counter={counter.toString}>
      <formulas>
        {sortedFormulas.flatMap(formulaNL)}</formulas>
    </theory>
  }

  def addFormula(formula:Formula) : (Theory,Formula) = {
    assert(formula.id==Formula.NO_ID)
    val formula2 = formula.copy(id=counter)
    val thy = copy(counter = counter+1, formulas = formulas.updated(counter, formula2))
    (thy, formula2)
  }

  def addTrafoInstance(trafo: TrafoInstance) : (Theory,Seq[Formula]) = {
    val formulas = trafo.formulas
    var theory = this
    var newFormulas = ListBuffer() : ListBuffer[Formula]
    for (f <- formulas) {
      if (f.id==Formula.NO_ID) {
        val (thy, newFormula) = theory.addFormula(f)
        newFormulas += newFormula
        theory = thy
      } else {
        assert(isMember(f))
      }
    }
    (theory,newFormulas.toList)
  }

  def isMember(formula:Formula) : Boolean = {
    val own = formulas.get(formula.id)
    own.isDefined && own.get == formula
  }

  /** Replaces a formula stored in the theory by a new formula.
    * The formula to be replaced is specified by the id of the new formula.
    *
    * @param formula th new formula
    * @return (thy,newFormula,oldFormula): thy is the new theory, oldFormula is the formula that was replaced,
    *         newFormula is the just added formula (newFormula may or may not be equal to the parameter formula,
    *         but the logical content is guaranteed to be the same)
    */
  def updateFormula(formula:Formula) : (Theory,Formula,Formula) = {
    val oldFormula = formulas.getOrElse(formula.id, throw new IllegalArgumentException("trying to update non-existing formula"))
    val thy2 = copy(formulas = formulas.updated(formula.id, formula))
    (thy2,formula,oldFormula)
  }
}
object Theory {
  def apply() : Theory = Theory(0,Map.empty)
  def fromXML(xml:Elem) : Theory = {
    assert(xml.label=="theory")
    val formulas = Map[Int,Formula]((xml \ "formulas" \ "formula").map { x => val f = Formula.fromXML(x.asInstanceOf[Elem]); f.id -> f } : _*)
    val counter = xml.attribute("counter").get.text.toInt
    new Theory(counter,formulas)
  }
}

final case class Formula private[theory] (id : Int = Formula.NO_ID, math : CMathML) {
  import Formula._
  def detach: Formula = copy(id=NO_ID)
  def toXML = <formula id={id.toString}>{Comment(" "+math.toPopcorn+" ")}{math.toXMLMath}</formula>
}

object Formula {
  def apply(math : CMathML) = new Formula(id=NO_ID, math=math)
  val NO_ID = -1
  def fromXML(xml:Elem) : Formula = {
    assert(xml.label=="formula")
    val id = xml.attribute("id").get.text.toInt
    val math = CMathML.fromXML((xml \ "math").head.asInstanceOf[Elem])
    new Formula(id,math)
  }
}

//case class FormulaRef(val id : Int, val formula : Formula) {
//  def math = formula.math
//}
