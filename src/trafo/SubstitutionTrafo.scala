package trafo

import cmathml.CMathML.relation1
import cmathml.{CI, Path}
import misc.Utils
import relation.{Equality, Relation}
import theory.Formula
import theory.Theory._
import trafo.Interaction._
import trafo.ModusPonensTrafo.Instance
import trafo.SubstitutionTrafo._
import z3.Z3

import scala.xml.Elem

class SubstitutionTrafo extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { Some(eq) <- ask("eq", new FormulaQ(<span>An equality</span>))
          (lhs,rhs) <- eq.math match {
            case relation1.equalE(l,r) => returnval((l,r))
            case _ => failWith("notEq", <span>No an equality (should be of the form A=B</span>)
          }

          Some((a,path)) <- ask("a", new FormulaSubtermQ(<span>Formula in which to substitute (select subterm to substitute)</span>))

          (subterm,bound) = a.math.subtermBound(path)
          bound2 = bound.collect { case CI(_,n) => n }

          freeVars = lhs.freeVariables ++ rhs.freeVariables
          colliding = bound2.toSet.intersect(freeVars)
          _ <- if (colliding.nonEmpty)
                 failWith("collision", <span>Clash between bound variables and variables in lhs: {colliding.mkString(", ")}</span>)
               else returnval(())

          _ <- if (!Z3.default.isEqual(lhs,subterm).contains(true))
                 failWith("not-same", <span>Selected term and term to be substituted are not the same</span>)
               else returnval(())

          b = Formula(a.math.replace(path,rhs))

          inst = Instance(eq,a,path,b)

          _ <- ask("confirm", new ShowFormulaQ(<span>This is what will be inserted</span>, b))
          _ = assert(inst.isValid,(eq,a,path,b))
    } yield inst
}

object SubstitutionTrafo {
  case class Instance(eq : Formula, a : Formula, path : Path, b : Formula, id : TrafoId = NO_T_ID) extends TrafoInstance {
    override val shortDescription: String = "substitution"
    override def update(id: TrafoId, formulas: Seq[Formula]): TrafoInstance = formulas match {
      case Seq(eq2,a2,b2) => Instance(eq2,a2,path,b2)
      case _ => sys.error("update with wrong number of formulas")
    }
    override val formulas: IndexedSeq[Formula] = Vector(eq,a,b)
    override val isValid: Boolean = {
      def f : Boolean = {
        val (lhs,rhs) = eq.math match {
          case relation1.equalE(l,r) => (l,r)
          case _ => return false
        }

        val (subterm,bound) = a.math.subtermBound(path)
        val bound2 = bound.collect { case CI(_,n) => n }
        val freeVars = lhs.freeVariables ++ rhs.freeVariables
        val colliding = bound2.toSet.intersect(freeVars)
        if (colliding.nonEmpty) return false

        if (!Z3.default.isEqual(lhs,subterm).contains(true)) return false

        val b2 = a.math.replace(path,rhs)
        if (!Z3.default.isEqual(b.math,b2).contains(true)) return false

        true
      }
      f
    }

    override val relation: Relation = Equality(premiseNum = 1)

    override def toXML: Elem = <substitution id={id.toString} path={path.toString}>
      {eq.toXML}
      {a.toXML}
      {b.toXML}
    </substitution>
  }

  def fromXML(xml:Elem) = {
    val id = TrafoId(xml.attribute("id").get.text)
    val path = Path.fromString(xml.attribute("path").get.text)
    Utils.elementsIn(xml) match {
      case Seq(eq,a,b) => Instance(Formula.fromXML(eq), Formula.fromXML(a), path, Formula.fromXML(b), id)
    }
  }
}