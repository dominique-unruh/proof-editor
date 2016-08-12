package trafo

import cmathml.CMathML.implies
import cmathml.{Apply, CMathML}
import theory.Formula
import trafo.Interaction.{ask, returnval}
import trafo.ModusPonensTrafo.Instance
import z3.Z3


class ModusPonensTrafo extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for {
      pOpt <- ask("p",new FormulaQ(<span>Antecedent to be eliminated (e.g., P)</span>))
      pqOpt <- ask("pq",new FormulaQ(<span>Formula in which P should be eliminated (e.g., P=>Q)</span>))
      Some(p) <- returnval(pOpt)
      Some(pq) <- returnval(pqOpt)
      instance <- pq.math match {
        case Apply(_,`implies`,p2,q) =>
          if (Z3.default.doesImply(p.math, p2).contains(true))
            returnval(new Instance(p, pq, Formula(q)))
          else
            Interaction.failWith("no-p", <span>The second formula must be P=>... where P is the first one</span>)
        case _ => Interaction.failWith("not-pq", <span>The second formula must be of the form P=>Q</span>)
      }
      _ <- ask("res", new ShowFormulaQ(<span>This will tempbe the result:</span>, instance.q))
    } yield instance
}

object ModusPonensTrafo {
  class Instance(val p : Formula, val pq : Formula, val q : Formula) extends TrafoInstance {
    override val formulas = Vector(p,pq,q)
    override val isValid: Boolean = pq.math match {
      case Apply(_,`implies`,p2,q2) =>
        Z3.default.doesImply(p.math, p2).contains(true) && q.math==q2
      case _ => false
    }
  }
}

