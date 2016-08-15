package trafo

import cmathml.Path
import relation.{Relation, Unrelated}
import theory.Formula
import trafo.Interaction._
import trafo.SubtermTrafo.Instance

class SubtermTrafo extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { Some((form,path)) <- ask("a", new FormulaSubtermQ(<span>A subterm of a formula</span>))
          inst = new Instance(path,form,Formula(form.math.subterm(path)))
          _ <- ask("res", new ShowFormulaQ(<span>This will be the result</span>, inst.outFormula))
    } yield inst
}

object SubtermTrafo {
  class Instance(path : Path, a: Formula, b: Formula) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = a.math.subterm(path) == b.math
    val outFormula = b
    override val relation: Relation = Unrelated(2)
  }
}
