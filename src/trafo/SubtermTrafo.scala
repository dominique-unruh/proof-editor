package trafo

import cmathml.Path
import theory.Theory.NO_ID
import theory.{Formula, Theory}
import trafo.Interaction._
import trafo.SubtermTrafo.Instance

import scala.xml.Elem

class SubtermTrafo extends Transformation {
  override def createInteractive: Interaction[TrafoInstance] =
    for { Some((form,path)) <- ask("a", new FormulaSubtermQ(<span>A subterm of a formula</span>))
          inst = Instance(path,form,Formula(form.math.subterm(path)))
          _ <- ask("res", new ShowFormulaQ(<span>This will be the result</span>, inst.outFormula))
    } yield inst
}


object SubtermTrafo {
  case class Instance(path : Path, a: Formula, b: Formula, id:Int=NO_ID) extends TrafoInstance {
    override val formulas = Vector(a, b)
    override lazy val isValid = a.math.subterm(path) == b.math
    def outFormula = b
    override def toXML: Elem = ???
    override def update(id: Int, formulas: Seq[Formula]): TrafoInstance = formulas match {
      case Seq(a2,b2) => Instance(path,a2,b2,id)
      case _ => sys.error("update with wrong number of formulas")
    }
  }
}