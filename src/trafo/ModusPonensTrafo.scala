package trafo

import scalaz.Scalaz._
import cmathml.{Apply, CMathML, Logic, Path}
import cmathml.CMathML.logic1.implies
import misc.{Log, Utils}
import misc.Log
import relation.{Implication, Relation}
import sun.java2d.cmm.kcms.CMM
import theory.{Formula, Theory}
import trafo.Interaction._
import trafo.ModusPonensTrafo.Instance
import z3.Z3

import scala.xml.Elem
//import com.thoughtworks.each.Monadic._


class ModusPonensTrafo extends Transformation {
  //  override def createInteractive: Interaction[TrafoInstance] =
  //    for {
  //      pOpt <- ask("p",new FormulaQ(<span>Antecedent to be eliminated (e.g., P)</span>))
  //      pqOpt <- ask("pq",new FormulaQ(<span>Formula in which P should be eliminated (e.g., P=>Q)</span>))
  //      Some(p) <- returnval(pOpt)
  //      Some(pq) <- returnval(pqOpt)
  //      instance <- pq.math match {
  //        case Apply(_,`implies`,p2,q) =>
  //          if (Z3.default.doesImply(p.math, p2).contains(true))
  //            returnval(new Instance(p, pq, Formula(q)))
  //          else
  //            Interaction.failWith("no-p", <span>The second formula must be P=>... where P is the first one</span>)
  //        case _ => Interaction.failWith("not-pq", <span>The second formula must be of the form P=>Q</span>)
  //      }
  //      _ <- ask("res", new ShowFormulaQ(<span>This will tempbe the result:</span>, instance.q))
  //    } yield instance
  override val createInteractive = interaction {
    val bOpt = ask("b", new FormulaSubtermQ(<span>Formula in which P should be eliminated (e.g., P=>Q)</span>)).each
    val aOpt = ask("a", new FormulaQ(<span>Antecedent to be eliminated (e.g., P)</span>)).each
    val (b,path) = bOpt.getOrElseI(fail).each
    val a = aOpt.getOrElseI(fail).each

    val (implPath,implChild) =
      if (path.isEmpty) (Path.empty,1)
      else path.splitLast

    if (implChild!=1)
      failWith[Unit]("not-pq1", <span>You have to select a term P inside a subterm P=>Q</span>).each

    Log.debug("path",path)
    Log.debug("implPath",implPath)

    val implSubterm = b.math.subterm(implPath)

    Log.debug("implSubterm",implSubterm)

    val (p:CMathML,q:CMathML) = implSubterm match {
      case Apply(_, `implies`, p2, q2) => (p2,q2)
      case _ => failWith[Null]("not-pq", <span>You have to select a term P inside a subterm P=>Q</span>).each
    }

    if (!Z3.default.doesImply(a.math, p).contains(true))
      failWithU("no-p", <span>The first formula must equal (or imply) the selected subterm</span>).each

    if (!Logic.variance(b.math,implPath).covariant)
      failWithU("not-covariant", <span>The selected implication must be in a covariant position</span>).each

    val res = b.math.replace(implPath,q)

    val instance = Instance(a, b, implPath, Formula(res))

    ask("res", new ShowFormulaQ(<span>This will be the result:</span>, instance.res, highlight=Some(implPath))).each

    assert(instance.isValid,(a,b,implPath,res))

    instance
  }
}

object ModusPonensTrafo {
  def fromXML(xml:Elem) = {
    val id = xml.attribute("id").get.text.toInt
    val path = Path.fromString(xml.attribute("path").get.text)
    Utils.elementsIn(xml) match {
      case Seq(a,b,res) => Instance(Formula.fromXML(a),Formula.fromXML(b),path,Formula.fromXML(res),id)
    }
  }

  case class Instance(a : Formula, b : Formula, path : Path, res : Formula, id : Int = Theory.NO_ID) extends TrafoInstance {
    override val shortDescription: String = "modus ponens"
    override val formulas = Vector(a,b,res)
    override val isValid: Boolean = {
      val impl = b.math.subterm(path)
      impl match {
        case Apply(_,`implies`,p,q) =>
          Logic.variance(b.math,path).covariant &&
          Z3.default.doesImply(a.math, p).contains(true) &&
            Z3.default.isEqual(b.math.replace(path,q), res.math).contains(true)
        case _ => false
      }
    }

    override def toXML: Elem = <modusPonens id={id.toString} path={path.toString}>
      {a.toXML}
      {b.toXML}
      {res.toXML}
    </modusPonens>

    override def update(id: Int, formulas: Seq[Formula]): TrafoInstance = formulas match {
      case Seq(a2,b2,res2) => Instance(a2,b2,path,res2,id)
      case _ => sys.error("update with wrong number of formulas")
    }
    override val relation: Relation = Implication(2,1)
  }
}

