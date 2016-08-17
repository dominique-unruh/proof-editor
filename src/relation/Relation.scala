package relation

import cmathml.CMathML.{fns1, logic1, relation1}
import cmathml.{CI, CMathML}

abstract class Relation {
  val formulaNum : Int

  /** A lambda expression of the form "lambda[$x1,...,$xn -> rule]"
    * with no free variables. n=[[formulaNum]]. rule is boolean.
    * When instantiated with n formulas, it give the logical relation specified by this Relation object.
    */
  def relatingFormula : CMathML
}

case class Implication(premiseNum : Int, conclusionNum : Int = 1) extends Relation {
  assert(conclusionNum>=1)
  val formulaNum = premiseNum + conclusionNum
  lazy val relatingFormula = {
    val premVars = (1 to premiseNum).map(i => CI(s"P$i"))
    val conclVars = (1 to conclusionNum).map(i => CI(s"C$i"))
    val vars = premVars ++ conclVars
    val concl = if (conclusionNum==1) conclVars.head else logic1.and(conclVars:_*)
    val rule = premVars.foldRight(concl)(logic1.implies(_,_))
    fns1.lambda(vars,rule)
  }
}


case object Equality extends Relation {
  val formulaNum = 2
  lazy val relatingFormula = fns1.lambda(Seq(CI("A"),CI("B")),relation1.equal(CI("A"),CI("B")))
}


case class Unrelated(formulaNum : Int) extends Relation {
  lazy val relatingFormula: CMathML =
    fns1.lambda((1 to formulaNum).map(i => CI(s"X$i")),logic1.trueSym)
}