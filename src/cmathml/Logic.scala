package cmathml

import cmathml.CMathML.{fns1, logic1, quant1}

/**
  * Created by unruh on 8/14/16.
  */
object Logic {
  def instantiateLambda(lambda: CMathML, arguments: CMathML*): CMathML = {
    lambda match {
      case fns1.lambdaE(vars,body) =>
        val freeVars = Set(arguments:_*).flatMap(_.freeVariables)
        val lambdaFree = body.freeVariables
        assert(vars.length==arguments.length)
        val varNames = vars.map { case CI(_,n) => n; case CNone(_) => sys.error("CNone") }
        assert(varNames.distinct.length == varNames.length) // all vars distinct
        assert(lambdaFree.intersect(freeVars).isEmpty) // alpha-renaming needed, not yet impl
        val subst = Map(varNames.zip(arguments) : _*)
        body.substitute(subst)
      case _ => sys.error("expecting lambda expression")
    }
  }

  final case class Variance(contravariant : Boolean = false, covariant : Boolean = false) {
    def flip = Variance(contravariant=covariant, covariant=contravariant)
  }

  /** Returns the variance of a subterm inside a term.
    * May return false for variance/covariance erroneously,
    * but will never return true erroneously
    * @param math the term
    * @param path path to the subterm
    */
  def variance(math: CMathML, path : Path) : Variance =
    math match {
      case _ if path.isEmpty => Variance(covariant=true)
      case _ if path.head<=0 => Variance()
      case Apply(_, logic1.implies, p, q) if path.head==1 => variance(p, path.tail).flip
      case Apply(_, logic1.implies, p, q) if path.head==2 => variance(q, path.tail)
      case Apply(_, logic1.and, args @ _*) => variance(args(path.head), path.tail)
      case Apply(_, logic1.or, args @ _*) => variance(args(path.head), path.tail)
      case Apply(_, logic1.not, x) => variance(x, path.tail).flip
      case Bind(_, quant1.forall, _, body) if path.head==1 => variance(body, path.tail)
      case Bind(_, quant1.exists, _, body) if path.head==1 => variance(body, path.tail)
      case _ => Variance()
    }
}
