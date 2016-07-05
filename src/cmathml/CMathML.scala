package cmathml

/** Represents mathematical formulas in Strict Content MathML encoding */
abstract class CMathML
/** <apply>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.apply]] */
case class Apply(hd: CMathML, args: CMathML*) extends CMathML
/** <ci>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.ci]] */
case class CI(v: String) extends CMathML
/** <cn>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cn]] */
case class CN(n: BigDecimal) extends CMathML
/** <csymbol>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.csymbol]] */
case class CSymbol(cd: String, name: String) extends CMathML
/** <cerror>-Content MathML element
  * We are more flexible than the standard here, we allow arbitrary elements as error arguments (not just MathML)
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cerror]] */
case class CError(cd: String, name: String, args: Any*) extends CMathML

case class Path(path : List[Int]) extends AnyVal {
  def prepend(i:Int) = new Path(i::path)
  override def toString = path.mkString("-")
  def toPathRev = new PathRev(path.reverse)
}
object Path {
  val empty = Path(List.empty)
  val emptyRev = PathRev(List.empty)
  def make(l:Int*) = Path(l.toList)
  def makeRev(l:Int*) = PathRev(l.toList.reverse)
}
case class PathRev(path : List[Int]) extends AnyVal {
  def append(i: Int) = new PathRev(i :: path)

  def toPath = new Path(path.reverse)

  override def toString = toPath.toString()
}