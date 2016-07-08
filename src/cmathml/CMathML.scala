package cmathml

import misc.Pure

case class InvalidPath() extends Exception

/** Represents mathematical formulas in Strict Content MathML encoding */
abstract class CMathML {
  def subterm(p: Path) : CMathML
  /** Replaces the subterm x at path p by f(x) */
  @Pure def mapAt(p:Path, f:CMathML=>CMathML) : CMathML
  /** Replaces the subterm x at path p by y */
  @Pure def replace(p:Path, y:CMathML) = mapAt(p,{_ => y})
}

object CMathML {
  val equal = CSymbol("relation1","eq")
  def equal(a: CMathML, b: CMathML) : CMathML = Apply(equal,a,b)

  val plus = CSymbol("arith1","plus")
  def plus(x:CMathML,y:CMathML) : CMathML = Apply(plus,x,y)
  val minus = CSymbol("arith1","minus")
  def minus(x:CMathML,y:CMathML) : CMathML = Apply(minus,x,y)
  val times = CSymbol("arith1","times")
  def times(x:CMathML,y:CMathML) : CMathML = Apply(times,x,y)
  val divide = CSymbol("arith1","divide")
  def divide(x:CMathML,y:CMathML) : CMathML = Apply(divide,x,y)
}

protected trait Leaf extends CMathML {
  def mapAt(p: Path, f: CMathML=>CMathML): CMathML = { if (!p.isEmpty) throw InvalidPath(); f(this) }
  def subterm(p: Path): CMathML = { if (!p.isEmpty) throw InvalidPath(); this }
}


/** <apply>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.apply]] */
case class Apply(hd: CMathML, args: CMathML*) extends CMathML {
  override def mapAt(p: Path, f: (CMathML) => CMathML): CMathML = {
    if (p.isEmpty) return f(this)
    val idx = p.head; val tl = p.tail
    if (idx==0) return Apply(hd.mapAt(tl,f),args:_*)
    if (idx<1) throw InvalidPath()
    if (idx>args.length) throw InvalidPath()
    return Apply(hd,args.toList.updated(idx-1, args(idx-1).mapAt(tl,f)):_*)
  }

  override def subterm(p: Path): CMathML = {
    if (p.isEmpty) return this
    val idx = p.head; val tl = p.tail
    if (idx==0) return hd.subterm(tl)
    if (idx<1) throw InvalidPath()
    if (idx>args.length) throw InvalidPath()
    return args(idx-1).subterm(tl)
  }
}

/** <ci>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.ci]] */
case class CI(v: String) extends CMathML with Leaf

/** <cn>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cn]] */
case class CN(n: BigDecimal) extends CMathML with Leaf

/** <csymbol>-Content MathML element
 *
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.csymbol]] */
case class CSymbol(cd: String, name: String) extends CMathML with Leaf

/** <cerror>-Content MathML element
  * We are more flexible than the standard here, we allow arbitrary elements as error arguments (not just MathML)
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cerror]] */
case class CError(cd: String, name: String, args: Any*) extends CMathML with Leaf

object Path {
  def fromString(str: String): Path = if (str=="") Path.empty else Path(str.split('-') map {_.toInt} toList)
  val empty = Path(List.empty)
  val emptyRev = PathRev(List.empty)
  def make(l:Int*) = Path(l.toList)
  def makeRev(l:Int*) = PathRev(l.toList.reverse)
}
case class Path(path : List[Int]) extends AnyVal {
  def head = path.head
  def tail = Path(path.tail)
  def prepend(i:Int) = new Path(i::path)
  override def toString = path.mkString("-")
  def toPath = this
  def toPathRev = new PathRev(path.reverse)
  def isEmpty = path.isEmpty
}
case class PathRev(path : List[Int]) extends AnyVal {
  def append(i: Int) = new PathRev(i :: path)
  def toPath = new Path(path.reverse)
  def toPathRev = this
  override def toString = toPath.toString()
}
