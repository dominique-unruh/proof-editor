package cmathml

import java.math.MathContext

import cmathml.CMathML.{Attributes, NoAttr}

import scala.math.BigDecimal.RoundingMode

case class InvalidPath() extends Exception

/** Represents mathematical formulas in Strict Content MathML encoding */
sealed trait CMathML {
  def subterm(p: Path) : CMathML
  /** Replaces the subterm x at path p by f(x) */
  def mapAt(p:Path, f:CMathML=>CMathML) : CMathML
  /** Replaces the subterm x at path p by y */
  def replace(p:Path, y:CMathML) = mapAt(p,{_ => y})
  final def +(b:CMathML) = CMathML.plus(this,b)
  final def -(b:CMathML) = CMathML.minus(this,b)
  final def *(b:CMathML) = CMathML.times(this,b)
  final def /(b:CMathML) = CMathML.divide(this,b)

  /** Negates this expression. If it is a CN, then the number itself is negated.
    * Otherwise arith1.unary_minus is applied.
    */
  def negate() : CMathML = Apply(CMathML.uminus,this)
  /** Only immutable objects may be inserted into this map */
  val attributes : Attributes
}

object CMathML {
  type Attributes = Map[(String,String),Any]
  val NoAttr : Attributes = Map.empty
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

  val uminus = CSymbol("arith1","unary_minus")
  def uminus(x:CMathML) : CMathML = Apply(uminus,x)
}

sealed protected trait Leaf extends CMathML {
  def mapAt(p: Path, f: CMathML=>CMathML): CMathML = { if (!p.isEmpty) throw InvalidPath(); f(this) }
  def subterm(p: Path): CMathML = { if (!p.isEmpty) throw InvalidPath(); this }
}


/** <apply>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.apply]] */
final case class Apply(val attributes : Attributes, hd: CMathML, args: CMathML*) extends CMathML {
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

  override def toString : String = {
    "Apply("+hd+","+args.mkString(",")+")"
  }
}
object Apply {
  def apply(hd: CMathML, args: CMathML*) = new Apply(NoAttr,hd,args:_*)
}

/** <ci>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.ci]] */
final case class CI(val attributes : Attributes = NoAttr, name : String) extends CMathML with Leaf {
  def this(name:String) = this(NoAttr,name)
}
object CI {
  def apply(name:String) = new CI(NoAttr,name)
}

/** <cn>-Content MathML element
  * The BigDecimal *must* have rounding mode [[java.math.RoundingMode.UNNECESSARY]] and precision 0.
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cn]] */
final case class CN(val attributes : Attributes = NoAttr, n: BigDecimal) extends CMathML with Leaf {
  assert(n.mc.getPrecision==0)
  assert(n.mc.getRoundingMode==java.math.RoundingMode.UNNECESSARY)
  final def isNegative = (n < 0)
  override def negate() = CN(-n)
}
object CN {
  def apply(d:BigDecimal) = new CN(NoAttr,d)
  def apply(i:Int) = new CN(NoAttr,BigDecimal(i,MATHCONTEXT))
  def apply(i:Double) = new CN(NoAttr,BigDecimal.exact(i)(MATHCONTEXT))
//  def apply(i:BigDecimal) = { if (i.mc!=MATHCONTEXT) new CN(new BigDecimal(i.bigDecimal,MATHCONTEXT)) else i }
  def apply(i:String) = new CN(NoAttr,BigDecimal(i,MATHCONTEXT))

  /** Use this math context to construct [[BigDecimal]]s for [[CN]] */
  val MATHCONTEXT = new MathContext(0,java.math.RoundingMode.UNNECESSARY)
}

/** <csymbol>-Content MathML element
 *
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.csymbol]] */
final case class CSymbol(val attributes : Attributes = NoAttr, cd: String, name: String) extends CMathML with Leaf
object CSymbol {
  def apply(cd: String, name: String) = new CSymbol(NoAttr,cd,name)
}

/** <cerror>-Content MathML element
  * We are more flexible than the standard here, we allow arbitrary elements as error arguments (not just MathML)
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cerror]] */
final case class CError(val attributes : Attributes, cd: String, name: String, args: Any*) extends CMathML with Leaf

/** An addition to the Content MathML standard. Represents a missing node.
  * Not valid Content MathML, cannot be exported to valid XML
  */
final case class CNone(val attributes : Attributes = NoAttr) extends CMathML with Leaf

object Path {
  def fromString(str: String): Path = if (str=="") Path.empty else Path(str.split('-').map{_.toInt}.toList)
  val empty = Path(List.empty)
  def apply(l:Int*) : Path = Path(l.toList)
}
object PathRev {
  def fromString(str:String) = Path.fromString(str).toPathRev
  val empty = PathRev(List.empty)
  def apply(l:Int*) : PathRev = PathRev(l.toList.reverse)
}
final case class Path(path : List[Int]) extends AnyVal {
  def head = path.head
  def tail = Path(path.tail)
  def prepend(i:Int) = new Path(i::path)
  override def toString = path.mkString("-")
  def toPath = this
  def toPathRev = new PathRev(path.reverse)
  def isEmpty = path.isEmpty
}
final case class PathRev(path : List[Int]) extends AnyVal {
  def append(i: Int) = new PathRev(i :: path)
  def toPath = new Path(path.reverse)
  def toPathRev = this
  override def toString = toPath.toString()
}
