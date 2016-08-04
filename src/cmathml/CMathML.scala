package cmathml

import java.math.MathContext

import cmathml.CMathML._
import com.sun.org.apache.xerces.internal.util.XMLChar
import misc.Pure

import scala.math.BigDecimal.RoundingMode
import scala.xml.{Elem, Utility}

case class InvalidPath() extends Exception

/** Represents mathematical formulas in Strict Content MathML encoding */
sealed trait CMathML {
  def subterm(p: Path) : CMathML
  /** Replaces the subterm x at path p by f(x) */
  @Pure def mapAt(p:Path, f:CMathML=>CMathML) : CMathML
  /** Replaces the subterm x at path p by y */
  @Pure def replace(p:Path, y:CMathML) = mapAt(p,{_ => y})
  @Pure final def +(b:CMathML) = CMathML.plus(this,b)
  @Pure final def -(b:CMathML) = CMathML.minus(this,b)
  @Pure final def *(b:CMathML) = CMathML.times(this,b)
  @Pure final def /(b:CMathML) = CMathML.divide(this,b)

  /** Negates this expression. If it is a CN, then the number itself is negated.
    * Otherwise arith1.unary_minus is applied.
    */
  @Pure def negate() : CMathML = Apply(CMathML.uminus,this)
  /** Only immutable objects may be inserted into this map */
  val attributes : Attributes

  @Pure def toXMLDoc : String = {
    val xml = <math xmlns="http://www.w3.org/1998/Math/MathML">{toXML}</math>
    val sb = new StringBuilder
    sb ++= """<?xml version="1.0" encoding="UTF-8"?>"""
    Utility.serialize(xml,sb=sb).toString
  }

  @Pure def toXML =
    if (attributes.isEmpty)
      toXML$
    else
      ???

  /** Same as [[toXML]] but without the outermost attributes */
  @Pure def toXML$ : scala.xml.Elem

  /** Terms with priority `priority` or lower need to be parenthesised */
  @Pure def toPopcorn(sb:StringBuilder, priority:Int) : Unit =
  if (attributes.isEmpty)
    toPopcorn$(sb,priority)
  else
    ???

  /** Same as [[toPopcorn]] but without the outermost attributes */
  @Pure protected def toPopcorn$(sb:StringBuilder, priority:Int) : Unit

  @Pure def toPopcorn : String = {
    val sb = new StringBuilder
    toPopcorn(sb,0)
    sb.toString
  }
}

object CMathML {

  /** Is it an NCName in the sense of [[https://www.w3.org/TR/xmlschema-2/#NCName]]? */
  private[cmathml] def isNCName(name: String): Boolean =
    XMLChar.isValidNCName(name) // This is not public api, but should be easy to reimplement if needed

  type Attributes = Map[(String,String),Any]
  val NoAttr : Attributes = Map.empty
  val equal = CSymbol("relation1","eq")
  def equal(a: CMathML, b: CMathML) : Apply = Apply(equal,a,b)

  val plus = CSymbol("arith1","plus")
  def plus(x:CMathML,y:CMathML) : Apply = Apply(plus,x,y)
  val minus = CSymbol("arith1","minus")
  def minus(x:CMathML,y:CMathML) : Apply = Apply(minus,x,y)
  val times = CSymbol("arith1","times")
  def times(x:CMathML,y:CMathML) : Apply = Apply(times,x,y)
  val divide = CSymbol("arith1","divide")
  def divide(x:CMathML,y:CMathML) : Apply = Apply(divide,x,y)
  val power = CSymbol("arith1","power")
  def power(x:CMathML, y:CMathML) : Apply = Apply(power,x,y)

  val uminus = CSymbol("arith1","unary_minus")
  def uminus(x:CMathML) : Apply = Apply(uminus,x)
}

sealed protected trait Leaf extends CMathML {
  def mapAt(p: Path, f: CMathML=>CMathML): CMathML = { if (!p.isEmpty) throw InvalidPath(); f(this) }
  def subterm(p: Path): CMathML = { if (!p.isEmpty) throw InvalidPath(); this }
}


/** <apply>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.apply]] */
final case class Apply(val attributes : Attributes, hd: CMathML, args: CMathML*) extends CMathML {
  override def toString = toPopcorn
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

//  override def toString : String = {
//    "Apply("+hd+","+args.mkString(",")+")"
//  }

  private def popcornBinop(sb: StringBuilder,priority:Int,opPriority:Int,op:String,x:CMathML,y:CMathML): Unit = {
    if (opPriority <= priority) sb += '('
    x.toPopcorn(sb,opPriority)
    sb ++= op
    y.toPopcorn(sb,opPriority)
    if (opPriority <= priority) sb += ')'
  }

  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = this match {
    case Apply(_,`equal`,x,y) => popcornBinop(sb,priority,10,"=",x,y)
    case Apply(_,`plus`,x,y) => popcornBinop(sb,priority,15,"+",x,y)
    case Apply(_,`minus`,x,y) => popcornBinop(sb,priority,16,"-",x,y)
    case Apply(_,`times`,x,y) => popcornBinop(sb,priority,17,"*",x,y)
    case Apply(_,`divide`,x,y) => popcornBinop(sb,priority,18,"/",x,y)
    case _ =>
      hd.toPopcorn(sb,1000)
      sb += '('
      var first = true
      for (a <- args) {
        if (!first) sb += ','
        a.toPopcorn(sb,0)
        first = false
      }
      sb += ')'
  }

  override def toXML$: Elem = <apply>{hd.toXML}{args.map(_.toXML)}</apply>
}
object Apply {
  def apply(hd: CMathML, args: CMathML*) = new Apply(NoAttr,hd,args:_*)
}

/** <ci>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.ci]] */
final case class CI(val attributes : Attributes = NoAttr, name : String) extends CMathML with Leaf {
  def this(name:String) = this(NoAttr,name)
  override def toString = toPopcorn

  /** Terms with priority `priority` or lower need to be parenthesised */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = {
    sb += '$'; sb ++= name }

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem = <ci>{name}</ci>
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
  @Pure final def isNegative = (n < 0)
  override def negate() = CN(-n)
  override def toString = toPopcorn

  /** Same as [[toPopcorn]] but without the outermost attributes */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit =
    sb ++= n.toString

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem =
    if (n.isWhole) <cn type="integer">{n.toString}</cn>
    else  <cn type="real">{n.toString}</cn>
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
final case class CSymbol(val attributes : Attributes = NoAttr, cd: String, name: String) extends CMathML with Leaf {
  import CMathML._
  assert(isNCName(cd))
  assert(isNCName(name))
  /** Same as [[toPopcorn]] but without the outermost attributes */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = {
    sb ++= cd; sb += '.'; sb ++= name }
  override def toString = toPopcorn

  override def toXML$: Elem = <csymbol cd={cd}>{name}</csymbol>
}
object CSymbol {
  def apply(cd: String, name: String) = new CSymbol(NoAttr,cd,name)
}

/** <cerror>-Content MathML element
  * We are more flexible than the standard here, we allow arbitrary elements as error arguments (not just MathML)
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cerror]] */
final case class CError(val attributes : Attributes, cd: String, name: String, args: Any*) extends CMathML with Leaf {
  assert(isNCName(cd))
  assert(isNCName(name))
  /** Same as [[toPopcorn]] but without the outermost attributes */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = ???
  override def toString = toPopcorn

  private def anyToXML(o : Any) = o match {
    case m : CMathML => m.toXML
    case _ => ???
  }

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem = <cerror><csymbol cd={cd}>{name}</csymbol>{args.map(anyToXML)}</cerror>
}

/** An addition to the Content MathML standard. Represents a missing node.
  * Not valid Content MathML, cannot be exported to valid XML
  */
final case class CNone(val attributes : Attributes = NoAttr) extends CMathML with Leaf {
  /** Same as [[toPopcorn]] but without the outermost attributes */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit =
  sb += '\u25a2'

  override def toString = toPopcorn

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem = <cerror><csymbol cd="moreerrors">encodingError</csymbol><cs>Cannot encode "hole" in formula</cs></cerror>
}

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
