package cmathml

import java.math.{BigInteger, MathContext}

import cmathml.CMathML._
import com.sun.org.apache.xerces.internal.util.XMLChar
import misc.{Log, Pure, Utils}
import org.symcomp.openmath.{OMSymbol, _}
import _root_.z3.Z3

import scala.xml.{Elem, Utility}

class MathException(message: String, val args: Any*) extends Exception(message)
class InvalidPath(message: String, val path:Path, args: Any*) extends MathException(message, path :: args.toList : _*)
class InvalidType(message: String, args:Any*) extends MathException(message,args)

/** Represents mathematical formulas in Strict Content MathML encoding */
sealed trait CMathML {
  def subterm(p: Path) : CMathML
  /** Replaces the subterm x at path p by f(x) */
  @Pure def mapAt(p:Path, f:CMathML=>CMathML) : CMathML
  /** Replaces the subterm x at path p by y */
  @Pure def replace(p:Path, y:CMathML) = mapAt(p,{_ => y})
  @Pure final def +(b:CMathML) = arith1.plus(this,b)
  @Pure final def -(b:CMathML) = arith1.minus(this,b)
  @Pure final def *(b:CMathML) = arith1.times(this,b)
  @Pure final def /(b:CMathML) = arith1.divide(this,b)

  /** Negates this expression. If it is a CN, then the number itself is negated.
    * Otherwise arith1.unary_minus is applied.
    */
  @Pure def negate() : CMathML = arith1.uminus(this)
  /** Only immutable objects may be inserted into this map */
  val attributes : Attributes

  @Pure def toXMLDoc : String = {
    val xml = toXMLMath
    val sb = new StringBuilder
    sb ++= """<?xml version="1.0" encoding="UTF-8"?>"""
    Utility.serialize(xml,sb=sb).toString
  }

  @Pure def toXMLMath = <math xmlns="http://www.w3.org/1998/Math/MathML">{toXML}</math>

  @Pure def toXML =
    if (attributes.isEmpty)
      toXML$
    else
      ???

  /** Same as [[toXML]] but without the outermost attributes */
  @Pure def toXML$ : scala.xml.Elem

  /** Same as [[toPopcorn]], but ignores top-level attributes */
  //noinspection AccessorLikeMethodIsUnit
  @Pure protected def toPopcorn$(sb:StringBuilder, priority:Int) : Unit
  @Pure def toPopcorn : String = {
    val sb = new StringBuilder
    toPopcorn(sb,0)
    sb.toString
  }
  /** Terms with priority `priority` or lower need to be parenthesised */
  //noinspection AccessorLikeMethodIsUnit
  @Pure def toPopcorn(sb:StringBuilder, priority:Int) : Unit =
  if (attributes.isEmpty)
    toPopcorn$(sb,priority)
  else
    ???

  @deprecated("better use toPopcorn","Aug 11, 2016")
  @Pure def toPopcornSYMCOMP : String = toSymcomp.toPopcorn

  @Pure def toSymcomp : org.symcomp.openmath.OpenMathBase = {
    if (attributes.isEmpty)
      toSymcomp$
    else
      ???
  }

  @Pure protected def toSymcomp$ : org.symcomp.openmath.OpenMathBase

  /** Checks whether the formula is valid.
    *
    * - Does not contain CNone
    * - Is well-typed
    * - Contains only supported symbols
    *
    * TODO: currently this is done by simply converting to Z3. Should be checked directly
    * TODO: should return explanation why this is not valid math
    */
  @Pure def isValidMath: Boolean = {
    try {
      z3.fromCMathML(this)
      true
    }
    catch {
      case e : Exception =>
        Log.debug("while converting to Z3",e,this)
        false
    }
  }

  /** Checks whether the formula is valid (see [[isValidMath]]) and boolean.
    */
  @Pure def isValidBooleanMath: Boolean = {
    try {
      z3.fromCMathML(this).isBool
    }
    catch {
      case e : Exception =>
        Log.debug("while converting to Z3",e,this)
        false
    }
  }
}


object CMathML {
  def tryFromPopcorn(str: String) : Option[CMathML] = {
    val om = try
      OpenMathBase.parsePopcorn(str.trim)
    catch {
      case e: OpenMathException =>
        Log.stackTraceDebug("tryFromPopcorn: could not parse",e)
        return None
    }
    Some(fromSymcomp(om))
  }

  private val z3 = new Z3()

  def fromPopcorn(str: String) = {
    val om = OpenMathBase.parsePopcorn(str.trim)
    fromSymcomp(om)
  }

  def fromSymcomp(math: OpenMathBase) : CMathML = math match {
    case n : OMInteger => CN(n.getIntValue)
    case n : OMFloat => CN(n.getDec)
    case s : OMSymbol =>
      CSymbol(s.getCd,s.getName) match {
        case CSymbol(attr,internal.holeSymbol.cd,internal.holeSymbol.name) => CNone(attr)
        case m => m
      }
    case s : OMString => CS(s.getValue)
    case x : OMVariable => CI(x.getName)
    case a : OMApply =>
      Apply(fromSymcomp(a.getHead),a.getParams.map(fromSymcomp) : _*) match {
        case Apply(attr,CSymbol(_,internal.decimalFractionSymbol.cd,internal.decimalFractionSymbol.name),CS(_,str)) =>
          CN(attr,str)
        case m => m
      }
    case b : OMBind =>
      Bind(fromSymcomp(b.getSymbol), b.getBvars.map(fromSymcomp(_).asInstanceOf[CI]).toSeq, fromSymcomp(b.getParam))
  }

  type Attributes = Map[(String,String),Any]
  val NoAttr : Attributes = Map.empty

  /** Is it an NCName in the sense of [[https://www.w3.org/TR/xmlschema-2/#NCName]]? */
  private[cmathml] def isNCName(name: String): Boolean =
    XMLChar.isValidNCName(name) // This is not public api, but should be easy to reimplement if needed


  object internal {
    val cd = "internal"
    val holeSymbol = CSymbol(cd,"hole")
    val decimalFractionSymbol = CSymbol(cd,"decimal_fraction")
  }


  object relation1 {
    val cd = "relation1"
    val equal = CSymbol(cd,"eq") // eq is used by Scala
    def equal(a: CMathML, b: CMathML) : Apply = Apply(equal,a,b)
    val neq = CSymbol(cd,"neq")
    def neq(a: CMathML, b: CMathML) : Apply = Apply(neq,a,b)
    val leq = CSymbol(cd,"leq")
    def leq(a: CMathML, b: CMathML) : Apply = Apply(leq,a,b)
    val geq = CSymbol(cd,"geq")
    def geq(a: CMathML, b: CMathML) : Apply = Apply(geq,a,b)
    val lt = CSymbol(cd,"lt")
    def lt(a: CMathML, b: CMathML) : Apply = Apply(lt,a,b)
    val gt = CSymbol(cd,"gt")
    def gt(a: CMathML, b: CMathML) : Apply = Apply(gt,a,b)
  }

  object complex1 {
    val cd = "complex1"
    val complex_cartesian = CSymbol(cd,"complex_cartesian")
    def complex_cartesian(x:CMathML, y:CMathML) : Apply = Apply(complex_cartesian,x,y)
  }

  object arith1 {
    val cd = "arith1"
    val plus = CSymbol(cd,"plus")
    def plus(x:CMathML,y:CMathML) : Apply = Apply(plus,x,y)
    val abs = CSymbol(cd,"abs")
    def abs(x:CMathML) : Apply = Apply(abs,x)
    val minus = CSymbol(cd,"minus")
    def minus(x:CMathML,y:CMathML) : Apply = Apply(minus,x,y)
    val times = CSymbol(cd,"times")
    def times(x:CMathML,y:CMathML) : Apply = Apply(times,x,y)
    val divide = CSymbol(cd,"divide")
    def divide(x:CMathML,y:CMathML) : Apply = Apply(divide,x,y)
    val power = CSymbol(cd,"power")
    def power(x:CMathML, y:CMathML) : Apply = Apply(power,x,y)
    val root = CSymbol(cd,"root")
    def root(x:CMathML, y:CMathML) : Apply = Apply(root,x,y)
    val sum = CSymbol(cd,"sum")
    def sum(x:CMathML, y:CMathML) : Apply = Apply(sum,x,y)
    val product = CSymbol(cd,"product")
    def product(x:CMathML, y:CMathML) : Apply = Apply(product,x,y)
    val uminus = CSymbol(cd,"unary_minus")
    def uminus(x:CMathML) : Apply = Apply(uminus,x)
  }

  object calculus1 {
    val cd = "calculus1"
    val diff = CSymbol(cd,"diff")
    def diff(x:CMathML) : Apply = Apply(diff,x)
    val int = CSymbol(cd,"int")
    def int(x:CMathML) : Apply = Apply(int,x)
    val defint = CSymbol(cd,"defint")
    def defint(x:CMathML, y:CMathML) : Apply = Apply(defint,x,y)
  }

  object nums1 {
    val cd = "nums1"
    val pi = CSymbol(cd,"pi")
    val e = CSymbol(cd,"e")
    val i = CSymbol(cd,"i")
    val infinity = CSymbol(cd,"infinity")
  }

  object minmax1 {
    val cd = "minmax1"
    val max = CSymbol(cd,"max")
    def max(x:CMathML) : Apply = Apply(max,x)
    val min = CSymbol(cd,"min")
    def min(x:CMathML) : Apply = Apply(min,x)
  }

  object fns1 {
    val cd = "fns1"
    val lambda = CSymbol(cd,"lambda")
    def lambda(vars:Seq[CILike], body:CMathML) : Bind = Bind(lambda,vars,body)
    def lambda(variable:CILike, body:CMathML) : Bind = Bind(lambda,variable,body)
  }

  object combinat1 {
    val cd = "combinat1"
    val binomial = CSymbol(cd,"binomial")
    def binomial(n:CMathML, m:CMathML) : Apply = Apply(binomial,n,m)
  }

  object integer1 {
    val cd = "integer1"
    val factorial = CSymbol(cd,"factorial")
    def factorial(x:CMathML) : Apply = Apply(factorial,x)
  }
  
  object logic1 {
    val cd = "logic1"
    val and = CSymbol(cd,"and")
    def and(x:CMathML, y:CMathML) : Apply = Apply(and,x,y)
    val equivalent = CSymbol(cd,"equivalent")
    def equivalent(x:CMathML, y:CMathML) : Apply = Apply(equivalent,x,y)
    val falseSym = CSymbol(cd,"false")
    val trueSym = CSymbol(cd,"true")
    val implies = CSymbol(cd,"implies")
    def implies(x:CMathML, y:CMathML) : Apply = Apply(implies,x,y)
    val not = CSymbol(cd,"not")
    def not(x:CMathML) : Apply = Apply(not,x)
    val or = CSymbol(cd,"or")
    def or(x:CMathML, y:CMathML) : Apply = Apply(or,x,y)
    val xor = CSymbol(cd,"xor")
    def xor(x:CMathML, y:CMathML) : Apply = Apply(xor,x,y)
  }

  object quant1 {
    val cd = "quant1"
    val forall = CSymbol(cd,"forall")
    def forall(vars:Seq[CILike],body:CMathML) : Bind = Bind(forall,vars,body)
    val exists = CSymbol(cd,"exists")
    def exists(vars:Seq[CILike],body:CMathML) : Bind = Bind(exists,vars,body)
  }

  object transc1 {
    val cd = "transc1"
    val cos = CSymbol(cd,"cos")
    def cos(x:CMathML) : Apply = Apply(cos,x)
    val cosh = CSymbol(cd,"cosh")
    def cosh(x:CMathML) : Apply = Apply(cosh,x)
    val cot = CSymbol(cd,"cot")
    def cot(x:CMathML) : Apply = Apply(cot,x)
    val coth = CSymbol(cd,"coth")
    def coth(x:CMathML) : Apply = Apply(coth,x)
    val csc = CSymbol(cd,"csc")
    def csc(x:CMathML) : Apply = Apply(csc,x)
    val csch = CSymbol(cd,"csch")
    def csch(x:CMathML) : Apply = Apply(csch,x)
    val exp = CSymbol(cd,"exp")
    def exp(x:CMathML) : Apply = Apply(exp,x)
    val sec = CSymbol(cd,"sec")
    def sec(x:CMathML) : Apply = Apply(sec,x)
    val sech = CSymbol(cd,"sech")
    def sech(x:CMathML) : Apply = Apply(sech,x)
    val sin = CSymbol(cd,"sin")
    def sin(x:CMathML) : Apply = Apply(sin,x)
    val sinh = CSymbol(cd,"sinh")
    def sinh(x:CMathML) : Apply = Apply(sinh,x)
    val tan = CSymbol(cd,"tan")
    def tan(x:CMathML) : Apply = Apply(tan,x)
    val tanh = CSymbol(cd,"tanh")
    def tanh(x:CMathML) : Apply = Apply(tanh,x)
  }

  def fromXML(xml: Elem) : CMathML = xml.label match {
    case "math" => fromXML(xml.child.head.asInstanceOf[Elem])
    case "csymbol" => CSymbol.fromXML(xml)
    case "cn" => CN.fromXML(xml)
    case "ci" => CI.fromXML(xml)
    case "cs" => CS.fromXML(xml)
    case "apply" => Apply.fromXML(xml)
    case "bind" => Bind.fromXML(xml)
  }
}

sealed protected trait Leaf extends CMathML {
  def mapAt(p: Path, f: CMathML=>CMathML): CMathML = { if (!p.isEmpty) throw new InvalidPath("path descending below leaf",p); f(this) }
  def subterm(p: Path): CMathML = { if (!p.isEmpty) throw new InvalidPath("path descending below leaf",p); this }
}


/** <apply>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.apply]] */
final case class Apply(attributes : Attributes, hd: CMathML, args: CMathML*) extends CMathML {
  override def toString = toPopcorn
  override def mapAt(p: Path, f: (CMathML) => CMathML): CMathML = {
    if (p.isEmpty) return f(this)
    val idx = p.head; val tl = p.tail
    if (idx==0) return Apply(attributes,hd.mapAt(tl,f),args:_*)
    if (idx>args.length) throw new InvalidPath("path refers to argument beyond last in Apply",p)
    Apply(attributes,hd,args.toList.updated(idx-1, args(idx-1).mapAt(tl,f)):_*)
  }

  private def popcornBinop(sb: StringBuilder,priority:Int,opPriority:Int,op:String,x:CMathML,y:CMathML): Unit = {
    if (opPriority <= priority) sb += '('
    x.toPopcorn(sb,opPriority)
    sb ++= op
    y.toPopcorn(sb,opPriority)
    if (opPriority <= priority) sb += ')'
  }

  private def popcornPrefixOp(sb: StringBuilder,priority:Int,opPriority:Int,op:String,x:CMathML): Unit = {
    if (opPriority <= priority) sb += '('
    sb ++= op
    x.toPopcorn(sb,opPriority)
    if (opPriority <= priority) sb += ')'
  }

  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = this match {
    case Apply(_,arith1.`power`,x,y)    => popcornBinop(sb,priority,90,"^",x,y)
    case Apply(_,arith1.`times`,x,y)    => popcornBinop(sb,priority,80,"*",x,y)
    case Apply(_,arith1.`plus`,x,y)     => popcornBinop(sb,priority,70,"+",x,y)
    case Apply(_,arith1.`minus`,x,y)    => popcornBinop(sb,priority,75,"-",x,y)
    case Apply(_,arith1.`uminus`,x,y)   => popcornPrefixOp(sb,priority,65,"!",x)
    case Apply(_,relation1.`equal`,x,y) => popcornBinop(sb,priority,60,"=",x,y)
    case Apply(_,relation1.`neq`,x,y)   => popcornBinop(sb,priority,60,"!=",x,y)
    case Apply(_,relation1.`gt`,x,y)    => popcornBinop(sb,priority,60,">",x,y)
    case Apply(_,relation1.`geq`,x,y)   => popcornBinop(sb,priority,60,">=",x,y)
    case Apply(_,relation1.`lt`,x,y)    => popcornBinop(sb,priority,60,"<",x,y)
    case Apply(_,relation1.`leq`,x,y)   => popcornBinop(sb,priority,60,"<=",x,y)
    case Apply(_,arith1.`divide`,x,y)   => popcornBinop(sb,priority,85,"/",x,y)
    case Apply(_,complex1.complex_cartesian,x,y)   => popcornBinop(sb,priority,100,"|",x,y)
    case Apply(_,logic1.implies,x,y)    => popcornBinop(sb,priority,30,"==>",x,y)
    case Apply(_,logic1.equivalent,x,y) => popcornBinop(sb,priority,30,"<=>",x,y)
    case Apply(_,logic1.or,x,y)   => popcornBinop(sb,priority,40," or ",x,y)
    case Apply(_,logic1.and,x,y)   => popcornBinop(sb,priority,50," and ",x,y)


    /*
    // From http://java.symcomp.org/download/org.symcomp-1.5.0-src.zip
    public int prec_unary_minus() { return 65; }
    public int prec_power() { return 90; }
    public int prec_complex_cartesian() { return 100; }
    public int prec_interval() { return 65; }
    public int prec_integer_interval() { return 65; }
    public int prec_list() { return 65; }
    public int prec_implies() { return 30; }
    public int prec_equivalent() { return 30; }
    public int prec_or() { return 40; }
    public int prec_and() { return 50; }
    public int prec_true_() { return 40; }
    public int prec_false_() { return 40; }
    public int prec_rational() { return 110; }
    public int prec_block() { return 10; }
    public int prec_assign() { return 20; }
    public int Relation2.prec_approx() { return 60; } // NOT IN THE "STANDARD"!
    public int prec_set() { return 65; }
    */

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

  override def subterm(p: Path): CMathML = {
    if (p.isEmpty) return this
    val idx = p.head; val tl = p.tail
    if (idx==0) return hd.subterm(tl)
    if (idx>args.length) throw new InvalidPath("path refers to argument beyond last in Apply",p)
    args(idx-1).subterm(tl)
  }

  override def toXML$: Elem = <apply>{hd.toXML}{args.map(_.toXML)}</apply>

  @Pure
  override protected def toSymcomp$: OpenMathBase = hd.toSymcomp.apply(Array(args.map(_.toSymcomp) : _*))
}
object Apply {
  def fromXML(xml: Elem) : Apply = {
    val elems = Utils.elementsIn(xml)
    val hd = CMathML.fromXML(elems.head)
    val args = elems.tail.map(CMathML.fromXML)
    Apply(hd,args :_*)
  }

  def apply(hd: CMathML, args: CMathML*) = new Apply(NoAttr,hd,args:_*)
}


/** <apply>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.bind]] */
final case class Bind(attributes : Attributes, hd: CMathML, vars: Seq[CILike], body: CMathML) extends CMathML {
  override def toString = toPopcorn
  override def mapAt(p: Path, f: (CMathML) => CMathML): CMathML = {
    if (p.isEmpty) return f(this)
    val idx = p.head; val tl = p.tail
    idx match {
      case 0 => Bind(attributes,hd.mapAt(tl,f),vars,body)
      case 1 => Bind(attributes,hd,vars,body.mapAt(tl,f))
      case i if i>=2 =>
        if (idx-2>=vars.length) throw new InvalidPath("Beyond last variable in Bind",p,this)
        vars(idx-2).mapAt(tl,f) match {
          case s : CI =>
            Bind(attributes,hd,vars.toList.updated(idx-2, s),body)
          case m => throw new InvalidType(s"trying to substitute variable in binder by ${m.getClass}",this)
        }
    }
  }

  override def subterm(p: Path): CMathML = {
    if (p.isEmpty) return this
    val idx = p.head; val tl = p.tail
    idx match {
      case 0 => hd.subterm(tl)
      case 1 => body.subterm(tl)
      case _ if idx >= 2 =>
        if (idx-2 >= vars.length) throw new InvalidPath("Beyond last variable in Bind",p,this)
        vars(idx-2).subterm(tl)
    }
  }

  override def toXML$: Elem = <bind>{hd.toXML}{vars.map(v => <bvar>{v.toXML}</bvar>)}{body.toXML}</bind>

  @Pure
  override protected def toSymcomp$: OpenMathBase =
    hd.toSymcomp.bind(Array(vars.map(_.toSymcomp.asInstanceOf[OMVariable]):_*), body.toSymcomp)

  @Pure override protected
  def toPopcorn$(sb: StringBuilder, priority: Int): Unit = {
    hd.toPopcorn(sb, 1000)
    sb += '['
    var first = true
    for (a <- vars) {
      if (!first) sb += ','
      a.toPopcorn(sb, 0)
      first = false
    }
    sb ++= "->"
    body.toPopcorn(sb, 0)
    sb += ']'
  }
}
object Bind {
  def fromXML(xml: Elem) : Bind = {
    val elems = Utils.elementsIn(xml)
    val hd = CMathML.fromXML(elems.head)
    val body = CMathML.fromXML(elems.last)
    val vars = elems.tail.dropRight(1).map { bvar =>
      assert(bvar.label=="bvar")
      CMathML.fromXML(Utils.firstElementIn(bvar)).asInstanceOf[CI]
    }
    Bind(hd,vars,body)
  }

  def apply(hd: CMathML, vars: Seq[CILike], body:CMathML) = new Bind(NoAttr,hd,vars,body)
  def apply(hd: CMathML, variable: CILike, body:CMathML) = new Bind(NoAttr,hd,Seq(variable),body)
}

/** A CI or CNone */
sealed trait CILike extends CMathML

/** <ci>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.ci]] */
final case class CI(attributes : Attributes = NoAttr, name : String) extends CMathML with Leaf with CILike {
  def this(name:String) = this(NoAttr,name)
  override def toString = toPopcorn

  /** Terms with priority `priority` or lower need to be parenthesised */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = {
    sb += '$'; sb ++= name }

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem = <ci>{name}</ci>

  @Pure override protected
  def toSymcomp$: OpenMathBase = new OMVariable(name)
}
object CI {
  def fromXML(xml: Elem) = CI(xml.text)

  def apply(name:String) = new CI(NoAttr,name)
}

/** <cn>-Content MathML element
  * The BigDecimal *must* have rounding mode [[java.math.RoundingMode.UNNECESSARY]] and precision 0.
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cn]] */
final case class CN(attributes : Attributes = NoAttr, n: BigDecimal) extends CMathML with Leaf {
  assert(n.mc.getPrecision==0)
  assert(n.mc.getRoundingMode==java.math.RoundingMode.UNNECESSARY)
  @Pure def isNegative = n < 0
  override def negate() = CN(-n)
  override def toString = toPopcorn

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem =
    if (n.isWhole) <cn type="integer">{n.toString}</cn>
    else  <cn type="real">{n.toString}</cn>

  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = {
    if (n.isExactDouble) sb ++= n.toString
    else Apply(internal.decimalFractionSymbol,CS(n.toString)).toPopcorn(sb,priority)
  }

  @Pure override protected
  def toSymcomp$: OpenMathBase =
    if (n.isWhole)
      new OMInteger(n.toBigIntExact.get.bigInteger)
    else
      new OMApply(internal.decimalFractionSymbol.cd,internal.decimalFractionSymbol.name,
        new OMString(n.toString))
//        new OMInteger(n.bigDecimal.unscaledValue),new OMInteger(n.scale))
}
object CN {
  def fromXML(xml: Elem) = CN(xml.text)

  def apply(d:BigDecimal) = new CN(NoAttr,d)
  def apply(i:BigInteger) = new CN(NoAttr,BigDecimal(i,MATHCONTEXT))
  def apply(i:Int) = new CN(NoAttr,BigDecimal(i,MATHCONTEXT))
  def apply(d:Double) = new CN(NoAttr,BigDecimal.exact(d)(MATHCONTEXT))
  def apply(s:String) = new CN(NoAttr,BigDecimal(s,MATHCONTEXT))
  def apply(attr:Attributes, s:String) = new CN(attr,BigDecimal(s,MATHCONTEXT))

  /** Use this math context to construct [[scala.BigDecimal]]s for [[CN]] */
  val MATHCONTEXT = new MathContext(0,java.math.RoundingMode.UNNECESSARY)
}

/** <cs>-Content MathML element
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cs]] */
final case class CS(attributes : Attributes = NoAttr, str: String) extends CMathML with Leaf {
  override def toString = toPopcorn

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem = <cs>{str}</cs>

  /** Same as [[toPopcorn]] but without the outermost attributes */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = {
    sb += '"'
    sb ++= str.replace("\\","\\\\").replace("\"","\\\"").replace("\n","\\n").replace("\t","\\t").replace("\r","\\r")
    sb += '"'
  }

  @Pure override protected
  def toSymcomp$: OpenMathBase = ???
}
object CS {
  def fromXML(xml: Elem) = CS(xml.text)

  def apply(str:String) = new CS(NoAttr,str)
}



/** <csymbol>-Content MathML element
 *
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.csymbol]] */
final case class CSymbol(attributes : Attributes = NoAttr, cd: String, name: String) extends CMathML with Leaf {
  import CMathML._
  assert(isNCName(cd))
  assert(isNCName(name))
  override def toString = toPopcorn

  override def toXML$: Elem = <csymbol cd={cd}>{name}</csymbol>

  /** TODO: Support abbreviated symbols */
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = {
    sb ++= cd; sb += '.'; sb ++= name }

  @Pure override protected
  def toSymcomp$: OpenMathBase = new OMSymbol(cd,name)
}
object CSymbol {
  def fromXML(xml: Elem) = CSymbol(xml.attribute("cd").get.text, xml.text)

  private def makePopcornAbbrevs(mappings: (CSymbol,String)*) =
    Map(mappings.map { case (sym:CSymbol,abbr:String) => ((sym.cd,sym.name),abbr) } : _*)
  lazy val popcornAbbrevs = makePopcornAbbrevs(
    transc1.cos -> "cos",
    transc1.cosh -> "cosh",
    transc1.cot -> "cot",
    transc1.coth -> "coth",
    transc1.csc -> "csc",
    transc1.csch -> "csch",
    transc1.exp -> "exp",
    transc1.sec -> "sec",
    transc1.sech -> "sech",
    transc1.sin -> "sin",
    transc1.sinh -> "sinh",
    transc1.tan -> "tan",
    transc1.tanh -> "tanh",
    arith1.abs -> "abs",
    arith1.root -> "root",
    arith1.sum -> "sum",
    arith1.product -> "product",
    calculus1.diff -> "diff",
    calculus1.int -> "int",
    calculus1.defint -> "defint",
    nums1.pi -> "pi",
    nums1.e -> "e",
    nums1.i -> "i",
    nums1.infinity -> "infinity",
    minmax1.min -> "min",
    minmax1.max -> "max",
    fns1.lambda -> "lambda",
    logic1.trueSym -> "true",
    logic1.falseSym -> "false",
  combinat1.binomial -> "binomial",
  integer1.factorial -> "factorial"
  )

  def apply(cd: String, name: String) = new CSymbol(NoAttr,cd,name)
}

/** <cerror>-Content MathML element
  * We are more flexible than the standard here, we allow arbitrary elements as error arguments (not just MathML)
  * @see [[https://www.w3.org/TR/MathML3/chapter4.html#contm.cerror]] */
final case class CError(attributes : Attributes, cd: String, name: String, args: Any*) extends CMathML with Leaf {
  assert(isNCName(cd))
  assert(isNCName(name))
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit = ???
  override def toString = toPopcorn

  private def anyToXML(o : Any) = o match {
    case m : CMathML => m.toXML
    case _ => ???
  }

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem = <cerror><csymbol cd={cd}>{name}</csymbol>{args.map(anyToXML)}</cerror>

  @Pure override protected
  def toSymcomp$: OpenMathBase = ??? // new OMSymbol(cd,name).error(args.map(_.toSymcomp).toArray)
}

/** An addition to the Content MathML standard. Represents a missing node.
  * Not valid Content MathML, cannot be exported to valid XML
  */
final case class CNone(attributes : Attributes = NoAttr) extends CMathML with Leaf with CILike {
  override protected def toPopcorn$(sb: StringBuilder, priority: Int): Unit =
//    sb += '\u25a2'
    CMathML.internal.holeSymbol.toPopcorn(sb,priority)

  override def toString = toPopcorn

  /** Same as [[toXML]] but without the outermost attributes */
  override def toXML$: Elem = <cerror><csymbol cd="moreerrors">encodingError</csymbol><cs>Cannot encode "hole" in formula</cs></cerror>

  @Pure override protected
  def toSymcomp$: OpenMathBase = CMathML.internal.holeSymbol.toSymcomp
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
  /** O(n) */
  def splitLast : (Path,Int) = {
    val (rest,last) = toPathRev.splitLast
    (rest.toPath,last)
  }
}
final case class PathRev(path : List[Int]) extends AnyVal {
  def splitLast : (PathRev,Int) = (new PathRev(path.tail),path.head)
  def append(i: Int) = new PathRev(i :: path)
  def toPath = new Path(path.reverse)
  def toPathRev = this
  override def toString = toPath.toString()
}
