package z3

import java.math.BigInteger

import cmathml._
import com.microsoft.z3.{BoolSort, _}
import misc.{Pure, Utils}
//import org.apache.commons.io.IOUtils
//import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter, Opcodes}
import scala.collection.{JavaConversions, JavaConverters}
import com.microsoft.z3.enumerations.Z3_decl_kind._
import CMathML._

final class Z3(config:Map[String,String]) {
  def doesImply(a: Seq[CMathML], b: CMathML) : Option[Boolean] = synchronized {
    val a$ = a.map(fromCMathML_(_).asInstanceOf[BoolExpr])
    val b$ = fromCMathML_(b).asInstanceOf[BoolExpr]
    val solver = context.mkSolver()
    try {
      for (aa <- a$) solver.add(aa)
      solver.add(context.mkNot(b$))
      val status = solver.check
      status match {
        case Status.SATISFIABLE => Some(false)
        case Status.UNKNOWN => None
        case Status.UNSATISFIABLE => Some(true)
      }
    } finally {
      solver.dispose()
      a$.foreach(_.dispose())
      b$.dispose()
    }
  }

  def doesImply(a: CMathML, b: CMathML) : Option[Boolean] = doesImply(Seq(a),b)
  def isTrue(b: CMathML) : Option[Boolean] = doesImply(Nil,b)

  def isEqual(a: CMathML, b: CMathML) : Option[Boolean] = synchronized {
    val a$ = fromCMathML_(a)
    val b$ = fromCMathML_(b)
    val equal = try { context.mkEq(a$, b$) }
      catch { case _ : Z3Exception => return Some(false) } // Happens if the sorts of a,b do not even match
    val neq = context.mkNot(equal)
    val solver = context.mkSolver()
    try {
      solver.add(neq)
      val status = solver.check
      status match {
        case Status.SATISFIABLE => Some(false)
        case Status.UNKNOWN => None
        case Status.UNSATISFIABLE => Some(true)
      }
    } finally {
      solver.dispose()
      a$.dispose()
      b$.dispose()
    }
  }

  def this() = this(Map())
  /** Not thread safe */
  private def toCMathML(expr: Expr) : CMathML = expr match {
    case e if e.getSort==stringSort_ && e.isConst && e.getFuncDecl.getName.toString.startsWith("string@") =>
      CS(e.getFuncDecl.getName.toString.stripPrefix("string@"))
    case e: Expr if e.isConst => CI(e.getFuncDecl.getName.toString)
    case e: ArithExpr if e.isApp =>
      (e.getFuncDecl.getDeclKind,e.getNumArgs) match {
        case (Z3_OP_ADD,2) => Apply(arith1.plus, e.getArgs map toCMathML: _*)
        case (Z3_OP_MUL,2) => Apply(arith1.times, e.getArgs map toCMathML: _*)
        case (Z3_OP_SUB,2) => Apply(arith1.minus, e.getArgs map toCMathML: _*)
        case (Z3_OP_DIV,2) => Apply(arith1.divide, e.getArgs map toCMathML: _*)
        case (Z3_OP_UMINUS,1)=> Apply(arith1.uminus, e.getArgs map toCMathML: _*)
//        case (Z3_OP_EQ,2) => Apply(relation1.equal, e.getArgs map toCMathML: _*)
        case (Z3_OP_POWER,2) => Apply(arith1.power, e.getArgs map toCMathML: _*)
        case k => throw new MathException(s"cannot convert arith expr from Z3 to CMathML: $e (unkown decl kind $k)")
      }
    case e: Quantifier => // Must be before "case e:BoolExpr" because Quantifier <: BoolExpr
      val vars1 = e.getBoundVariableNames.toSeq.zip(e.getBoundVariableSorts)
      val vars2 = vars1.map { v => context.mkConst(v._1,v._2) }
      val vars3 = vars2.map(toCMathML(_).asInstanceOf[CI])
      val body1 = e.getBody.substituteVars(vars2.toArray)
      val body2 = toCMathML(body1)
      var sym : CSymbol =
        if (e.isUniversal) quant1.forall
        else if (e.isExistential) quant1.exists
        else throw new MathException("Z3 quantifier is neither universal nor existential",e)
      Bind(sym,vars3,body2)
    case e: BoolExpr if e.isApp =>
      (e.getFuncDecl.getDeclKind,e.getNumArgs) match {
        case (Z3_OP_EQ,2) => Apply(relation1.equal, e.getArgs map toCMathML: _*)
        case (Z3_OP_LE,2) => Apply(relation1.leq, e.getArgs map toCMathML: _*)
        case (Z3_OP_LT,2) => Apply(relation1.lt, e.getArgs map toCMathML: _*)
        case (Z3_OP_GE,2) => Apply(relation1.geq, e.getArgs map toCMathML: _*)
        case (Z3_OP_GT,2) => Apply(relation1.gt, e.getArgs map toCMathML: _*)
        case (Z3_OP_TRUE,0) => logic1.trueSym
        case (Z3_OP_FALSE,0) => logic1.falseSym
        case (Z3_OP_AND,_) => Apply(logic1.and, e.getArgs map toCMathML: _*)
        case (Z3_OP_OR,_) => Apply(logic1.or, e.getArgs map toCMathML: _*)
        case (Z3_OP_IFF,2) => Apply(logic1.equivalent, e.getArgs map toCMathML: _*)
        case (Z3_OP_IMPLIES,2) => Apply(logic1.implies, e.getArgs map toCMathML: _*)
        case (Z3_OP_XOR,2) => Apply(logic1.xor, e.getArgs map toCMathML: _*)
        case (Z3_OP_NOT,1) => Apply(logic1.not, e.getArgs map toCMathML: _*)
        case k => throw new MathException(s"cannot convert bool expr from Z3 to CMathML: $e (unkown decl kind $k)")
      }
    case e: RatNum =>
      val num = e.getBigIntNumerator
      val denom = e.getBigIntDenominator
      val num2 = BigDecimal(num, CN.MATHCONTEXT)
      if (denom == BigInteger.ONE) CN(num2)
      else {
        val denom2 = BigDecimal(denom, CN.MATHCONTEXT)
        try CN(num2 / denom2)
        catch {
          case _: ArithmeticException => arith1.divide(CN(num2), CN(denom2))
        }
      }

    case e => throw new MathException(s"cannot convert from Z3 to CMathML (${e.getClass}): "+e)
  }

  def fromCMathML(m: CMathML) = synchronized { new Z3.Expr(this, fromCMathML_(m)) }


  /** Not thread safe */
  private def fromCMathML_(m: CMathML) : Expr = m match {
    case CN(_, i) => context.mkNumeral(i.toString, realSort_)
    case CI(_, n) =>
      val sort = if (n.charAt(0).isUpper) boolSort_ else realSort_
      context.mkConst(n, sort)
    case CS(_, s) => context.mkConst("string@"+s,stringSort_)
    case Apply(_, arith1.plus,x,y) => context.mkAdd(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                     fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, arith1.minus,x,y) => context.mkSub(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                      fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, arith1.times,x,y) => context.mkMul(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                      fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, arith1.divide,x,y) => context.mkDiv(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                       fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, relation1.equal,x,y) => context.mkEq(fromCMathML_(x),fromCMathML_(y))
    case Apply(_, relation1.neq,x,y) => context.mkNot(context.mkEq(fromCMathML_(x),fromCMathML_(y)))
    case Apply(_, relation1.lt,x,y) => context.mkLt(fromCMathML_(x).asInstanceOf[ArithExpr],
      fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, relation1.leq,x,y) => context.mkLe(fromCMathML_(x).asInstanceOf[ArithExpr],
      fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, relation1.gt,x,y) => context.mkGt(fromCMathML_(x).asInstanceOf[ArithExpr],
      fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, relation1.geq,x,y) => context.mkGe(fromCMathML_(x).asInstanceOf[ArithExpr],
      fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, arith1.uminus,x) => context.mkUnaryMinus(fromCMathML_(x).asInstanceOf[ArithExpr])
    case Apply(_, arith1.power,x,y) => context.mkPower(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                        fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, logic1.or,x,y) => context.mkOr(fromCMathML_(x).asInstanceOf[BoolExpr],
                                                  fromCMathML_(y).asInstanceOf[BoolExpr])
    case Apply(_, logic1.and,x,y) => context.mkAnd(fromCMathML_(x).asInstanceOf[BoolExpr],
                                                    fromCMathML_(y).asInstanceOf[BoolExpr])
    case Apply(_, logic1.equivalent,x,y) => context.mkIff(fromCMathML_(x).asInstanceOf[BoolExpr],
                                                           fromCMathML_(y).asInstanceOf[BoolExpr])
    case Apply(_, logic1.implies,x,y) => context.mkImplies(fromCMathML_(x).asInstanceOf[BoolExpr],
                                                            fromCMathML_(y).asInstanceOf[BoolExpr])
    case Apply(_, logic1.xor,x,y) => context.mkXor(fromCMathML_(x).asInstanceOf[BoolExpr],
                                                    fromCMathML_(y).asInstanceOf[BoolExpr])
    case Apply(_, logic1.not,x) => context.mkNot(fromCMathML_(x).asInstanceOf[BoolExpr])
    case logic1.trueSym => context.mkTrue()
    case logic1.falseSym => context.mkFalse()
    case Bind(_, quant1.forall,vs,body) =>
      context.mkForall(
        vs.map(fromCMathML_(_)).toArray,
        fromCMathML_(body),
        0, null, null,null,null)
    case Bind(_, quant1.exists,vs,body) =>
      context.mkExists(
        vs.map(fromCMathML_(_)).toArray,
        fromCMathML_(body),
        0, null, null,null,null)
  }


  Z3 // Make sure the static initializer of Z3 is invoked
  private val context = new Context(JavaConversions.mapAsJavaMap(config))
//  def mkSymbol(name: String) = synchronized { new Z3.Symbol(context.mkSymbol(name)) }
//  def mkSymbol(i: Int) = synchronized { new Z3.Symbol(context.mkSymbol(i)) }
  private lazy val boolSort_ = context.mkBoolSort()
  private lazy val intSort_ = context.mkIntSort()
  private lazy val realSort_ = context.mkRealSort()
  private lazy val stringSort_ = context.mkUninterpretedSort("unicodeString")

//  /** Declares a new function. Note: the range of the function is given <b>first</b>!
//    * (Hence the suffix RD)
//    *
//    * @param name
//    * @param range
//    * @param domain
//    */
//  def mkFuncDeclRD(name:Symbol, range:Z3.Sort, domain:Z3.Sort*): Z3.FuncDecl = {
//    assert(range.z3 eq this)
//    for (d<-domain) assert(d.z3 eq this)
//    synchronized { context.mkFuncDecl(name, domain.toArray, range) }
//  }
//  def mkFreshFuncDeclRD(name: String, range: Z3.Sort, domain: Z3.Sort*): Z3.FuncDecl = {
//    assert(range.z3 eq this)
//    for (d<-domain) assert(d.z3 eq this)
//    synchronized {
//      context.mkFreshFuncDecl(name, domain.toArray, range)
//    }
//  }
//  def mkConst(x: Symbol, sort: Z3.Sort) : Z3.Expr = {
//    assert(sort.z3 eq this)
//    synchronized {
//      context.mkConst(x, sort)
//    }
//  }
//  def mkFreshConst(name: String, sort: Z3.Sort) : Z3.Expr = {
//    assert(sort.z3 eq this)
//    synchronized {
//      context.mkFreshConst(name, sort)
//    }
//  }
//  def mkApp(f: Z3.FuncDecl, args: Z3.Expr*) = synchronized { context.mkApp(f,args:_*) }
//  def mkGoal(models: Boolean, unsatCores: Boolean, proofs: Boolean) = synchronized { new Z3.Goal(context.mkGoal(models,unsatCores,proofs)) }
//  def mkEq(a: Z3.Expr, b: Z3.Expr) = synchronized { context.mkEq(a,b) }
}

object Z3 {
  lazy val default = new Z3(Map())

//  monkeyPatchZ3Native() // TODO needed?
//  loadLib("/libz3.so")
//  loadLib("/libz3java.so")
//  print("java.library.path",System.getProperty("java.library.path"))
//  try System.loadLibrary("z3")
//  catch { case _ : UnsatisfiedLinkError => System.loadLibrary("libz3") }
  try System.loadLibrary("z3java")
  catch { case _ : UnsatisfiedLinkError => System.loadLibrary("libz3java") }

  private def loadLib(name:String*) = {
//    try {
       System.load(Utils.resourceFile(name : _*))
//    } catch {
//      case e : java.lang.UnsatisfiedLinkError if e.getMessage.endsWith(" already loaded in another classloader") => ()
//    }
  }


  def majorVersion = Version.getMajor
  def minorVersion = Version.getMinor
  def version = Version.getString
  def toggleWarningMessages(enabled: Boolean) = com.microsoft.z3.Global.ToggleWarningMessages(enabled)

  final class Expr protected[z3] (val z3:Z3, private val expr:com.microsoft.z3.Expr) {
    def isBool = expr.isBool

    def simplify = z3.synchronized { new Expr(z3,expr.simplify) }
    override def toString = expr.toString

    def toCMathML = z3.synchronized { z3.toCMathML(expr) }
  }
//  final class Symbol private (val z3:Z3, private val symbol:com.microsoft.z3.Expr)

//  final class Goal(val z3:Z3, val goal:com.microsoft.z3.Goal) {
//    @Pure
//    def simplify = z3.synchronized { new Goal(z3, goal.simplify()) }

//    def add(exprs: BoolExpr*) : Unit = goal.add(exprs:_*)
//    override def toString = goal.toString
//    def getFormula : Expr = { val fs = goal.getFormulas; assert(fs.length==1, "getFormulas.length="+fs.length); fs(0) }
//  }

  /*  /** Loads the class [[com.microsoft.z3.Native]] with the static initializer removed.
      * The static initializer is supposed to load the native library z3java, but fails.
      * We load the library ourselves in the initializer of [[Z3]],
      * so we can remove the static initializer of [[Native]] without harm.
      *
      * This function must be called before any classes from [[com.microsoft.z3]] are accessed,
      * and it must be called only once.
      */
    private def monkeyPatchZ3Native(): Unit = {
      val byteCodeStream = getClass.getResource("/com/microsoft/z3/Native.class").openStream()
      val byteCode = IOUtils.toByteArray(byteCodeStream)
      byteCodeStream.close()
      val cw = new ClassWriter(0)
      val cv = new ClassVisitor(Opcodes.ASM5, cw) {
        override def visitMethod(access:Int, name:String, desc:String, signature:String, exceptions:Array[String]) = {
          if (name=="<clinit>")
            null
          else
            super.visitMethod(access, name, desc, signature, exceptions)
        }
      }
      val cr = new ClassReader(byteCode)
      cr.accept(cv,0)
      var byteCodeMod = cw.toByteArray
      val argTypes : Array[Class[_]] = null
      val method =
        classOf[ClassLoader].getDeclaredMethod("defineClass",classOf[String],classOf[Array[Byte]],classOf[Int],classOf[Int])
      method.setAccessible(true)
      method.invoke(getClass.getClassLoader,null,byteCodeMod,0.asInstanceOf[Object],byteCodeMod.length.asInstanceOf[Object])
    } */
}
