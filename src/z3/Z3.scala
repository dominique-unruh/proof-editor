package z3

import java.math.BigInteger

import cmathml.{Apply, CI, CMathML, CN}
import com.microsoft.z3.{BoolSort, _}
import misc.{Pure, Utils}
import ui.mathview.MathException
//import org.apache.commons.io.IOUtils
//import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter, Opcodes}
import scala.collection.{JavaConversions, JavaConverters}



final class Z3(config:Map[String,String]) {
  def isEqual(a: CMathML, b: CMathML) : Option[Boolean] = synchronized {
    val a$ = fromCMathML_(a)
    val b$ = fromCMathML_(b)
    val neq = context.mkNot(context.mkEq(a$,b$))
    val solver = context.mkSolver()
    solver.add(neq)
    val status = solver.check
    status match {
      case Status.SATISFIABLE => Some(false)
      case Status.UNKNOWN => None
      case Status.UNSATISFIABLE => Some(true)
    }
  }

  def this() = this(Map())
  /** Not thread safe */
  private def toCMathML(expr: Expr) : CMathML = expr match {
    case e: ArithExpr if e.isAdd => Apply(CMathML.plus, e.getArgs map toCMathML: _*)
    case e: ArithExpr if e.isMul => Apply(CMathML.times, e.getArgs map toCMathML: _*)
    case e: ArithExpr if e.isSub => Apply(CMathML.minus, e.getArgs map toCMathML: _*)
    case e: ArithExpr if e.isDiv => Apply(CMathML.divide, e.getArgs map toCMathML: _*)
    case e: ArithExpr if e.isUMinus => Apply(CMathML.uminus, e.getArgs map toCMathML: _*)
    case e: BoolExpr if e.isEq => Apply(CMathML.equal, e.getArgs map toCMathML: _*)
    case e: ArithExpr if e.isConst => CI(e.getFuncDecl.getName.toString)
    case e: RatNum =>
      val num = e.getBigIntNumerator
      val denom = e.getBigIntDenominator
      val num2 = BigDecimal(num, CN.MATHCONTEXT)
      if (denom == BigInteger.ONE) CN(num2)
      else {
        val denom2 = BigDecimal(denom, CN.MATHCONTEXT)
        try CN(num2 / denom2)
        catch {
          case _: ArithmeticException => CMathML.divide(CN(num2), CN(denom2))
        }
      }
    case e => throw new MathException("cannot convert from Z3 to CMathML: "+e)
  }

  def fromCMathML(m: CMathML) = synchronized { new Z3.Expr(this, fromCMathML_(m)) }

  /** Not thread safe */
  private def fromCMathML_(m: CMathML) : Expr = m match {
    case CN(_, i) => context.mkNumeral(i.toString, realSort_)
    case CI(_, n) => context.mkConst(n, realSort_)
    case Apply(_, CMathML.plus,x,y) => context.mkAdd(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                  fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, CMathML.minus,x,y) => context.mkSub(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                   fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, CMathML.times,x,y) => context.mkMul(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                   fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, CMathML.divide,x,y) => context.mkDiv(fromCMathML_(x).asInstanceOf[ArithExpr],
                                                    fromCMathML_(y).asInstanceOf[ArithExpr])
    case Apply(_, CMathML.equal,x,y) => context.mkEq(fromCMathML_(x),fromCMathML_(y))
    case Apply(_, CMathML.uminus,x) => context.mkUnaryMinus(fromCMathML_(x).asInstanceOf[ArithExpr])
  }


  Z3 // Make sure the static initializer of Z3 is invoked
  private val context = new Context(JavaConversions.mapAsJavaMap(config))
//  def mkSymbol(name: String) = synchronized { new Z3.Symbol(context.mkSymbol(name)) }
//  def mkSymbol(i: Int) = synchronized { new Z3.Symbol(context.mkSymbol(i)) }
  private lazy val boolSort_ = context.mkBoolSort()
  private lazy val intSort_ = context.mkIntSort()
  private lazy val realSort_ = context.mkRealSort()

  /** Declares a new function. Note: the range of the function is given <b>first</b>!
    * (Hence the suffix RD)
    *
    * @param name
    * @param range
    * @param domain
    */
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
