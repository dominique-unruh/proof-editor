package z3

import com.microsoft.z3.{BoolSort, _}
import misc.Utils
import org.apache.commons.io.IOUtils
import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter, Opcodes}

import scala.collection.{JavaConversions, JavaConverters}

class Goal(val goal:com.microsoft.z3.Goal) extends AnyVal {
  def add(exprs: BoolExpr*) : Unit = goal.add(exprs:_*)
  override def toString = goal.toString
}

class Z3(config:Map[String,String]) {
  Z3 // Make sure the static initializer of Z3 is invoked
  val context = new Context(JavaConversions.mapAsJavaMap(config))
  def mkSymbol(name:String) = context.mkSymbol(name)
  def mkSymbol(i:Int) = context.mkSymbol(i)
  lazy val boolSort = context.mkBoolSort()

  /** Declares a new function. Note: the range of the function is given <b>first</b>!
    * (Hence the suffix RD)
    *
    * @param name
    * @param range
    * @param domain
    */
  def mkFuncDeclRD(name:Symbol, range:Sort, domain:Sort*) = context.mkFuncDecl(name,domain.toArray,range)
  def mkFreshFuncDeclRD(name: String, range: Sort, domain: Sort*): FuncDecl = context.mkFreshFuncDecl(name,domain.toArray,range)
  def mkConst(x: Symbol, sort: BoolSort) = context.mkConst(x,sort)
  def mkFreshConst(name: String, sort: Sort) : Expr = context.mkFreshConst(name,sort)
  def mkApp(f: FuncDecl, args: Expr*) = context.mkApp(f,args:_*)
  def mkGoal(models: Boolean, unsatCores: Boolean, proofs: Boolean) = new Goal(context.mkGoal(models,unsatCores,proofs))
  def mkEq(a: Expr, b: Expr) = context.mkEq(a,b)
}

object Z3 {

  monkeyPatchZ3Native()
  loadLib("/libz3.so.0")
  loadLib("/libz3java.so")

  private def loadLib(name:String) = {
//    try {
       System.load(Utils.resourceFile(name))
//    } catch {
//      case e : java.lang.UnsatisfiedLinkError if e.getMessage.endsWith(" already loaded in another classloader") => ()
//    }
  }


  def majorVersion = Version.getMajor
  def minorVersion = Version.getMinor
  def version = Version.getString
  def toggleWarningMessages(enabled: Boolean) = com.microsoft.z3.Global.ToggleWarningMessages(enabled)

  /** Loads the class [[com.microsoft.z3.Native]] with the static initializer removes.
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
  }
}
