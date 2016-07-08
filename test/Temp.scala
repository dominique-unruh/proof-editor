import java.util

import com.microsoft.z3._
import java.io.File

import misc.Utils
import org.apache.bcel.classfile.JavaClass
import org.apache.commons.io.IOUtils
import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter, Opcodes}
import test.UnitSpec
import z3.Z3

class Temp extends UnitSpec {
  "z3.Z3" should "be tried" in {
    val z3 = new Z3(Map("model"->"true"))

    Z3.toggleWarningMessages(true)


    val ctx = z3.context

    val fname = z3.mkSymbol("f")
    val x = z3.mkSymbol("x")
    val y = z3.mkSymbol("y")

    val bs = z3.boolSort

    val f = z3.mkFuncDeclRD(fname, range=bs, domain=bs, bs)
    val fapp = z3.mkApp(f, z3.mkConst(x, bs), z3.mkConst(y, bs))

    val fargs2 = Array[Expr]()
    val domain2 = Array[Sort](bs)
    val fapp2 = z3.mkApp(z3.mkFreshFuncDeclRD("fp", range=bs, domain=bs), z3.mkFreshConst("cp", bs))

    val trivial_eq = z3.mkEq(fapp, fapp)
    val nontrivial_eq = z3.mkEq(fapp, fapp2)

    val g = z3.mkGoal(true, false, false)
    g.add(trivial_eq, nontrivial_eq)
    System.out.println("Goal: " + g)

  }
}
