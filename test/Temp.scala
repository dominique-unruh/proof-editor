import java.util

import com.microsoft.z3._
import java.io.File

import cmathml.{CI, CMathML, CN}
import misc.Utils
import org.apache.bcel.classfile.JavaClass
import org.apache.commons.io.IOUtils
import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter, Opcodes}
import test.UnitSpec
import z3.Z3

class Temp extends UnitSpec {
  test("temp z3") {
    val z3 = new Z3(Map("model"->"true"))
    Z3.toggleWarningMessages(true)

    val cmathml = CMathML.equal(CMathML.plus(CI("x"),CI("y")),
      CMathML.plus(CI("y"),CN(-1)))

    val expr = z3.fromCMathML(cmathml)

    val goal = z3.mkGoal(true,true,false)
    goal.add(expr.asInstanceOf[BoolExpr])

    println("Goal: " + goal)

    val goal2 = goal.simplify

//    println(goal2.goal.getNumExprs)

    val expr2 = goal2.getFormula

    println("Simplified: " + expr2)

    println(expr2.getArgs.toList)
    println(expr2.getFuncDecl)

    val c2 = z3.toCMathML(expr2)

    println("CMathML "+c2)
  }
}
