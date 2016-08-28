import cmathml.CI
import test.UnitSpec
import cmathml.CMathML.logic1
import com.microsoft.z3.Context
import z3.Z3

import scala.collection.JavaConversions

class Temp extends UnitSpec {
  test("temp") {
    val context = new Context()
    val boolSort = context.mkBoolSort()
    val e = context.mkConst("true", boolSort)
    val tr = context.mkTrue()

    println(e,tr)

    e.getFuncDecl.getDeclKind

    println(e.isTrue,tr.isTrue)

//
//    val e = logic1.trueSym
//    val f = CI("true")
//    println(e)
//    val z = Z3.default.fromCMathML(e)
//    val zf = Z3.default.fromCMathML(f)
//    println(z,zf)
//
//    val e2 = z.toCMathML
//    val f2 = zf.toCMathML
//    println(e2,f2)
  }
}
