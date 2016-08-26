package z3

import java.math.BigInteger

import cmathml.CMathML.relation1._
import cmathml.CMathML.{arith1, quant1, relation1}
import cmathml._
import com.microsoft.z3.{Expr, RatNum, Version}
import test.UnitSpec

class Z3Test extends UnitSpec {


  test("Z3 initializes") {
    println("version",Z3.version)
  }

  test("integer conversion") {
    val z3 = new Z3(Map())
    val e = z3.fromCMathML(CN(12))
    assert(e.toString=="12")
//    println("e = "+e)
//    val d = e.asInstanceOf[RatNum].getBigIntDenominator
//    println("d = "+d)
//    println(d==BigInteger.ONE)
//    val c = e.toCMathML
//    println("c = "+c)
//    assertResult(CN(12)) { c }
  }

  test("roundtrips CMathML<->Expr") {
    val z3 = new Z3(Map())
    import cmathml.CMathML._
    for (e <- CMathMLTest.cMathMLRoundtrips) {
      val e2 = z3.fromCMathML(e)
      assertResult(e) { e2.toCMathML }
    }
  }

  test("convert CMathML to Expr") {
    val z3 = new Z3(Map())
    val cmathml = relation1.equal(CN(1)+CN(2), CN(4) + CN(-1))
    assertResult("(= (+ 1.0 2.0) (+ 4.0 (- 1.0)))") {
      z3.fromCMathML(cmathml).toString
    }
  }
}