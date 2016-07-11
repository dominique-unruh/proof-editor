package z3

import java.math.BigInteger

import cmathml.{Apply, CI, CMathML, CN}
import com.microsoft.z3.{Expr, RatNum, Version}
import test.UnitSpec

class Z3Test extends UnitSpec {


  test("Z3 initializes") {
    println("version",Z3.version)
  }

  test("integer conversion") {
    val z3 = new Z3(Map())
    val e = z3.fromCMathML(CN(12))
    println("e = "+e)
    val d = e.asInstanceOf[RatNum].getBigIntDenominator
    println("d = "+d)
    println(d==BigInteger.ONE)
    val c = z3.toCMathML(e)
    println("c = "+c)
    assertResult(CN(12)) { c }
  }

  test("roundtrips CMathML<->Expr") {
    val z3 = new Z3(Map())
    import cmathml.CMathML._
    val expressions = List(
      plus(CN(1),CN(2)),
      CI("x"),
      equal(CI("x"),CN(2)),
      CN(1.23),
      divide(CN(2),CN(2))
    )
    for (e <- expressions) {
      val e2 = z3.fromCMathML(e)
      assertResult(e) { z3.toCMathML(e2) }
    }
  }

  test("non-decimal fraction to CMathML") {
    val z3 = new Z3(Map())
    val frac23 = z3.context.mkNumeral("2/3",z3.realSort)
    println(frac23)
    assert(frac23.isInstanceOf[RatNum])
    assertResult(CMathML.divide(CN(2),CN(3))) { z3.toCMathML(frac23) }
  }

  test("convert CMathML to Expr") {
    val z3 = new Z3(Map())
    val cmathml = CMathML.equal(CMathML.plus(CN(1),CN(2)),
                                CMathML.plus(CN(4),CN(-1)))
    assertResult("(= (+ 1.0 2.0) (+ 4.0 (- 1.0)))") {
      z3.fromCMathML(cmathml).toString
    }
  }
}