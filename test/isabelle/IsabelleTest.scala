package isabelle

import java.math.BigInteger

import cmathml.CMathML.relation1._
import cmathml.CMathML.{arith1, quant1, relation1}
import cmathml._
import test.UnitSpec
import isabelle.Isabelle.{defaultInstance => isa}

class IsabelleTest extends UnitSpec {

  test("Isabelle initializes") {
    Isabelle.defaultInstance.ping()
  }

  test("integer conversion") {
    val e = Isabelle.fromCMathML(CN(12))
    assert(isa.termToString(e) == "12::real")
    //    println("e = "+e)
    //    val d = e.asInstanceOf[RatNum].getBigIntDenominator
    //    println("d = "+d)
    //    println(d==BigInteger.ONE)
    //    val c = e.toCMathML
    //    println("c = "+c)
    //    assertResult(CN(12)) { c }
  }

  test("roundtrips CMathML<->Expr") {
    import cmathml.CMathML._
    for (e <- CMathMLTest.cMathMLRoundtrips) {
      print(s"$e -> ")
      val e2 = Isabelle.fromCMathML(e)
      println(e2)
      assertResult(e) { Isabelle.toCMathML(e2) }
    }
  }

  test("convert CMathML to Expr") {
    val cmathml = relation1.equal(CN(1)+CN(2), CN(4) + CN(-1))
    assertResult("(1::real) + (2::real) = (4::real) + - (1::real)") {
      isa.termToString(Isabelle.fromCMathML(cmathml))
    }
  }
}
