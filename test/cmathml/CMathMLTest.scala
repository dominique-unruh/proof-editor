package cmathml

import test.UnitSpec
import CMathML._
import z3.Z3

class CMathMLTest extends UnitSpec {
  test("CMathML.replace") {
    val m = divide(CN(1),CN(2))
    val p = Path(1)
    assertResult(divide(CN(99),CN(2))) {
      m.replace(p,CN(99))
    }
  }

  test("CMathML.subterm") {
    for ((m,p,s) <- CMathMLTest.subtermTests)
      assert(m.subterm(p)==s)
  }

  /** A few dummy lines to achieve higher test coverage */
  test("missing-coverage") {
//    assertResult(Some(BigDecimal(1,CN.MATHCONTEXT))) { CN.unapply(CN(1)) }
  }

  test("CN.isNegative") {
    assert(!CN(1).isNegative)
    assert(!CN(0).isNegative)
    assert(CN(-1).isNegative)
  }

  test("XML") {
    assertResult("<ci>x</ci>") { CI("x").toXML.toString }
    assertResult("""<csymbol cd="arith1">times</csymbol>""") { times.toXML.toString }
    assertResult("""<cn type="integer">123</cn>""") { CN(123).toXML.toString }
    assertResult("""<cn type="real">0.5</cn>""") { CN(0.5).toXML.toString }
  }

  test("toPopcorn") {
    assertResult("$x") { CI("x").toPopcorn }
    assertResult("arith1.times") { times.toPopcorn }
    assertResult("123") { CN(123).toPopcorn }
//    assertResult("0.5") { CN(0.5).toPopcorn }  // Not supported yet
  }

  test("popcorn roundtrip") {
    for (e <- CMathMLTest.cMathMLRoundtrips) {
      val p = e.toPopcorn
      assertResult(e) { CMathML.fromPopcorn(p) }
    }
  }
}

object CMathMLTest {
  val cMathMLRoundtrips = List(
    plus(CN(1),CN(2)),
    CI("x"),
    equal(CI("x"),CN(2)),
    CN(1.23),
    divide(CN(2),CN(2)),
    minus(CN(3),CN(2)),
    times(minus(CN(4),CN(2)),CN(1.2)),
    uminus(CI("x")),
    uminus(CN("123")),
    CN(-234),
    power(CN(3),CI("x"))
  )

  val subtermTests = List(
    ( divide(CN(1),CN(2)), Path(1), CN(1) ),
    ( equal(CI("x")+CI("y"), CI("y")+CN(-1)), Path(2,2), CN(-1) ),
    ( equal(CI("x")+CI("y"), CI("y")+CN(-1)), Path(2,1), CI("y") )
  )
}