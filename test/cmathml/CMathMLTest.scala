package cmathml

import test.UnitSpec

import CMathML._

class CMathMLTest extends UnitSpec {
  test("CMathML.replace") {
    val m = divide(CN(1),CN(2))
    val p = Path(1)
    assertResult(divide(CN(99),CN(2))) {
      m.replace(p,CN(99))
    }
  }

  test("CMathML.subterm") {
    val m = divide(CN(1),CN(2))
    val p = Path(1)
    assertResult(CN(1)) {
      m.subterm(p)
    }
  }

  /** A few dummy lines to achieve higher test coverage */
  test("missing-coverage") {
//    assertResult(Some(BigDecimal(1,CN.MATHCONTEXT))) { CN.unapply(CN(1)) }
  }

  test("CN.isNegative") {
    assert(CN(1).isNegative == false)
    assert(CN(0).isNegative == false)
    assert(CN(-1).isNegative == true)
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
}
