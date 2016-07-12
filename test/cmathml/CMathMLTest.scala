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
    assertResult(Some(BigDecimal(1,CN.MATHCONTEXT))) { CN.unapply(CN(1)) }
  }
}
