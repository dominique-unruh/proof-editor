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
}
