package cmathml

import test.UnitSpec

import cmathml.CMathML._

class MutableCMathMLTest extends UnitSpec {

  test("roundTrip") {
    val expressions = List(
      plus(CN(1),CN(2)),
      CI("x"),
      equal(CI("x"),CN(2)),
      CN(1.23),
      divide(CN(2),CN(2)),
      minus(CN(3),CN(2)),
      times(minus(CN(4),CN(2)),CN(1.2)),
      uminus(CI("x")),
      uminus(CN("123")),
      CN(-234)
    )

    for (e<-expressions) {
      val root = new MutableCMathMLDocument(e)
      assert(root.toCMathML==e)
    }
  }
}
