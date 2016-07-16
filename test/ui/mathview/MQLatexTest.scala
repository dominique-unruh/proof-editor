package ui.mathview

import cmathml.{CI, CN}
import cmathml.CMathML._
import test.UnitSpec

/**
  * Created by unruh on 7/16/16.
  */
class MQLatexTest extends UnitSpec {
  test("addition") {
    assertResult(CN(1)+CN(2)) { MQLatex.parseLatex("1+2") }
  }

  test("addition assoc") {
    assertResult( (CN(1)+CN(2)) + CN(3) ) { MQLatex.parseLatex("1+2+3") }
  }

  test("mix plus minus") {
    assertResult( (CN(1)+CN(2)) - CN(3) ) { MQLatex.parseLatex("1+2-3") }
    assertResult( (CN(1)-CN(2)) + CN(3) ) { MQLatex.parseLatex("1-2+3") }

  }

  test("invisible times") {
    assertResult( CI("a") * CI("b") ) { MQLatex.parseLatex("ab") }
  }

  test("int") {
    assert(MQLatex.parseLatex("23")==CN(23))
  }

  test("negative int") {
    assert(MQLatex.parseLatex("-23")==CN(-23))
  }
}
