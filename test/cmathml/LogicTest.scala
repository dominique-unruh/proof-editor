package cmathml

import cmathml.CMathML.fns1
import org.scalatest.FunSuite

class LogicTest extends FunSuite {
  test("instantiateLambda") {
    val math = fns1.lambda(List(CI("x"),CI("y")),CI("x")+CI("y"))
    val inst = Logic.instantiateLambda(math,CN(1),CN(2))
    assert(inst == CN(1)+CN(2))
  }
}
