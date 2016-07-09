package trafo

import cmathml.{CMathML, CN}
import test.UnitSpec
import Interaction._

class InteractionTest extends UnitSpec {
  test("returnval") {
    def int = returnval(123)
    assertResult(None) { int.error }
    assertResult(None) { int.question }
    assertResult(Some(123)) { int.result }
  }

  test("for-comprehension") {
    def q = new IntQ(<span>some int</span>)
    println(q.message)

    def int : Interaction[Int] = for { i <- ask(q) } yield i.i*i.i
    assertResult(None) { int.error }
    assertResult(None) { int.result }
    assertResult("some int") { int.question.get.message.text }

    def int2 = int.answer(new IntA(10))
    assertResult(None) { int2.error }
    assertResult(None) { int2.question }
    assertResult(100) { int2.result.get }
  }
}
