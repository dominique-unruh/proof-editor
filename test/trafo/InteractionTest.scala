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

  test("ask for int") {
    // make sure we don't run into the problem from
    // http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
    def int = ask("q1",new IntQ(<span>int?</span>))
    def int2 = int.answer(Some(10))
    assertResult(10) { int2.result.get }
  }

  test("for-comprehension") {
    def q = new StringQ(<span>some string</span>)
    println(q.message)

    def int : Interaction[String] = for { i <- ask("q1",q) } yield i.get+i.get
    assertResult(None) { int.error }
    assertResult(None) { int.result }
    assertResult("some string") { int.question.get.message.text }

    def int2 = int.answer(Some("abc"))
    assertResult(None) { int2.error }
    assertResult(None) { int2.question }
    assertResult("abcabc") { int2.result.get }
  }
}
