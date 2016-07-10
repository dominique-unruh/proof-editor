package trafo

import cmathml.{CMathML, CN}
import test.UnitSpec
import Interaction._

class InteractionTest extends UnitSpec {
  test("returnval") {
    def int = returnval(123)
    assertResult(123) { int.asInstanceOf[InteractionFinished[_]].result }
  }

  test("ask for int") {
    // make sure we don't run into the problem from
    // http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
    def int = ask("q1",new IntQ(<span>int?</span>))
    def int2 = int.answer(10.asInstanceOf[Integer])
    assertResult(10) { int2.asInstanceOf[InteractionFinished[_]].result }
  }

  test("for-comprehension") {
    def q = new StringQ(<span>some string</span>)
    println(q.message)

    def int : Interaction[String] = for { i <- ask("q1",q) } yield i+i
    assertResult("some string") { int.asInstanceOf[InteractionRunning[_]].question.message.text }

    def int2 = int.asInstanceOf[InteractionRunning[_]].answer("abc")
    assertResult("abcabc") { int2.asInstanceOf[InteractionFinished[_]].result }
  }
}
