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
    // some related mysteries
    println(10.getClass)
    println(10.isInstanceOf[Int])
    println(classOf[Int].isInstance(10))
    println(10.getClass.isInstance(10))
    println(classOf[Integer].isInstance(10))

    def int = ask(new IntQ(<span>int?</span>))
    def int2 = int.answer(10)
    assertResult(10) { int2.result.get }
  }

  test("for-comprehension") {
    def q = new StringQ(<span>some string</span>)
    println(q.message)

    def int : Interaction[String] = for { i <- ask(q) } yield i+i
    assertResult(None) { int.error }
    assertResult(None) { int.result }
    assertResult("some string") { int.question.get.message.text }

    def int2 = int.answer("abc")
    assertResult(None) { int2.error }
    assertResult(None) { int2.question }
    assertResult("abcabc") { int2.result.get }
  }
}
