package trafo

import test.UnitSpec
import trafo.Interaction._

import scala.runtime.BoxedUnit

object InteractionTest {
  def questionMessage(int : Interaction[_]) : String = int match {
    case InteractionRunning(_,question,_) => question.message.text
    case _ => null : String
  }

  def answerQuestion[T](int : Interaction[T], ans: AnyRef) : Interaction[T] = int match {
    case InteractionRunning(_,q,a) =>
//      assert(q.answerType.isInstance(ans))
      val resp = a(ans)
      assert(resp!=null)
      resp
    case _ => null : Interaction[T]
  }

  def interactionResult[T](int : Interaction[T]) : T = int match {
    case InteractionFinished(result) => assert(result!=null); result
    case _ => null.asInstanceOf[T]
  }

  def isFailed[T](int : Interaction[T]) : Boolean = int match {
    case InteractionFailed() => true
    case _ => false
  }
}

class InteractionTest extends UnitSpec {
  import InteractionTest._

  test("returnval") {
    val int = returnval(123 : Integer)
    assertResult(123) { interactionResult(int) }
  }

  test("fail") {
    val int = Interaction.fail
    assert(isFailed(int))
  }

  test("fail in for (last step)") {
    val int : Interaction[Integer] = for { i <- Interaction.fail[Integer] } yield i
    assert(isFailed(int))
  }

  test("fail in for (prev step)") {
    val int = for { i <- Interaction.fail[Integer]; j <- returnval(3 : Integer) } yield j
    assert(isFailed(int))
  }

  test("ask for int") {
    // make sure we don't run into the problem from
    // http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
    val int = ask("q1",new IntQ(<span>int?</span>))
    val int2 = answerQuestion(int,10 : Integer)
    assertResult(10) { interactionResult(int2) }
  }

  test("for-comprehension") {
    val q = new StringQ(<span>some string</span>)
    println(q.message)

    val int : Interaction[String] = for { i <- ask("q1",q) } yield i+i
    assertResult("some string") { questionMessage(int) }

    val int2 = int.asInstanceOf[InteractionRunning[_]].answer("abc")
    assertResult("abcabc") { int2.asInstanceOf[InteractionFinished[_]].result }
  }

  test("for-comprehesion, 2 lines") {
    val int = for { i <- ask("q1",new IntQ(<span>q1</span>))
                    s <- ask("q2",new StringQ(<span>q2</span>))
        } yield s * i
    assertResult("q1") { questionMessage(int) }
    val int2 = answerQuestion(int,3 : Integer)
    assertResult("q2") { questionMessage(int2) }
    val int3 = answerQuestion(int2,"test")
    assertResult("testtesttest") { interactionResult(int3) }

  }
}
