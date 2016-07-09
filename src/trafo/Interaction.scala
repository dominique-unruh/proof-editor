package trafo

import cmathml.CMathML

import scala.xml.Elem

abstract class Answer

abstract class Question[T <: Answer] {
  val answerType : Class[T]
  def message : scala.xml.Elem
}

class FormulaA(val formula:CMathML) extends Answer
class FormulaQ(val message:Elem) extends Question[FormulaA] {
  val answerType = classOf[FormulaA]
}

class IntA(val i:Int) extends Answer
class IntQ(val message:Elem) extends Question[IntA] {
  val answerType = classOf[IntA]
}

case class ErrorMessage(message:Elem)

/** Valid combinations:
  * question=Some, result=None, error=*
  * question=None, result=Some, error=*
  * question=None, result=None, error=Some
  * @tparam T
  */
abstract class Interaction[T] /*with FilterMonadic*/ {
  val question : Option[Question[_]]
  def answer(answer : Answer) : Interaction[T]
  val result : Option[T]
  val error: Option[ErrorMessage]
  def flatMap[U](f: T => Interaction[U]) : Interaction[U] = {
    if (question.isEmpty) {
      if (result.isEmpty) Interaction.error[U](error.get)
      else if (error.isEmpty) f(result.get)
      else f(result.get).withError(error.get)
    } else {
      val orig = this
      new Interaction[U] {
        override def answer(answer: Answer): Interaction[U] = orig.answer(answer).flatMap(f)
        override val result: Option[U] = None
        override val question: Option[Question[_]] = orig.question
        override val error = orig.error
      }
    }
  }
  def map[U](f: T => U) = flatMap((x:T) => Interaction.returnval(f(x)))
  def withError(err:ErrorMessage) = { val self = this; new Interaction[T] {
    override val question: Option[Question[_]] = self.question
    override def answer(answer: Answer): Interaction[T] = self.answer(answer)
    override val result: Option[T] = self.result
    override val error: Option[ErrorMessage] = Some(err)
  }}
}
object Interaction {
  def returnval[T](res : T) = new Interaction[T] {
    val result = Some(res)
    val question = None
    def answer(answer: Answer) = sys.error("answer called without question")
    val error: Option[ErrorMessage] = None
  }
  def ask[T <: Answer](q : Question[T], err:Option[ErrorMessage]=None) = new Interaction[T] {
    override val error = err
    override val question: Option[Question[T]] = Some(q)
    override def answer(answer: Answer): Interaction[T] = {
      assert(answer!=null)
      assert(q.answerType.isInstance(answer)) // equivalent to "answer.isInstanceOf[T]"
      Interaction.returnval(answer.asInstanceOf[T])
    }
    override val result: Option[T] = None
  }
  def error[T](err:ErrorMessage) : Interaction[T] = new Interaction[T] {
    override val result = None
    override val question = None
    override def answer(answer: Answer): Interaction[T] = sys.error("no question asked")
    override val error: Option[ErrorMessage] = Some(err)
  }
}

//class WithError[T](err:ErrorMessage, interaction : Interaction[T]) extends Interaction[T] {
//  override val question: Option[Question[_]] = interaction.question
//  override def answer(answer: Answer): Interaction[T] = interaction.answer(answer)
//  override val result: Option[T] = interaction.result
//  override val error: Option[ErrorMessage] = Some(err)
//}

//class InteractionDone[T](res : T) extends Interaction[T] {
//  val result = Some(res)
//  val question = None
//  def answer(answer: Answer) = sys.error("answer called without question")
//  val error: Option[ErrorMessage] = None
//}

//class ErrorInteraction[T](err : ErrorMessage) extends Interaction[T] {
//  override val result = None
//  override val question = None
//  override def answer(answer: Answer): Interaction[T] = sys.error("no question asked")
//  override val error: Option[ErrorMessage] = Some(err)
//}

//class InteractionAsk[T <: Answer](q : Question[T], val error : Option[ErrorMessage] = None) extends Interaction[T] {
//  override val question: Option[Question[T]] = Some(q)
//  override def answer(answer: Answer): Interaction[T] = {
//    assert(answer!=null)
//    assert(q.answerType.isInstance(answer)) // equivalent to "answer.isInstanceOf[T]"
//    Interaction.returnval(answer.asInstanceOf[T])
//  }
//  override val result: Option[T] = None
//}