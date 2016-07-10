package trafo

import cmathml.CMathML

import scala.xml.Elem

abstract class Question[T <: AnyRef] {
  // T <: Object because we want T to be a reference type (else we run into the problem from http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
  val answerType : Class[T]
  def message : scala.xml.Elem
  val default : T
}

//class FormulaA(val formula:CMathML) extends Answer
class FormulaQ(val message:Elem) extends Question[Option[CMathML]] {
  val answerType = classOf[Option[CMathML]]
  val default = None
}

class IntQ(val message:Elem) extends Question[Integer] {
  val answerType = classOf[Integer]
  val default = 0.asInstanceOf[Integer]
}

class StringQ(val message:Elem) extends Question[String] {
  val answerType = classOf[String]
  val default = ""
}



case class ErrorMessage(message:Elem)

/** Valid combinations:
  * question=Some, result=None, error=*
  * question=None, result=Some, error=*
  * question=None, result=None, error=Some
  * @tparam T
  */
abstract class Interaction[T] /*with FilterMonadic*/ {
  val question : Option[Question[_ <: AnyRef]]
  def answer(answer : AnyRef) : Interaction[T]
  val result : Option[T]
  val error: Option[ErrorMessage]
  /** Must be "" if question=None, else non-"" */
  val id: String
  final def flatMap[U](f: T => Interaction[U]) : Interaction[U] = {
    if (question.isEmpty) {
      if (result.isEmpty) Interaction.error[U](error.get)
      else if (error.isEmpty) f(result.get)
      else f(result.get).withError(error.get)
    } else {
      val orig = this
      new Interaction[U] {
        override def answer(answer: AnyRef): Interaction[U] = orig.answer(answer).flatMap(f)
        override val result: Option[U] = None
        override val question: Option[Question[_<:AnyRef]] = orig.question
        override val error = orig.error
        override val id = orig.id
      }
    }
  }
  def map[U](f: T => U) = flatMap((x:T) => Interaction.returnval(f(x)))
  def withError(err:ErrorMessage) = { val self = this; new Interaction[T] {
    override val question: Option[Question[_<:AnyRef]] = self.question
    override def answer(answer: AnyRef): Interaction[T] = self.answer(answer)
    override val result: Option[T] = self.result
    override val error: Option[ErrorMessage] = Some(err)
    override val id = self.id
  }}
}
object Interaction {
  def returnval[T](res : T) = new Interaction[T] {
    val result = Some(res)
    val question = None
    def answer(answer: AnyRef) = sys.error("answer called without question")
    val error: Option[ErrorMessage] = None
    val id = ""
  }
  def ask[T <: Object](id_ : String, q : Question[T], err:Option[ErrorMessage]=None) = new Interaction[T] {
    override val error = err
    override val question: Option[Question[T]] = Some(q)
    override def answer(answer: AnyRef): Interaction[T] = {
      assert(answer!=null)
      assert(q.answerType.isInstance(answer), "answer "+answer+" should be of type "+q.answerType+" not "+answer.getClass) // equivalent to "answer.isInstanceOf[T]"
      Interaction.returnval(answer.asInstanceOf[T])
    }
    override val result: Option[T] = None
    override val id = id_
  }
  def error[T](err:ErrorMessage) : Interaction[T] = new Interaction[T] {
    override val result = None
    override val question = None
    override def answer(answer: AnyRef): Interaction[T] = sys.error("no question asked")
    override val error: Option[ErrorMessage] = Some(err)
    override val id = ""
  }
}
