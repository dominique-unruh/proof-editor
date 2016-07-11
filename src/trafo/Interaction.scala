package trafo
import scala.language.existentials

import cmathml.CMathML
import theory.Formula

import scala.runtime.BoxedUnit
import scala.xml.Elem


abstract class Question[T <: AnyRef] {
  // T <: Object because we want T to be a reference type (else we run into the problem from http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
  val answerType : Class[T]
  def message : scala.xml.Elem
  val default : T
}

class FormulaQ(val message:Elem) extends Question[Option[Formula]] {
  val answerType = classOf[Option[Formula]]
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



class MessageQ(val message:Elem) extends Question[BoxedUnit] { // TODO: add message kind (error, warn, info)
  val answerType = classOf[BoxedUnit]
  val default = BoxedUnit.UNIT
}


//case class ErrorMessage(message:Elem)


final case class InteractionRunning[T](val id: String, val question : Question[A] forSome {type A <: AnyRef}, val answer : AnyRef => Interaction[T]) extends Interaction[T]
final case class InteractionFinished[T](val result: T) extends Interaction[T]
final case class InteractionFailed[T]() extends Interaction[T]

sealed trait Interaction[T] /*with FilterMonadic*/ {
  final def flatMap[U](f: T => Interaction[U]) : Interaction[U] = this match {
    case InteractionRunning(id, question, answer) => InteractionRunning(id, question, a => answer(a).flatMap(f))
    case InteractionFinished(result) => f(result)
    case InteractionFailed() => InteractionFailed()
  }
  final def map[U](f: T => U) : Interaction[U] = this match {
    case InteractionRunning(id, question, answer) => InteractionRunning(id, question, a => answer(a).map(f))
    case InteractionFinished(result) => InteractionFinished(f(result))
    case InteractionFailed() => InteractionFailed()
  }
}
object Interaction {
  def fail[T <: AnyRef] = new InteractionFailed[T]()
  def returnval[T <: AnyRef](res : T) = new InteractionFinished[T](res)
  def ask[T <: AnyRef](id : String, question : Question[T]) =
    new InteractionRunning[T](id,question, { a =>
      assert(a!=null)
      assert(question.answerType.isInstance(a), "answer "+a+" should be of type "+question.answerType+" not "+a.getClass) // equivalent to "answer.isInstanceOf[T]"
      Interaction.returnval(a.asInstanceOf[T])
    })
  def error(id:String,err:Elem) : Interaction[BoxedUnit] =
    new InteractionRunning[BoxedUnit](id,new MessageQ(err),{a=>returnval(BoxedUnit.UNIT)})
}
