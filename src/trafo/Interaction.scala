package trafo
import scala.language.existentials
import cmathml.CMathML
import misc.Pure
import theory.{Formula, FormulaRef}

import scala.annotation.tailrec
import scala.util.control.Breaks
//import theory.Formula

import scala.runtime.BoxedUnit
import scala.xml.Elem


abstract class Question[T <: AnyRef] {
  // T <: Object because we want T to be a reference type (else we run into the problem from http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
  val answerType : Class[T]
  def message : scala.xml.Elem
  val default : T
}

class FormulaQ(val message:Elem) extends Question[Option[FormulaRef]] {
  val answerType = classOf[Option[FormulaRef]]
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
  override def toString = "[MSG: "+message.text+"]"
}

final case class InteractionRunning[T](val id: String, val question : Question[A] forSome {type A <: AnyRef}, val answer : AnyRef => Interaction[T]) extends Interaction[T] {
  override def resultMaybe: Option[T] = None
  override def isDone: Boolean = false
  override def isRunning: Boolean = true
  override def isFailed: Boolean = false
}
final case class InteractionFinished[T](val result: T) extends Interaction[T] {
  override def resultMaybe: Option[T] = Some(result)
  override def isDone: Boolean = true
  override def isRunning: Boolean = false
  override def isFailed: Boolean = false
}
final case class InteractionFailed[T]() extends Interaction[T] {
  override def resultMaybe: Option[T] = None
  override def isDone: Boolean = false
  override def isRunning: Boolean = false
  override def isFailed: Boolean = true
}

sealed trait Interaction[T] {
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
  @Pure final def quickInteract(answers : AnyRef*) : (T,List[MessageQ]) = {
    val (inter,msgs) = quickInteract0(answers : _*)
    inter match {
      case InteractionFinished(result) => (result,msgs)
      case InteractionFailed() => throw new RuntimeException("interaction failed "+msgs.mkString(","))
      case InteractionRunning(_,question,_) => throw new RuntimeException("interaction incomplete, expecting "+question.message.text)
    }
  }
  @Pure final def quickInteract0(answers : AnyRef*) : (Interaction[T],List[MessageQ]) = {
    import scala.util.control.Breaks._
    var inter = this
    var messages = Nil: List[MessageQ]
    val answersIt = answers.iterator
    breakable {
      while (true)
        inter match {
          case InteractionRunning(id, msg: MessageQ, answer) =>
            messages = msg :: messages
            inter = answer(BoxedUnit.UNIT)
          case InteractionRunning(id, question, answer) =>
            if (answersIt.hasNext)
              inter = answer(answersIt.next)
            else
              break
          case _ => break
        }
    }
    (inter, messages.reverse)
  }
  def resultMaybe : Option[T]
  def isFailed : Boolean
  def isRunning : Boolean
  def isDone : Boolean
}

object Interaction {
  def fail[T] = new InteractionFailed[T]()
  def returnval[T](res : T) = new InteractionFinished[T](res)
  def ask[T <: AnyRef](id : String, question : Question[T]) =
    new InteractionRunning[T](id,question, { a =>
      assert(a!=null)
      assert(question.answerType.isInstance(a), "answer "+a+" should be of type "+question.answerType+" not "+a.getClass) // equivalent to "answer.isInstanceOf[T]"
      Interaction.returnval(a.asInstanceOf[T])
    })
  def error(id:String,err:Elem) : Interaction[Unit] =
    new InteractionRunning[Unit](id,new MessageQ(err),{a=>returnval(())})
  def failWith[T](id:String, err:Elem) : Interaction[T] =
    for { _ <- error(id, err); res <- fail[T] } yield res
  val skip : Interaction[Unit] = returnval(())
}
