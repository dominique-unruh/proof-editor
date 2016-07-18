package trafo
import scala.language.existentials
import cmathml.CMathML
import misc.Pure
import theory.Formula

import scala.annotation.tailrec
import scala.reflect.api.TypeTags
import scala.util.control.Breaks
import scala.runtime.BoxedUnit
import scala.xml.Elem

import scala.reflect.runtime.universe._


abstract class Question[T] {
  // T <: Object because we want T to be a reference type (else we run into the problem from http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
  val answerType : TypeTag[T]
  val questionType : TypeTag[_ <: Question[T]]
  def message : scala.xml.Elem
  val default : T
}

class FormulaQ(val message:Elem,
               @deprecated val newFormula:Formula=null
              ) extends Question[Option[Formula]] {
  val answerType = typeTag[Option[Formula]]
  val questionType = typeTag[FormulaQ]
  val default = None
}

class IntQ(val message:Elem) extends Question[Integer] {
  val answerType = typeTag[Integer]
  val questionType = typeTag[IntQ]
  val default = 0.asInstanceOf[Integer]
}

class StringQ(val message:Elem) extends Question[String] {
  val answerType = typeTag[String]
  val questionType = typeTag[StringQ]
  val default = ""
}

class ShowFormulaQ(val message:Elem, val formula : Formula) extends Question[BoxedUnit] {
  import scala.reflect.runtime.universe._
  //  super(typeTag[BoxedUnit])
  val answerType = typeTag[BoxedUnit]
  val questionType = typeTag[ShowFormulaQ]
  val default = BoxedUnit.UNIT
  override def toString = s"[MSG: $message, $formula]"
}


object MessageQ {
  object Type extends Enumeration {
    val ERROR = Value
  }
  type Type = Type.Value
  def Error(msg : Elem) = new MessageQ(Type.ERROR, msg)
}
class MessageQ(val typ:MessageQ.Type, val message:Elem) extends Question[BoxedUnit] {
  import scala.reflect.runtime.universe._
//  super(typeTag[BoxedUnit])
  val answerType = typeTag[BoxedUnit]
  val questionType = typeTag[MessageQ]
  val default = BoxedUnit.UNIT
  override def toString = "[MSG: "+message.text+"]"
}

final case class InteractionRunning[T](val id: String,
                                       val question : Question[_<:AnyRef],
                                       val answer : AnyRef => Interaction[T]) extends Interaction[T] {
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

sealed trait Interaction[+T] {
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
  def ask[T<:AnyRef](id : String, question : Question[T]) =
    new InteractionRunning[T](id,question, { a =>
      assert(a!=null)
//      assert(question.answerType.isInstance(a), "answer "+a+" should be of type "+question.answerType+" not "+a.getClass) // equivalent to "answer.isInstanceOf[T]"
      Interaction.returnval(a.asInstanceOf[T])
    })
  def error(id:String,err:Elem) : Interaction[Unit] =
    new InteractionRunning[Unit](id,MessageQ.Error(err),{a=>returnval(())})
  def failWith[T](id:String, err:Elem) : Interaction[T] =
    for { _ <- error(id, err); res <- fail[T] } yield res
  val skip : Interaction[Unit] = returnval(())
}
