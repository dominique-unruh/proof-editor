package trafo
import scala.language.existentials
import cmathml.{CMathML, CNone, Path}
import com.thoughtworks.each.Monadic
import misc.Pure
import theory.Formula

import scala.language.higherKinds
import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.collection.generic.FilterMonadic
import scala.reflect.api.TypeTags
import scala.util.control.Breaks
import scala.runtime.BoxedUnit
import scala.xml.Elem
import scala.reflect.runtime.universe._
import scalaz.Monad


abstract class Question[T] {
  // T <: Object because we want T to be a reference type (else we run into the problem from http://stackoverflow.com/questions/38285616/in-scala-10-getclass-isinstance10-is-false
  val answerType : TypeTag[T]
  val questionType : TypeTag[_ <: Question[T]]
  def message : scala.xml.Elem
  val default : T
}

class FormulaQ(val message:Elem) extends Question[Option[Formula]] {
  val answerType = typeTag[Option[Formula]]
  val questionType = typeTag[FormulaQ]
  val default = None
}

class FormulaSubtermQ(val message:Elem) extends Question[Option[(Formula,Path)]] {
  val answerType = typeTag[Option[(Formula,Path)]]
  val questionType = typeTag[FormulaSubtermQ]
  val default = None
}

class MathQ(val message:Elem, val default : CMathML = CNone()) extends Question[CMathML] {
  val answerType = typeTag[CMathML]
  val questionType = typeTag[MathQ]
//  val default = CNone
}

class IntQ(val message:Elem) extends Question[Int] {
  val answerType = typeTag[Int]
  val questionType = typeTag[IntQ]
  val default = 0.asInstanceOf[Int]
}

class StringQ(val message:Elem) extends Question[String] {
  val answerType = typeTag[String]
  val questionType = typeTag[StringQ]
  val default = ""
}

class ShowFormulaQ(val message:Elem, val formula : Formula, val highlight : Option[Path] = None)
  extends Question[BoxedUnit] {
  import scala.reflect.runtime.universe._
  val answerType = typeTag[BoxedUnit]
  val questionType = typeTag[ShowFormulaQ]
  val default = BoxedUnit.UNIT
  override def toString =
    s"[MSG: $message, $formula${highlight.map(" "+_.toString).getOrElse("")}]"
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

final case class InteractionRunning[T,A](id: String,
                                       question : Question[A],
                                       answer : A => Interaction[T]) extends Interaction[T] {
  override def resultMaybe: Option[T] = None
  override def isDone: Boolean = false
  override def isRunning: Boolean = true
  override def isFailed: Boolean = false
  override def withFilter(p: (T) => Boolean): Interaction[T] = InteractionRunning[T,A](id, question, a => answer(a).withFilter(p))
}
final case class InteractionFinished[T](result: T) extends Interaction[T] {
  override def resultMaybe: Option[T] = Some(result)
  override def isDone: Boolean = true
  override def isRunning: Boolean = false
  override def isFailed: Boolean = false
  override def withFilter(p: (T) => Boolean): Interaction[T] =
    if (p(result)) this else InteractionFailed()
}
final case class InteractionFailed() extends Interaction[Nothing] {
  override def resultMaybe: Option[Nothing] = None
  override def isDone: Boolean = false
  override def isRunning: Boolean = false
  override def isFailed: Boolean = true
  override def withFilter(p: Nothing => Boolean) = this
}

sealed trait Interaction[+T] {
  def withFilter(p : T => Boolean) : Interaction[T]
  final def flatMap[U](f: T => Interaction[U]) : Interaction[U] = this match { // TODO: move to subclasses like withFilter
    case /*InteractionRunning(id, question, answer)*/int : InteractionRunning[T,a] => InteractionRunning[U,a](int.id, int.question, a => int.answer(a).flatMap(f))
    case InteractionFinished(result) => f(result)
    case InteractionFailed() => InteractionFailed()
  }
  final def map[U](f: T => U) : Interaction[U] = this match { // TODO: move to subclasses
    case int : InteractionRunning[T,a] => InteractionRunning[U,a](int.id, int.question, a => int.answer(a).map(f))
    case InteractionFinished(result) => InteractionFinished(f(result))
    case InteractionFailed() => InteractionFailed()
  }
  @Pure final def quickInteract(answers : Any*) : (T,List[Question[BoxedUnit]]) = {
    val (inter,msgs) = quickInteract0(answers : _*)
    inter match {
      case InteractionFinished(result) => (result,msgs)
      case InteractionFailed() => throw new RuntimeException("interaction failed "+msgs.mkString(","))
      case InteractionRunning(_,question,_) => throw new RuntimeException("interaction incomplete, expecting "+question.message.text)
    }
  }

  /** Runs the interaction [[this]] by providing the answers [[answers]]. Leftover answers are silently ignored.
    * @param answers The answers to be provided. Questions with answers of type [[BoxedUnit]] do not have to be provided.
    * @return (rest,messages): rest=remaining interaction, messages=questions with BoxedUnit-answers that where encountered
    */
  @Pure final def quickInteract0(answers : Any*) : (Interaction[T],List[Question[BoxedUnit]]) = {
    import scala.util.control.Breaks._
    var inter = this
    var messages = Nil: List[Question[BoxedUnit]]
    val answersIt = answers.iterator
    breakable {
      while (true)
        inter match {
//          case InteractionRunning(id, msg: MessageQ, answer) =>
//            inter = answer(BoxedUnit.UNIT)
          case InteractionRunning(id, question, answer) if question.answerType==typeTag[BoxedUnit] =>
            messages = question.asInstanceOf[Question[BoxedUnit]] :: messages
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
  def fail[T] : Interaction[T] = InteractionFailed()
  def failU : Interaction[Unit] = fail
  def returnval[T](res : T) = new InteractionFinished[T](res)
  def ask[T](id : String, question : Question[T]) =
    new InteractionRunning[T,T](id,question, { a =>
      assert(a!=null)
//      assert(question.answerType.isInstance(a), "answer "+a+" should be of type "+question.answerType+" not "+a.getClass) // equivalent to "answer.isInstanceOf[T]"
      Interaction.returnval(a.asInstanceOf[T])
    })
  def error(id:String,err:Elem) : Interaction[Unit] =
    new InteractionRunning[Unit,BoxedUnit](id,MessageQ.Error(err),{a=>returnval(())})
  def failWith[T](id:String, err:Elem) : Interaction[T] =
    for { _ <- error(id, err); res <- fail[T] } yield res
  def failWithU(id:String, err:Elem) : Interaction[Unit] = failWith(id,err)
  val skip : Interaction[Unit] = returnval(())

  implicit object interactionInstance extends Monad[Interaction] {
    override def bind[A, B](fa: Interaction[A])(f: (A) => Interaction[B]): Interaction[B] = fa.flatMap(f)
    override def point[A](a: => A): Interaction[A] = returnval(a)
  }

  val interaction = Monadic.monadic[Interaction]
  implicit def toEachOps[F[_], A](v: F[A]) : Monadic.EachOps[F,A] = Monadic.toEachOps(v)
  class OptionI[A](v : Interaction[Option[A]]) {
    def getOrElseI(default: Interaction[A]): Interaction[A] =
      v.flatMap {
        case Some(x) => returnval(x)
        case None => default
      }
  }
  implicit def toOptionI1[A](s:Option[A]) : OptionI[A] = new OptionI(returnval(s))
  implicit def toOptionI2[A](s:Interaction[Option[A]]) : OptionI[A] = new OptionI(s)
}

