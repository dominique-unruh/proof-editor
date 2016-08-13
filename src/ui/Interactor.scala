package ui

import scala.reflect.runtime.universe._
import javafx.beans.property.{Property, SimpleObjectProperty}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.layout.VBox

import misc.{Log, Utils}
import misc.Utils.ImplicitConversions._
import trafo._
import ui.Interactor.{Editor, EditorFactory}

import scala.collection.mutable
import scala.reflect.api.TypeTags
import scala.runtime.BoxedUnit
import scala.xml.Elem
import scalafx.beans.Observable
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectWrapper}
import scalafx.{Includes, scene}
import scalafx.scene.layout

import scalafx.Includes._

/** @tparam R result type */
class Interactor[R]() extends layout.VBox {
  def this(interaction : Interaction[R]) = { this(); setInteraction(interaction) }
  private val interactions = new mutable.ArrayBuffer[Interaction[R]]
  private case class QAPair[A](question:Question[A], answer:A)
  private val previousAnswers = new mutable.HashMap[String, QAPair[_]]()
  private var interaction : Interaction[R] = _
  private val result_ = ReadOnlyObjectWrapper(None : Option[R])
  val result = result_.readOnlyProperty

  private var editorFactory : Interactor.EditorFactory = Interactor.defaultEditorFactory

  def setEditorFactory(factory : Interactor.EditorFactory) : Unit =
    editorFactory = factory

  object Typed { @inline def unapply[T](x:T) = Some(x) }
  def editors : Seq[Editor[_]] = for { Typed(c : Cell) <- children } yield c.edit

  def clearInteraction() : Unit = {
    children.clear()
    interactions.clear()
    previousAnswers.clear()
    interaction = null
    result_.set(None)
  }

  def setInteraction(interaction : Interaction[R]) : Unit = {
    if (this.interaction!=null) clearInteraction()
    this.interaction = interaction
    updateInteraction(0, interaction)
  }


  private class Cell(idx:Int) extends VBox {
    private val label = new Label("<initialize me>")
    var edit: Interactor.Editor[_] = _
    var question: Question[_] = _

    def setHtml(html: Elem) : Unit = {
      label.setText(html.text)
    }

    def noQuestion() : Unit = {
      if (edit!=null) getChildren.remove(edit)
      edit = null
      question = null
    }

    def setQuestion[U<:AnyRef](q : Question[U]): Unit = {
      if (question == null || question!=q) {
        if (edit != null) getChildren.remove(edit)
        edit = null
        question = null
        val edit0: Interactor.Editor[U] = editorFactory.create(q)
        question = q
        edit = edit0
        edit0.valueProperty.addListener(new ChangeListener[U] {
          override def changed(obs: ObservableValue[_ <: U], old: U, answer: U): Unit =
            setAnswer(idx, answer, fromEditor=true)
        })
        if (edit0.valueProperty.getValue!=null)
          setAnswer(idx, edit0.valueProperty.getValue, fromEditor=true)
        this.getChildren.add(edit0)
      } /*else {
        if (question!=q) {
          assert(edit.questionType==q.questionType)
          edit.asInstanceOf[Editor[U]].setQuestion(q)
        }
      }*/
    }
    getChildren.addAll(label)
  }

  def currentAnswer[A](id:String, question:Question[A]) : A = {
    previousAnswers.get(id) match {
      case None => question.default
      case Some(qa) =>
        if (qa.question==question) qa.answer.asInstanceOf[A]
        else question.default
    }
  }

  private final def updateInteraction(idx: Int, int: Interaction[R]): Unit = {
    if (idx == interactions.length) interactions += int else interactions.update(idx, int)
    updateGUI(idx)
    recompute(idx + 1)
  }

  private final def recompute(idx: Int): Unit = {
    assert(idx >= 1)
    val int = interactions(idx - 1)
    int match {
      case InteractionFinished(res) =>
        interactions.remove(idx, interactions.length - idx)
        children.remove(idx-1, children.size)
        result_.set(Some(res))
      case InteractionFailed() =>
        interactions.remove(idx, interactions.length - idx)
        children.remove(idx-1, children.size)
        result_.set(None)
      case InteractionRunning(id,question,answer) =>
        val a = currentAnswer(id,question) // answers.getOrElse(id,question.default)
        val int2 =
          try { answer(a) }
          catch { case e:Throwable =>
            Log.stackTrace("uncaught exception in Interaction",e)
            Interaction.failWith("internal-error-"+System.identityHashCode(this),
              <span>internal error: {e.toString}</span>)
          }
        updateInteraction(idx, int2)
    }
  }

  /** Provide the answer to a question in the interaction.
    *
    * Type safety: answer must be of the correct type for the question
    *
    * @param idx Index of the question in the current interaction
    * @param fromEditor true if setAnswer is called due to editing via the associated Editor.
    *                   This makes sure the editor is not updated to show the new answer.
    */
  def setAnswer(idx: Int, answer: AnyRef, fromEditor:Boolean=false) : Unit = interactions(idx) match {
    case InteractionRunning(id, question, _) =>
      previousAnswers.update(id, QAPair[answer.type](question.asInstanceOf[Question[answer.type]],answer))
      if (!fromEditor) updateGUI(idx)
      recompute(idx + 1)
    case InteractionFinished(_) | InteractionFailed() =>
      throw new IndexOutOfBoundsException("setAnswer(" + idx + ",...)")
  }

  private def updateGUI(idx: Int) = {
    if (idx == children.size) children.add(new Cell(idx))
    val int = interactions(idx)
    val cell = children.get(idx).asInstanceOf[Cell] // If Cell would inherit from scalafx...VBox, not javafx..VBox, this cast would fail because the delegate of the Cell is added
    int match {
      case InteractionFinished(res) =>
        cell.noQuestion()
        cell.setHtml(<span><b>Result: </b>{res}</span>)
      case InteractionFailed() =>
        cell.noQuestion()
        cell.setHtml(<b>Failed</b>)
      case InteractionRunning(id,question,answer) =>
        cell.setQuestion(question)
        cell.setHtml(<span>{question.message.text} <i>#{id}</i></span>)
        val answer = currentAnswer(id,question) // answers.getOrElse(id,question.default)
        // TODO: add typetag check
        cell.edit.asInstanceOf[Editor[answer.type]].setValue(answer)
    }
  }
}

object Interactor {
  trait Editor[T<:AnyRef] extends Node {
    val valueProperty : Property[T]
    val editedType : TypeTag[T]
    val questionType : TypeTag[_ <: Question[T]]
//    def setQuestion(question : Question[T]) : Unit = {}
    def setValue(v:T) : Unit = {
//      assert(editedType.isInstance(v),
//        "v: "+v+", editedType: "+editedType)
      valueProperty.setValue(v)
    }
  }

  trait EditorFactory {
    def create[T<:AnyRef](q : Question[T]) : Editor[T]
    def cast[T<:AnyRef,U<:AnyRef](a: TypeTag[T], b: TypeTag[U], x : Editor[T]) : Editor[U] =
      if (a!=null && b!=null && a==b) x.asInstanceOf[Editor[U]] else throw new ClassCastException(s"$a != $b in type cast")
    def cast[T<:AnyRef,U<:AnyRef](b: TypeTag[U], x : Editor[T]) : Editor[U] =
      cast(x.editedType, b, x)
  }

  class StringEditor extends TextField with Editor[String] {
    override val valueProperty: Property[String] = textProperty()
    override val editedType = typeTag[String]
    override val questionType = typeTag[StringQ]
  }

  class MessageViewer extends Label with Editor[BoxedUnit] {
    override val valueProperty: Property[BoxedUnit] = new SimpleObjectProperty[BoxedUnit](BoxedUnit.UNIT)
    override val editedType = typeTag[BoxedUnit]
    override val questionType = typeTag[MessageQ]
  }

  // TODO there should be existing classes for this
  class IntEditor extends TextField with Editor[Integer] {
    override val valueProperty: Property[Integer] = new SimpleObjectProperty
    override val editedType: TypeTag[Integer] = typeTag[Integer]
    override val questionType = typeTag[IntQ]

    textProperty.addListener((newVal:String) =>
      try {
        val i = newVal.toInt
        valueProperty.setValue(i)
      } catch {
        case _:NumberFormatException => /*valueProperty.setValue(None)*/ ()
      })
    valueProperty.addListener({ (newVal:Integer) =>
      Log.debug("change: ",newVal)
      textProperty.setValue(newVal.toString)})
  }

  val defaultEditorFactory = new EditorFactory {
    override def create[T<:AnyRef](q : Question[T]) : Editor[T] = {
      val qt = q.questionType; val at = q.answerType
      if (qt == typeTag[StringQ]) cast(at, new StringEditor)
      else if (qt == typeTag[IntQ]) cast(at, new IntEditor)
      else if (qt == typeTag[MessageQ]) cast(at, new MessageViewer)
      else sys.error("unsupported question type " + qt)
    }
  }
}