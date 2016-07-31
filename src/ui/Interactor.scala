package ui

import scala.reflect.runtime.universe._
import javafx.beans.property.{Property, SimpleObjectProperty}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.layout.VBox

import misc.Utils
import misc.Utils.ImplicitConversions._
import trafo._
import ui.Interactor.Editor

import scala.collection.mutable
import scala.reflect.api.TypeTags
import scala.runtime.BoxedUnit
import scala.xml.Elem
import scalafx.beans.Observable
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectWrapper}




class Interactor[T]() extends VBox {
  def this(interaction : Interaction[T]) = { this(); setInteraction(interaction) }
  private val interactions = new mutable.ArrayBuffer[Interaction[T]]
  private val answers = new mutable.HashMap[String, AnyRef]()
  private var interaction : Interaction[T] = null
  private val result_ = ReadOnlyObjectWrapper(None : Option[T])
  val result = result_.readOnlyProperty

  private var editorFactory : Interactor.EditorFactory = Interactor.defaultEditorFactory

  def setEditorFactory(factory : Interactor.EditorFactory) : Unit =
    editorFactory = factory

  def clearInteraction() : Unit = {
    getChildren.clear()
    interactions.clear()
    answers.clear()
    interaction = null
    result_.set(None)
  }

  def setInteraction(interaction : Interaction[T]) : Unit = {
    if (this.interaction!=null) clearInteraction()
    this.interaction = interaction
    updateInteraction(0, interaction)
  }


  private class Cell(idx:Int) extends VBox {
    private val label = new Label("<initialize me>")
    var edit: Interactor.Editor[_] = null
    var question: Question[_] = null

    def setHtml(html: Elem) : Unit = {
      label.setText(html.text)
    }

    def noQuestion() : Unit = {
      if (edit!=null) getChildren.remove(edit)
      edit = null
      question = null
    }

    def setQuestion[T<:AnyRef](q : Question[T]): Unit = {
      if (question == null || q.questionType!=question.questionType) {
        if (edit != null) getChildren.remove(edit)
        edit = null
        question = null
        val edit0: Interactor.Editor[T] = editorFactory.create(q)
        edit0.setQuestion(q)
        question = q
        edit = edit0
        edit0.valueProperty.addListener(new ChangeListener[T] {
          override def changed(obs: ObservableValue[_ <: T], old: T, answer: T): Unit =
            setAnswer(idx, answer)
        })
        if (edit0.valueProperty.getValue!=null)
          setAnswer(idx, edit0.valueProperty.getValue)
        getChildren.add(edit0)
      } else {
        if (question!=q) {
          assert(edit.questionType==q.questionType)
          edit.asInstanceOf[Editor[T]].setQuestion(q)
        }
      }
    }
    getChildren.addAll(label)
  }

  private final def updateInteraction(idx: Int, int: Interaction[T]): Unit = {
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
        getChildren.remove(idx, getChildren.size)
        result_.set(Some(res))
      case InteractionFailed() =>
        interactions.remove(idx, interactions.length - idx)
        getChildren.remove(idx, getChildren.size)
        result_.set(None)
      case InteractionRunning(id,question,answer) =>
        val int2 = answer(answers.getOrElse(id,question.default))
        updateInteraction(idx, int2)
    }
  }

  def setAnswer(idx: Int, answer: AnyRef) : Unit = interactions(idx) match {
    case InteractionRunning(id, question, _) =>
        if (answers.get(id)!=answer) {
          answers.update(id, answer)
          updateGUI(idx)
          recompute(idx + 1)
        }
    case InteractionFinished(_) | InteractionFailed() =>
      throw new IndexOutOfBoundsException("setAnswer(" + idx + ",...)")
  }

  private def updateGUI(idx: Int) = {
    if (idx == getChildren.size) getChildren.add(new Cell(idx))
    val int = interactions(idx)
    val cell = getChildren().get(idx).asInstanceOf[Cell]
    int match {
      case InteractionFinished(result) =>
        cell.noQuestion()
        cell.setHtml(<span><b>Result: </b>{result}</span>)
      case InteractionFailed() =>
        cell.noQuestion()
        cell.setHtml(<b>Failed</b>)
      case InteractionRunning(id,question,answer) =>
        cell.setQuestion(question)
        cell.setHtml(<span>{question.message.text} <i>#{id}</i></span>)
        val answer : AnyRef = answers.getOrElse(id,question.default)
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
    def setQuestion(question : Question[T]) : Unit = {}
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
//    textProperty.addListener((newVal:String) => valueProperty.setValue(Some(newVal)))
//    valueProperty.addListener({ (newVal:Option[String]) =>
//      println("change: ",newVal)
//      textProperty.setValue(newVal.getOrElse(""))})
  }

  class MessageViewer extends Label with Editor[BoxedUnit] {
    override val valueProperty: Property[BoxedUnit] = new SimpleObjectProperty[BoxedUnit](BoxedUnit.UNIT)
    override val editedType = typeTag[BoxedUnit]
    override val questionType = typeTag[MessageQ]
//    override val setQuestion(question : Question[BoxedUnit]) = setText(question.message.text)
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
      println("change: ",newVal)
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