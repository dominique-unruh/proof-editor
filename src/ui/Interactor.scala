package ui

import javafx.beans.property.{ObjectProperty, Property, SimpleObjectProperty}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.layout.VBox
import javafx.scene.web.HTMLEditor

import misc.Utils.JavaFXImplicits._
import trafo._
import ui.Interactor.Editor

import scala.collection.mutable
import scala.xml.Elem

//protected class QA[T](val interaction : Interaction[T], val answer : Option[_]) {
//  assert(answer.isEmpty || interaction.question.get.answerType.isInstance(answer.get))
////  var dirty = true
//
//  override def toString : String = {
//    val str = new StringBuilder("QA(")
//    if (interaction.question.isEmpty) str ++= "done"
//    else str ++= interaction.question.get.message.text
//    if (!answer.isEmpty) { str += ','; str ++= answer.get.toString }
//    if (!interaction.error.isEmpty) { str += ','; str ++= "error: "; str ++= interaction.error.get.message.text }
//    if (!interaction.result.isEmpty) { str += ','; str ++= "result: "; str ++= interaction.result.get.toString }
//    str += ')'
//    str.toString
//  }
//  def setAnswer(newAnswer : Any) = new QA(interaction, Some(newAnswer))
//}

class Interactor[T]() extends VBox {
  def this(interaction : Interaction[T]) = { this(); setInteraction(interaction) }
  private val interactions = new mutable.ArrayBuffer[Interaction[T]]
  private val answers = new mutable.HashMap[String, Object]()
  private var interaction : Interaction[T] = null
//  updateInteraction(0, interaction)

  private var editorFactory = Interactor.defaultEditorFactory(_)

  def setEditorFactory(factory : Class[_ <: Question[_<:AnyRef]] => Editor[_<:AnyRef]) : Unit =
    editorFactory = factory

  def clearInteraction() : Unit = {
    getChildren.clear()
    interactions.clear()
    answers.clear()
    interaction = null
  }

  def setInteraction(interaction : Interaction[T]) : Unit = {
    if (this.interaction!=null) clearInteraction()
    this.interaction = interaction
    updateInteraction(0, interaction)
  }

  private class Cell(idx:Int) extends VBox {
    private val label = new Label("<initialize me>")
    var edit : Interactor.Editor[_ <: AnyRef] = null
    var questionType : Class[_] = null

    def setHtml(html: Elem) : Unit = {
//      label.setHtmlText(<html contentEditable="false"><head></head><body contentEditable="false">{html}</body></html>.toString)
      label.setText(html.text)
    }

    def noQuestion() : Unit = {
      if (edit!=null) getChildren.remove(edit)
      edit = null
      questionType = null
    }

    def setQuestionType(q : Question[_<:AnyRef]): Unit = {
      val qt = q.getClass
      if (qt==questionType) return
      if (edit!=null) getChildren.remove(edit)
      edit = null
      questionType = null
      val edit0 : Editor[_ <: AnyRef] = editorFactory(qt)
      assert(q.answerType.isAssignableFrom(edit0.editedType),
             "question was for type "+q.answerType+" but editor factory returned editor for type "+edit0.editedType)
      edit = edit0
      questionType = qt
      edit.valueProperty.addListener {
        (answer:AnyRef) => setAnswer(idx,answer) }
      getChildren.add(edit)
    }
//    edit.valueProperty.addListener((answer:Int) => setAnswer(idx,answer))
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
      case InteractionFinished(_) | InteractionFailed() =>
        interactions.remove(idx, interactions.length - idx)
        getChildren.remove(idx, getChildren.size)
      case InteractionRunning(id,question,answer) =>
        val int2 = answer(answers.getOrElse(id,question.default))
//        println("set int", idx, id, int2, int2.question)
        updateInteraction(idx, int2)
    }
  }

  def setAnswer(idx: Int, answer: AnyRef) : Unit = interactions(idx) match {
    case InteractionRunning(id, question, _) =>
//      if (answer.isEmpty)
//        answers.remove(id)
//      else {
        assert(question.answerType.isInstance(answer),
          "q-type " + question.answerType + ", a-type " + answer.getClass)
        answers.update(id, answer)
        updateGUI(idx)
        recompute(idx + 1)
//      }
    case InteractionFinished(_) | InteractionFailed() =>
      throw new IndexOutOfBoundsException("setAnswer(" + idx + ",...)")
  }

  //  setItems(FXCollections.observableArrayList())
  //  private def setGUIStr(idx:Int, str:String) = {
  //    if (idx==getItems.size) getItems.add(str) else getItems.set(idx,str)
  //  }
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
        cell.setQuestionType(question)
        cell.setHtml(<span>{question.message.text} <i>{id}</i></span>)
        val answer = answers.getOrElse(id,question.default)
        cell.edit.asInstanceOf[Editor[Object]].setValue(answer)
    }
  }
}

object Interactor {
  trait Editor[T <: AnyRef] extends Node {
    val valueProperty : Property[T]
    val editedType : Class[T]
    def setValue(v:T) : Unit = {
      assert(editedType.isInstance(v),
        "v: "+v+", editedType: "+editedType)
      valueProperty.setValue(v)
    }
  }

  class StringEditor extends TextField with Editor[String] {
    override val valueProperty: Property[String] = textProperty()
    override val editedType: Class[String] = classOf[String]
//    textProperty.addListener((newVal:String) => valueProperty.setValue(Some(newVal)))
//    valueProperty.addListener({ (newVal:Option[String]) =>
//      println("change: ",newVal)
//      textProperty.setValue(newVal.getOrElse(""))})
  }

  // TODO there should be existing classes for this
  class IntEditor extends TextField with Editor[Integer] {
    override val valueProperty: Property[Integer] = new SimpleObjectProperty
    override val editedType: Class[Integer] = classOf[Integer]
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

  def defaultEditorFactory(qt : Class[_<:Question[_<:AnyRef]]) : Editor[_<:AnyRef] = {
      if (qt==classOf[StringQ]) new StringEditor
      else if (qt==classOf[IntQ]) new IntEditor
      else sys.error("unsupported question type "+qt)
  }
}