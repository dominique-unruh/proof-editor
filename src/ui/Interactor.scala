package ui

import javafx.beans.property.{ObjectProperty, Property, SimpleObjectProperty}
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.layout.VBox

import misc.Utils.JavaFXImplicits._
import trafo.{IntQ, Interaction, Question, StringQ}

import scala.collection.mutable

protected class QA[T](val interaction : Interaction[T], val answer : Option[_]) {
  assert(answer.isEmpty || interaction.question.get.answerType.isInstance(answer.get))
//  var dirty = true

  override def toString : String = {
    val str = new StringBuilder("QA(")
    if (interaction.question.isEmpty) str ++= "done"
    else str ++= interaction.question.get.message.text
    if (!answer.isEmpty) { str += ','; str ++= answer.get.toString }
    if (!interaction.error.isEmpty) { str += ','; str ++= "error: "; str ++= interaction.error.get.message.text }
    if (!interaction.result.isEmpty) { str += ','; str ++= "result: "; str ++= interaction.result.get.toString }
    str += ')'
    str.toString
  }
  def setAnswer(newAnswer : Any) = new QA(interaction, Some(newAnswer))
}


class Interactor[T](val interaction : Interaction[T]) extends VBox {
  private val interactions = new mutable.ArrayBuffer[Interaction[T]]
  private val answers = new mutable.HashMap[String, Object]()
  setInteraction(0, interaction)

  //  set(0,new QA(interaction,None))
  //  fillFrom(1)

  trait Editor[T <: Object] extends Node {
    val valueProperty : Property[Option[T]]
    val editedType : Class[T]
    def setValue(v:Option[T]) : Unit = {
      assert(v.isEmpty || editedType.isInstance(v.get),
             "v: "+v+", editedType: "+editedType)
      valueProperty.setValue(v)
    }
  }

  class StringEditor extends TextField with Editor[String] {
    override val valueProperty: Property[Option[String]] = new SimpleObjectProperty
    override val editedType: Class[String] = classOf[String]
    textProperty.addListener((newVal:String) => valueProperty.setValue(Some(newVal)))
    valueProperty.addListener({ (newVal:Option[String]) =>
      println("change: ",newVal)
      textProperty.setValue(newVal.getOrElse(""))})
  }

  class IntEditor extends TextField with Editor[Integer] {
    override val valueProperty: Property[Option[Integer]] = new SimpleObjectProperty
    override val editedType: Class[Integer] = classOf[Integer]
    textProperty.addListener((newVal:String) =>
      try {
        val i = newVal.toInt
        valueProperty.setValue(Some(i))
      } catch {
        case _:NumberFormatException => valueProperty.setValue(None)
      })
    valueProperty.addListener({ (newVal:Option[Integer]) =>
      println("change: ",newVal)
      textProperty.setValue(newVal.getOrElse(0).toString)})
  }

  private class Cell(idx:Int) extends VBox {
    val label = new Label("<initialize me!>")
    var edit : Editor[_ <: Object] = null
    var questionType : Class[_] = null

    def noQuestion() : Unit = {
      if (edit!=null) getChildren.remove(edit)
      edit = null
      questionType = null
    }

    def setQuestionType(q : Question[_]): Unit = {
      val qt = q.getClass
      if (qt==questionType) return
      if (edit!=null) getChildren.remove(edit)
      edit = null
      questionType = null
      val edit0 : Editor[_<:Object] = {
        if (qt==classOf[StringQ]) new StringEditor
        else if (qt==classOf[IntQ]) new IntEditor
        else sys.error("unsupported question type "+qt)
      }
      assert(q.answerType.isAssignableFrom(edit0.editedType))
      edit = edit0
      questionType = qt
      edit.valueProperty.addListener {
        (answer:Option[_ <: Object]) => setAnswer(idx,answer) }
      getChildren.add(edit)
    }
//    edit.valueProperty.addListener((answer:Int) => setAnswer(idx,answer))
    getChildren.addAll(label)
  }

  private def setInteraction(idx: Int, int: Interaction[T]): Unit = {
    if (idx == interactions.length) interactions += int else interactions.update(idx, int)
    updateGUI(idx)
    recompute(idx + 1)
  }

  private def recompute(idx: Int): Unit = {
    assert(idx >= 1)
    //    val qa = qaList(idx-1)
    val int = interactions(idx - 1)
    val question = int.question
    if (question.isEmpty) {
      interactions.remove(idx, interactions.length - idx)
      //      for (i <- idx until interactions.length) interactions.pop()
      getChildren.remove(idx, getChildren.size)
      //      getItems.remove(idx,getItems.size)
    } else {
      val int2 = int.answer(answers.get(int.id))
      println("set int", idx, int.id, int2.id, int2.question)
      setInteraction(idx, int2)
    }
  }

  def setAnswer(idx: Int, answer: Option[Object]) : Unit = {
    val int = interactions(idx)
    if (answer.isEmpty)
      answers.remove(int.id)
    else {
      assert(!int.question.isEmpty)
      assert(int.question.get.answerType.isInstance(answer.get),
        "q-type " + int.question.get.answerType + ", a-type " + answer.get.getClass)
      answers.update(int.id, answer.get)
      updateGUI(idx)
      recompute(idx + 1)
    }
  }

  //  setItems(FXCollections.observableArrayList())
  //  private def setGUIStr(idx:Int, str:String) = {
  //    if (idx==getItems.size) getItems.add(str) else getItems.set(idx,str)
  //  }
  private def updateGUI(idx: Int) = {
    if (idx == getChildren.size) getChildren.add(new Cell(idx))
    val int = interactions(idx)
    val cell = getChildren().get(idx).asInstanceOf[Cell]
    if (int.question.isEmpty) {
      cell.noQuestion()
      cell.label.setText(int.id + " " + "Result: " + int.result)
    } else {
      cell.setQuestionType(int.question.get)
      cell.label.setText(int.id + " " + int.question.get.message.text)
      val answer = answers.get(int.id)
      cell.edit.asInstanceOf[Editor[Object]].setValue(answer)
    }
  }
}
