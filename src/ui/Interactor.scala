package ui

import javafx.application.Platform
import javafx.collections.ListChangeListener.Change
import javafx.collections.{FXCollections, ListChangeListener}
import javafx.scene.control.ListView.EditEvent
import javafx.scene.control._
import javafx.scene.control.cell.ComboBoxListCell
import javafx.scene.layout.{AnchorPane, VBox}
import javafx.util.Callback

import trafo.{Interaction, Question}
import misc.Utils.JavaFXImplicits._

import scala.collection.mutable

protected class QA[T](val interaction : Interaction[T], val answer : Option[_]) {
  assert(answer.isEmpty || interaction.question.get.answerType.isInstance(answer.get))
  var dirty = true

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

class Interactor[T](val interaction : Interaction[T]) extends ListView[QA[T]] {
  val qaList = FXCollections.observableArrayList[QA[T]]()
  qaList.addListener(new ListChangeListener[QA[T]] {
    override def onChanged(c: Change[_ <: QA[T]]): Unit = {
      val changes = mutable.Queue[Int]()
      while (c.next) changes ++= c.getFrom until c.getTo
      Platform.runLater { () => for (i <- changes) maybeDirty(i) }
  }})
  set(0,new QA(interaction,None))
  setItems(qaList)
  ComboBoxListCell.forListView()

  setCellFactory(new Callback[ListView[QA[T]], ListCell[QA[T]]] {
    override def call(param: ListView[QA[T]]): ListCell[QA[T]] = {
      new ListCell[QA[T]]() {
        lazy val label = new Label()
        lazy val edit = { val e = new TextField(); e.textProperty.addListener((x:String) => edited(x)); e}
        lazy val vbox = new VBox(label,edit)
        lazy val done = new Label()

        def edited(x:String) = {
          println("edit: "+x+" "+getIndex+" "+qaList.get(getIndex).answer)
//          startEdit()
          set(getIndex,getItem.setAnswer(x))
        }

        override protected def updateItem(qa:QA[T], empty:Boolean) = {
          super.updateItem(qa, empty)
          setText(null)
          if (empty) {
            setGraphic(null)
          } else {
            val question = qa.interaction.question
            if (question.isEmpty) {
              println("UPDATE "+getIndex+" done")
              done.setText(qa.interaction.result.get.toString)
              setGraphic(done)
            } else {
              println("UPDATE "+getIndex+" gfx")
              label.setText(question.get.message.text)
              edit.setText(qa.answer.getOrElse("nothing?").toString)
              setGraphic(vbox)
            }
          }
        }
      }
    }
  })

  def answer(idx: Int, answer: Any) : Unit = {
    val qa = qaList.get(idx)
    val qa2 = qa.setAnswer(answer)
    set(idx,qa2)
//    fillFrom(idx+1)
  }

  private def set(idx:Int, qa:QA[T]) : Unit = {
    if (idx==qaList.size) qaList.add(qa)
    else qaList.set(idx,qa)
  }

  private def maybeDirty(idx:Int) : Unit = {
    if (idx >= qaList.size) return
    val qa = qaList.get(idx)
//    println("maybeDirty",idx,qa,qa.dirty)
    if (qa.dirty) {
      qa.dirty = false
      val int = qa.interaction
      val answer = qa.answer
      val question = int.question
      if (!question.isEmpty) {
        println("INTERACTING "+(idx+1))
        val int2 = int.answer(answer.getOrElse(question.get.default.get))
        val qa2 = new QA(int2, None)
        set(idx+1, qa2)
      } else {
        qaList.remove(idx+1,qaList.size)
      }
    }
  }

//  private def fillFrom(idx:Int) : Unit = {
//    assert(idx>=1)
//    val qa = qaList.get(idx-1)
//    val int = qa.interaction
//    val answer = qa.answer
//    val question = int.question
//    if (!question.isEmpty) {
//      val int2 = int.answer(answer.getOrElse(question.get.default.get))
//      val qa2 = new QA(int2, None)
//      set(idx, qa2)
//      fillFrom(idx + 1)
//    }
//  }
}
