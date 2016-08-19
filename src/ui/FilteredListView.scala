package ui

import javafx.collections.transformation.FilteredList
import javafx.scene.control.{ListView, TextField}

import misc.Log
import misc.Utils.ImplicitConversions._

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.{Priority, VBox}

class FilteredListView[T](val items: ObservableBuffer[T]) extends VBox {
  private val filterField = new TextField()
  private val listView = new ListView[T]()
  private val filteredList = new FilteredList[T](items)
  listView.setItems(filteredList)
  vgrow = Priority.Always
  listView.vgrow = Priority.Always
  def selectionModel = listView.selectionModel

  def promptText = filterField.promptText


  filterField.focused.onChange { (_,_,focused) =>
  if (focused) filterField.selectAll() }

  filterField.text.onChange { (_, _, filterStr) =>
    val lcFilterStr = filterStr.toLowerCase
    filteredList.setPredicate { item:T =>
      item.toString.toLowerCase.contains(lcFilterStr) }
    if (selectionModel.value.getSelectedItem==null)
      selectionModel.value.selectFirst()
  }

  children += filterField
  children += listView

  filterField.onKeyPressed = { e:KeyEvent =>
    import scalafx.scene.input.KeyCode._
    Log.debug("key press", e)
    e.code match {
      case Down =>
        listView.requestFocus()
        e.consume()
      case Escape =>
        filterField.clear()
        e.consume()
      case _ =>
    }
  }

  def focusFilter() = filterField.requestFocus()
}
