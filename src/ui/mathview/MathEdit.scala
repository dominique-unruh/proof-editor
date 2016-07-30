package ui.mathview

import cmathml.{CNone, MCNone, MutableCMathML}
import ui.mathview.MathViewFX.{CursorLeft, CursorPos, CursorRight}

import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.control.{ContextMenu, MenuItem, Separator, SeparatorMenuItem}
import scalafx.scene.input.{ContextMenuEvent, KeyCode, KeyEvent}

class MathEdit extends MathViewFX {
  focusTraversable = true

  private var selectingFrom : Option[CursorPos] = None

  private def menuItem(text:String, action: => Unit) = {
    val item = new MenuItem(text)
    item.onAction = {_:ActionEvent => action}
    item
  }
  private def menuItem(text:String) = {
    val item = new MenuItem(text)
    item.disable = true
    item
  }

  private def createContextMenu = {
    val cm = new ContextMenu
//    if (!mathDoc.isEmpty)
//      cm.items += menuItem("_Clear", setMath(CNone()))
    if (selection.value.isDefined)
      cm.items += menuItem("_Clear", {
        val none = new MCNone
        selection.value.get.replaceWith(none)
        cursorPos.value = CursorPos(none,CursorLeft)
      })
    if (cm.items.isEmpty)
      cm.items += menuItem("No actions available")
    cm
  }

//  val contextMenu = new ContextMenu {
//    items += MenuItem.sfxMenuItem2jfx(new MenuItem("Hello") { onAction = { e:ActionEvent => println("Hello",e) }})
//  }

  onContextMenuRequested = { e:ContextMenuEvent =>
    println("Context menu requested: "+e)
    createContextMenu.show(this,e.screenX,e.screenY)
  }

  private def clearSelection(): Unit = {
    selectingFrom = None
    selection.value = None
  }

  private def atSideOf(side: KeyCode, cursor: CursorPos, jump: Boolean) = side match {
    case KeyCode.Right => rightOf(cursor,jump)
    case KeyCode.Left => leftOf(cursor,jump)
    case _ => throw new IllegalArgumentException(side.toString)
  }

  onKeyPressed = { e:KeyEvent =>
    import scalafx.scene.input.KeyCode._
    e.code match {
      case Right|Left =>
        val newPos = atSideOf(e.code, cursorPos.value, jump=e.controlDown)
        if (newPos.isDefined) {
          if (e.shiftDown) {
            if (selectingFrom.isEmpty)
              selectingFrom = Some(cursorPos.value)
            val selected = encompassingNode(newPos.get.node,selectingFrom.get.node)
            println(s"Selection: From ${selectingFrom.get.node} to ${newPos.get.node}: ${selected}")
            selection.value = Some(selected)
          } else
            clearSelection()
          cursorPos.value = newPos.get
        }
      case _ => println("Key pressed: "+e)
    }
  }
}
