package ui.mathview

import javafx.geometry.Bounds

import cmathml.CMathML.plus
import cmathml._
import misc.Utils.ImplicitConversions._
import ui.mathview.MathViewFX.{CursorLeft, CursorPos, CursorRight, CursorSide}

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.event.ActionEvent
import scalafx.scene.control.{ContextMenu, MenuItem}
import scalafx.scene.input.{ContextMenuEvent, KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.Region

class MathEdit extends MathViewFX {
  focusTraversable = true

  val editable = ObjectProperty(None : Option[MutableCMathML])

  override def setMath(m : CMathML): Unit = {
    super.setMath(m)
    cursorPos.value = CursorPos(mathDoc.root, CursorRight)
  }

  private var selectingFrom : Option[CursorPos] = None
  val cursorPos = ObjectProperty[CursorPos](CursorPos(mathDoc.root, CursorLeft))
  cursorPos.onChange { (_, oldPos, newPos) =>
    removeCursor(oldPos)
    if (focused.value)
      showCursor(newPos)
  }
  focused.onChange { (_, _, focus) =>
    if (focus)
      showCursor(cursorPos.value)
    else
      removeCursor(cursorPos.value)
  }

  private def removeCursor(pos:CursorPos) =
    getHighlights(pos.node).removeIf((_: MathHighlight).isInstanceOf[MathCursor])
  private def showCursor(pos:CursorPos) =
    getHighlights(pos.node) += new MathCursor(pos.side)

  val selection = ObjectProperty[Option[MutableCMathML]](None)
  selection.onChange { (_, oldSel, newSel) =>
    assert(oldSel!=newSel)
    if (oldSel != newSel) {
      if (oldSel.isDefined) {
        val oldHighlights = getHighlights(oldSel.get)
        oldHighlights.removeIf((_: MathHighlight).isInstanceOf[MathSelection])
      }
      if (newSel.isDefined) {
        val newHighlights = getHighlights(newSel.get)
        newHighlights += new MathSelection()
      }
    }
  }

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
    if (selection.value.exists(inEditableRange(_)))
      cm.items += menuItem("_Clear", {
        val none = new MCNone
        selection.value.get.replaceWith(none)
        cursorPos.value = CursorPos(none,CursorLeft)
      })
    if (cm.items.isEmpty)
      cm.items += menuItem("No actions available")
    cm
  }

  onContextMenuRequested = { e:ContextMenuEvent =>
    println("Context menu requested: "+e)
    createContextMenu.show(this,e.screenX,e.screenY)
  }

  private def clearSelection(): Unit = {
    selectingFrom = None
    selection.value = None
  }

  private def inEditableRange(math: MutableCMathML) = editable.value match {
    case None => false
    case Some(m) => math.isDescendantOf(m)
  }

  private def atSideOf(side: KeyCode, cursor: CursorPos, jump: Boolean) = side match {
    case KeyCode.Right => rightOf(cursor,jump)
    case KeyCode.Left => leftOf(cursor,jump)
    case _ => throw new IllegalArgumentException(side.toString)
  }

  onMouseClicked = { e:MouseEvent => requestFocus() }

  onKeyPressed = handleKeyPress(_:KeyEvent)
  onKeyTyped = handleKeyTyped(_:KeyEvent)

  def selectAll() = {
    if (inEditableRange(cursorPos.value.node) && (selection.value != editable.value))
      selection.value = editable.value
    else if (selection.value.contains(mathDoc.root) && editable.value.isDefined)
      selection.value = editable.value
    else
      selection.value = Some(mathDoc.root)
    cursorPos.value = CursorPos(selection.value.get, CursorRight)
  }

  private def handleKeyTyped(e:KeyEvent) = {
    println("Key typed: "+e)
    var processed = true
    e.character match {
      case "+" => insertBinaryOp(plus)
      case "-" => insertBinaryOp(CMathML.minus)
      case "*" => insertBinaryOp(CMathML.times)
      case "/" => insertBinaryOp(CMathML.divide)
      case "=" => insertBinaryOp(CMathML.equal)
      case _ => processed = false
    }
    if (processed) e.consume()
  }

  def replaceWith(a: MutableCMathML, b: MApply) = {
    a.replaceWith(b)
    if (editable.value.contains(a))
      editable.value = Some(b)
    if (selection.value.contains(a))
      selection.value = Some(b)
    if (cursorPos.value.node == a)
      cursorPos.value = cursorPos.value.copy(node=b)
  }


  private def insertBinaryOp(op:CMathML) : Unit = {
    println("insertBinaryOp",op)

    val (target,toLeft) = selection.value match {
      case None => (cursorPos.value.node, cursorPos.value.side==CursorRight)
      case Some(sel) => (sel,true) }

    if (!inEditableRange(target)) return

    val left = new MCNone()
    val right = new MCNone()
    val binop = new MApply(MutableCMathML.fromCMathML(op), left, right)

    replaceWith(target,binop)

    if (toLeft)
      left.replaceWith(target)
    else
      right.replaceWith(target)

    clearSelection()

    val cursorNode = if (toLeft) right else left
    cursorPos.value = CursorPos(cursorNode,CursorLeft)
  }

  private def handleKeyPress(e:KeyEvent) = {
    import scalafx.scene.input.KeyCode._
    var processed = true
    e.code match {
      case Right|Left =>
        val newPos = atSideOf(e.code, cursorPos.value, jump=e.controlDown)
        if (newPos.isDefined) {
          if (e.shiftDown) {
            if (selectingFrom.isEmpty)
              selectingFrom = Some(cursorPos.value)
            val selected = encompassingNode(newPos.get.node,selectingFrom.get.node)
            println(s"Selection: From ${selectingFrom.get.node} to ${newPos.get.node}: $selected")
            selection.value = Some(selected)
          } else
            clearSelection()
          cursorPos.value = newPos.get
        }
      case A if e.shortcutDown => selectAll()
      case _ => /*println("Key pressed: "+e);*/ processed = false
    }
    if (processed)
      e.consume()
  }
}


private class MathCursor(val cursorSide: CursorSide) extends Region with MathHighlight {
  id = Integer.toHexString(hashCode) // TODO: remove
  styleClass += "mathCursor"
  cursorSide match {
    case CursorLeft => styleClass += "mathCursorLeft"
    case CursorRight => styleClass += "mathCursorRight"
  }
  override def setSize(size:Bounds) = {
    layoutX = size.minX
    layoutY = size.minY
    prefWidth = size.width
    prefHeight = size.height
    resize(size.width,size.height)
  }
}

private class MathSelection extends Region with MathHighlight {
  id = Integer.toHexString(hashCode) // TODO: remove
  styleClass += "mathSelection"
  override def setSize(size:Bounds) = {
    layoutX = size.minX
    layoutY = size.minY
    prefWidth = size.width
    prefHeight = size.height
    resize(size.width,size.height)
  }
}

