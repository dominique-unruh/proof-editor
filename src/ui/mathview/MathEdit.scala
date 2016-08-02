package ui.mathview

import javafx.geometry.Bounds
import javafx.scene.control.Alert
import javafx.scene.input

import cmathml.CMathML.{plus, times}
import cmathml.MutableCMathML.fromCMathML
import cmathml._
import misc.Utils.ImplicitConversions._
import ui.mathview.MathViewFX.{CursorLeft, CursorPos, CursorRight, CursorSide}

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.event.ActionEvent
import scalafx.scene.control.{ContextMenu, MenuItem}
import scalafx.scene.input._
import scalafx.scene.layout.Region

object MathEdit {
  val AlphaChar = "([a-zA-Z])".r
  private val dataformatCMathML = new DataFormat("-xxx-cmathml-internal-")
  private val dataformatPopcorn = new DataFormat("text/x.openmath-popcorn")
  private val dataformatCMathMLXML = new DataFormat("application/mathml-content+xml")
}

class MathEdit extends MathViewFX {
  import MathEdit._

  focusTraversable = true

  val editable = ObjectProperty(None : Option[MutableCMathML])

  override def setMath(m : CMathML): Unit = {
    super.setMath(m)
    cursorPos.value = CursorPos(mathDoc.root, CursorRight)
  }

  override def setMath(m : MutableCMathML): Unit = {
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

  private def removeCursor() : Unit = removeCursor(cursorPos.value)
  private def removeCursor(pos:CursorPos) : Unit =
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
    if (selection.value.isDefined)
      cm.items += menuItem("_Copy", clipboardCopy())
    else
      cm.items += menuItem("Copy")
    if (selection.value.exists(inEditableRange(_)))
      cm.items += menuItem("C_ut", clipboardCut())
    else
      cm.items += menuItem("Cut")
    if (selection.value.exists(inEditableRange(_))
      || (selection.value.isEmpty && inEditableRange(cursorPos.value.node)))
      cm.items += menuItem("_Paste", clipboardPaste())
    else
      cm.items += menuItem("Paste")
//    if (selection.value.exists(inEditableRange(_)))
//      cm.items += menuItem("_Clear", {
//        val none = new MCNone
//        selection.value.get.replaceWith(none)
//        cursorPos.value = CursorPos(none,CursorLeft)
//      })
//    if (cm.items.isEmpty)
//      cm.items += menuItem("No actions available")
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
      case "*" => insertBinaryOp(times)
      case "/" => insertBinaryOp(CMathML.divide)
      case "=" => insertBinaryOp(CMathML.equal)
      case AlphaChar(c) => insertMath(CI(c))
      case _ => processed = false
    }
    if (processed) e.consume()
  }

  def replaceWith(a: MutableCMathML, b: MutableCMathML) = {
    a.replaceWith(b)
    if (editable.value.contains(a))
      editable.value = Some(b)
    if (selection.value.contains(a))
      selection.value = Some(b)
    if (cursorPos.value.node == a)
      cursorPos.value = cursorPos.value.copy(node=b)
  }

  /** Inserts math at cursor or instead of selection. (Only if in the editable range.) */
  def insertMath(math:CMathML) : Unit = {
    val mmath = fromCMathML(math)
    selection.value match {
      case Some(sel) =>
        if (!inEditableRange(sel)) return
        replaceWith(sel,mmath)
        clearSelection()
      case None =>
        val target = cursorPos.value.node
        if (!inEditableRange(target)) return
        target match {
          case hole : MCNone =>
            replaceWith(hole,mmath)
          case _ =>
            val hole = new MCNone
            cursorPos.value.side match {
              case CursorLeft => replaceWith(target, new MApply(fromCMathML(times), mmath, hole))
              case CursorRight => replaceWith(target, new MApply(fromCMathML(times), hole, mmath)) }
            hole.replaceWith(target)
          }
        }
    cursorPos.value = CursorPos(mmath,CursorRight)
  }

  private def insertBinaryOp(op:CMathML) : Unit = {
    val (target,toLeft) = selection.value match {
      case None => (cursorPos.value.node, cursorPos.value.side==CursorRight)
      case Some(sel) => (sel,true) }

    if (!inEditableRange(target)) return

    val left = new MCNone()
    val right = new MCNone()
    val binop = new MApply(fromCMathML(op), left, right)

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
      case X if e.shortcutDown => clipboardCut()
      case C if e.shortcutDown => clipboardCopy()
      case V if e.shortcutDown => clipboardPaste()
      case A if e.shortcutDown => selectAll()
      case _ => /*println("Key pressed: "+e);*/ processed = false
    }
    if (processed)
      e.consume()
  }

  override def getImageOfNode(math:MutableCMathML) = {
    val sel = selection.value
    selection.value = None
    removeCursor()
    val image = super.getImageOfNode(math)
    showCursor(cursorPos.value)
    selection.value = sel
    image
  }

  private def toClipboard(math: MutableCMathML): Unit ={
    val content = new ClipboardContent

    val cmathml = math.toCMathML
    content.put(dataformatCMathML,cmathml)

    val xml = cmathml.toXMLDoc
    content.put(dataformatCMathMLXML, xml)

    val str = cmathml.toString
    content.putString(str)

    val image = getImageOfNode(math)
    content.putImage(image)

    val popcorn = cmathml.toPopcorn
    content.put(dataformatPopcorn,popcorn)

    Clipboard.systemClipboard.setContent(content)
  }

  /** Copies the current selection into the clipboard */
  def clipboardCopy(): Unit = selection.value match {
    case None =>
    case Some(math) => toClipboard(math)
  }

  /** Cuts the current selection into the clipboard */
  def clipboardCut(): Unit = selection.value match {
    case Some(math) =>
      toClipboard(math)
      if (inEditableRange(math)) {
        val none = new MCNone
        replaceWith(math, none)
        clearSelection()
        cursorPos.value = CursorPos(none, CursorLeft)
      }
    case _ =>
  }
  private def errorPopup(msg: String): Unit =
    new Alert(Alert.AlertType.ERROR, msg).showAndWait()

  def clipboardPaste(): Unit = {
    val clip = Clipboard.systemClipboard
    clip.content(dataformatCMathML) match {
      case math : CMathML =>
        insertMath(math)
      case _ =>
        clip.content(dataformatCMathMLXML) match {
          case xml : Any => ???
          case null => errorPopup("Selection does not contain Content MathML")
        }
    }
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

