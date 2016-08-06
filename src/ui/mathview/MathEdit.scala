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
  val NumChar = "([0-9])".r
  private val dataformatCMathML = new DataFormat("application/x.proof-editor-math-internal")
  private val dataformatPopcorn = new DataFormat("text/x.openmath-popcorn")
  private val dataformatCMathMLXML = new DataFormat("application/mathml-content+xml")
}

class MathEdit extends MathViewFX {
  import MathEdit._

  focusTraversable = true

  val editable = ObjectProperty(None : Option[MutableCMathML])
  installHighlight(editable,classOf[EditableHighlight])

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

  private def installHighlight(prop: ObjectProperty[Option[MutableCMathML]],
                               highlightClass: Class[_<:MathHighlight]): Unit = {
    val cons = highlightClass.getConstructor()
    prop.onChange { (_, oldSel, newSel) =>
      assert(oldSel!=newSel)
      if (oldSel != newSel) {
        if (oldSel.isDefined) {
          val oldHighlights = getHighlights(oldSel.get)
          oldHighlights.removeIf(highlightClass.isInstance(_: MathHighlight))
        }
        if (newSel.isDefined) {
          val newHighlights = getHighlights(newSel.get)
          newHighlights += cons.newInstance()
        }
      }
    }
  }

  val selection = ObjectProperty[Option[MutableCMathML]](None)
  installHighlight(selection,classOf[MathSelection])

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
      case "^" => insertBinaryOp(CMathML.power)
      case AlphaChar(c) => insertMath(CI(c))
      case NumChar(c) => insertDigit(c(0)-'0')
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

  def insertDigit(digit:Int) : Unit = {
    assert(digit >= 0 && digit <= 9)
    (selection.value, cursorPos.value) match {
      case (None, CursorPos(m@MCN(i), CursorRight)) =>
        if (i.isWhole) m.n = i * 10 + digit
        else ???
      case _ => insertMath(CN(digit))
    }
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
      case Delete => delete()
      case BackSpace => backspace()
      case _ => processed = false
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
      case math : CMathML => // TODO: do not use serialization here. Instead, either rely on XML transport,
        // or simply put an index to an hashmap with unguessable IDs into the clipboard
        insertMath(math)
      case _ =>
        clip.content(dataformatCMathMLXML) match {
          case xml : Any => ???
          case null => errorPopup("Selection does not contain Content MathML")
        }
    }
  }

  /** Deletes the current selection
    * */
  def delete(): Unit = selection.value match {
    case Some(math) =>
      if (inEditableRange(math)) {
        val none = new MCNone
        replaceWith(math, none)
        clearSelection()
        cursorPos.value = CursorPos(none, CursorLeft)
      }
    case _ =>
  }

  /** Deletes the current selection
    *
    * TODO: Delete left of cursor if no selection
    * */
  def backspace(): Unit = selection.value match {
    case Some(math) =>
      if (inEditableRange(math)) {
        val none = new MCNone
        replaceWith(math, none)
        clearSelection()
        cursorPos.value = CursorPos(none, CursorLeft)
      }
    case None if (!inEditableRange(cursorPos.value.node)) =>
      leftOf(cursorPos.value) match {
        case None =>
        case Some(pos) => cursorPos.value = pos
      }
    case None =>
      val cursor = cursorPos.value
      val hole = new MCNone
        /*
        Notation: [] = CNone,  | = Cursor,  +BS = apply backspace again
                  [|] = []| or |[]
        x+[|] --> x
        []+[|] --> []
        DONE: (x+y)| --> x+(y|) + BS  (if y != [])
        []+(|y) --> |y
        x+(|y) --> (x|)+y (if y != [])
        DONE: x| --> |[]  (if x is variable or number)
        DONE: f(x1,...,xn)| --> f(x1,...,xn|) + BS
        f(...,x1,|x2,...) --> f(...,x1|,x2,...)  (if x1 != [])
        f(...,[],|x2,...) --> f(...,|[],x2,...)
        f(|[],...,[]) --> f|  (if f != [])
        [](|[],...,[]) --> |[]
        f(|x1,...,xn) --> ???   (if f != [] and not all xi=[])
       */
      (cursor.node, cursor.side, cursor.node.parent) match {
        case (m @ (MCN(_)|MCI(_)|MCSymbol(_,_)|MCError(_,_,_)), CursorRight, _) => replaceWith(m,hole); cursorPos.value = CursorPos(hole,CursorLeft)
        case (m @ (MApply(hd,args@_*)), CursorRight, _) =>
          leftOf(cursor) match {
            case None =>
            case Some(pos) =>
              cursorPos.value = pos
              backspace()
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

private class EditableHighlight extends Region with MathHighlight {
  id = Integer.toHexString(hashCode) // TODO: remove
  styleClass += "mathEditable"
  override def setSize(size:Bounds) = {
    layoutX = size.minX
    layoutY = size.minY
    prefWidth = size.width
    prefHeight = size.height
    resize(size.width,size.height)
  }
}

