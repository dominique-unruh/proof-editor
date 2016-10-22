package ui.mathview

import javafx.geometry.Bounds
import javafx.scene.control.Alert
import javafx.scene.layout

import cmathml.CMathML._
import cmathml.MutableCMathML.fromCMathML
import cmathml._
import fastparse.core.ParseError
import misc.Log
import misc.Utils.ImplicitConversions._
import ui.mathview.MathView.{CursorLeft, CursorPos, CursorRight, CursorSide}

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

  private[MathEdit] def makeExtendBinopCombinations(entries : ((CSymbol,String),CSymbol)*) =
    Map(entries.map { case ((sym,op),newSym) => (sym.cd,sym.name,op) -> (newSym.cd,newSym.name) } : _*)
  private[MathEdit] lazy val extendBinopCombinations = makeExtendBinopCombinations(
    relation1.equal -> ">" -> logic1.implies,
    relation1.equal -> "/" -> relation1.neq,
    relation1.gt -> "=" -> relation1.geq,
    relation1.lt -> "=" -> relation1.leq,
    relation1.leq -> ">" -> logic1.equivalent
  )
}

class MathEdit extends MathView {
  import MathEdit._

  val editable = ObjectProperty(None : Option[MutableCMathML])
  var _onChange : () => Unit = { () => }
  private var selectingFrom : Option[CursorPos] = None
  val cursorPos = ObjectProperty[CursorPos](CursorPos(mathDoc.root, CursorLeft))
  val selection = ObjectProperty[Option[MutableCMathML]](None)
  init()

  private def init() = {
    styleClass += "mathedit"
    focusTraversable = true

    installHighlight(editable,classOf[EditableHighlight])
    installHighlight(selection,classOf[MathSelection])

    mathDoc.addGlobalChangeListener({ _ => _onChange() })

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

    onContextMenuRequested = { e:ContextMenuEvent =>
      //    println("Context menu requested: "+e)
      createContextMenu.show(this,e.screenX,e.screenY)
    }

    onMousePressed = { e:MouseEvent =>
      Log.debug("clicked",e)
      requestFocus()
      val pos = getPositionFromMouse(e)
      pos match {
        case None =>
        case Some(pos2) => moveTo(pos2,selecting=e.shiftDown)
      }
    }

    onMouseDragged = { e:MouseEvent =>
      val pos = getPositionFromMouse(e)
      Log.debug("dragged pos",pos)
      pos match {
        case None =>
        case Some(pos2) => moveTo(pos2,selecting=true)
      }
    }

    onKeyPressed = handleKeyPress(_:KeyEvent)
    onKeyTyped = handleKeyTyped(_:KeyEvent)
  }

  def getPositionFromMouse(e: MouseEvent) : Option[CursorPos] = {
//    Log.debug("click",e.target)
    getMathNodeFromMouse(e) match {
      case None => None
      case Some(node) =>
        val bb = node.boundingBoxInScene
        val centerX = (bb.getMinX+bb.getMaxX)/2
//        Log.debug("clikc",e.sceneX,bb,centerX)
        val side = if (e.sceneX >= centerX) CursorRight else CursorLeft
        val pos = CursorPos(node.math, side)
        Some(pos)
    }
  }

  def onChange(f:()=>Unit) = _onChange = f

  override def setMath(m : CMathML): Unit = {
    super.setMath(m)
    cursorPos.value = CursorPos(mathDoc.root, CursorRight)
  }

  def setMathEditable(m : CMathML): Unit = {
    setMath(m)
    editable.value = Some(mathDoc.root)
  }

  override def setMath(m : MutableCMathML): Unit = {
    super.setMath(m)
    cursorPos.value = CursorPos(mathDoc.root, CursorRight)
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
    if (selection.value.exists(inEditableRange))
      cm.items += menuItem("C_ut", clipboardCut())
    else
      cm.items += menuItem("Cut")
    if (selection.value.exists(inEditableRange)
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


  def selectAll() = {
    if (inEditableRange(cursorPos.value.node) && (selection.value != editable.value))
      selection.value = editable.value
    else if (selection.value.contains(mathDoc.root) && editable.value.isDefined)
      selection.value = editable.value
    else
      selection.value = Some(mathDoc.root)
    cursorPos.value = CursorPos(selection.value.get, CursorRight)
  }

  private def extendBinop(char:String) : Boolean = {
    cursorPos.value match {
      case CursorPos (none @ MCNone(), CursorLeft) =>
        none.parent match {
          case MApply(hd @ MCSymbol(cd,name),_*) =>
            extendBinopCombinations.get(cd,name,char) match {
              case Some((newCd,newName)) =>
                hd.replaceWith(MCSymbol(newCd,newName))
                true
              case None => false
            }
          case _ => false
        }
      case _ => false
    }
  }

  private def extendOrInsertBinop(char:String, op:CMathML) =
    if (!extendBinop(char))
      insertBinaryOp(op)

  private def handleKeyTyped(e:KeyEvent) = {
//    println("Key typed: "+e)
    var processed = true
    e.character match {
      case "+" => extendOrInsertBinop("+",arith1.plus)
      case "-" => extendOrInsertBinop("-",arith1.minus)
      case "*" => extendOrInsertBinop("*",arith1.times)
      case "/" => extendOrInsertBinop("/",arith1.divide)
      case ">" => extendOrInsertBinop(">",relation1.gt)
      case "<" => extendOrInsertBinop("<",relation1.lt)
      case "=" => extendOrInsertBinop("=",relation1.equal)
      case "^" => extendOrInsertBinop("^",arith1.power)
      case "|" => extendOrInsertBinop("|",logic1.or)
      case "&" => extendOrInsertBinop("&",logic1.and)
      case "T" => insertMath(logic1.trueSym)
      case "F" => insertMath(logic1.falseSym)
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
              case CursorLeft => replaceWith(target, new MApply(fromCMathML(arith1.times), mmath, hole))
              case CursorRight => replaceWith(target, new MApply(fromCMathML(arith1.times), hole, mmath)) }
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
        if (newPos.isDefined)
          moveTo(newPos.get,selecting=e.shiftDown)
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

  private def storeInClipboard(math: MutableCMathML): Unit ={
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
    case Some(math) => storeInClipboard(math)
  }

  /** Cuts the current selection into the clipboard */
  def clipboardCut(): Unit = selection.value match {
    case Some(math) =>
      storeInClipboard(math)
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

  private def clipboardToCMathML(clip:Clipboard) : Option[CMathML] = {
    clip.content(dataformatCMathML) match {
      case math: CMathML => // TODO: do not use serialization here. Instead, either rely on XML transport,
        // or simply put an index to an hashmap with unguessable IDs into the clipboard
        return Some(math)
      case _ =>
    }

    clip.content(dataformatCMathMLXML) match {
      case xml: Any => ???
      case _ =>
    }

    clip.content(DataFormat.PlainText) match {
      case str: String =>
        try { return Some(CMathML.fromPopcorn(str)) }
        catch { case e : ParseError => }
    }

    None
  }

  def clipboardPaste(): Unit = {
    val clip = Clipboard.systemClipboard
    clipboardToCMathML(clip) match {
      case Some(math) => insertMath(math)
      case None => errorPopup("Selection does not contain Content MathML or another supported math format")
    }
  }

  /** Deletes the current selection */
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

  private def moveTo(newPos: CursorPos, selecting:Boolean=false) = {
    if (selecting) {
      if (selectingFrom.isEmpty)
        selectingFrom = Some(cursorPos.value)
      val selected = encompassingNode(newPos.node,selectingFrom.get.node)
      selection.value = Some(selected)
    } else
      clearSelection()
    cursorPos.value = newPos
  }

  private def moveLeft() = {
    leftOf(cursorPos.value) match {
      case None =>
      case Some(pos) => moveTo(pos)
    }
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
    case None if !inEditableRange(cursorPos.value.node) =>
      moveLeft()
    case None =>
      val cursor = cursorPos.value
      val hole = new MCNone
      def leftAndBS() = {
        leftOf(cursor) match {
          case None =>
          case Some(pos) =>
            cursorPos.value = pos
            backspace()
        }}
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
      (cursor.node, cursor.side) match {
        case (m @ (MCN(_)|MCI(_)|MCSymbol(_,_)|MCError(_,_,_)), CursorRight) => replaceWith(m,hole); cursorPos.value = CursorPos(hole,CursorLeft)
        case (m @ (MApply(_,_*) | MCNone()), CursorRight) => leftAndBS()
        case (m, CursorLeft) =>
          m.parent match {
            case _ : MutableCMathMLDocument => () // We are at the left of the formula
            case parent : MutableCMathML if !inEditableRange(parent) => moveLeft()
            case parent : MApply =>
              (parent,m) match {
                case MApply.IsHead() => leftAndBS()
                case MApply.IsArg(i) =>
                  // TODO: check if parent is rendered as f(...). We currently assume infix rendering
                  if (i==0) leftAndBS()
                  else singleNotNoneArg(parent) match {
                    case None => moveLeft()
                    case Some((arg,idx)) =>
                      arg.replaceWith(new MCNone) // detaching
                      replaceWith(parent,arg)
                      cursorPos.value = CursorPos(arg,if (idx<i) CursorRight else CursorLeft)
                  }
                case _ => ??? // Must be in the attributes
              }
          }
      }
  }

  private def singleNotNoneArg(apply:MApply) = {
    var num = 0
    var arg = new MCNone : MutableCMathML
    var idx = -1
    for ((a,i) <- apply.args.zipWithIndex) {
      if (!a.isInstanceOf[MCNone]) { num += 1; arg = a; idx = 0 }
    }
    if (num<=1) Some((arg,idx)) else None
  }
}



private class MathCursor(val cursorSide: CursorSide) extends javafx.scene.layout.Region with MathHighlight {
  this.id = Integer.toHexString(hashCode) // TODO: remove
  getStyleClass += "mathCursor"
  cursorSide match {
    case CursorLeft => getStyleClass += "mathCursorLeft"
    case CursorRight => getStyleClass += "mathCursorRight"
  }
  override def setSize(size:Bounds) = {
    this.layoutX = size.minX
    this.layoutY = size.minY
    setPrefWidth(size.width)
    setPrefHeight(size.height)
    resize(size.width,size.height)
  }
}

private class MathSelection extends javafx.scene.layout.Region with MathHighlight {
  this.id = Integer.toHexString(hashCode) // TODO: remove
  this.styleClass += "mathSelection"
  override def setSize(size:Bounds) = {
    this.layoutX = size.minX
    this.layoutY = size.minY
    setPrefWidth(size.width)
    setPrefHeight(size.height)
    resize(size.width,size.height)
  }
}

private class EditableHighlight extends layout.Region with MathHighlight {
  this.id = Integer.toHexString(hashCode) // TODO: remove
  this.styleClass += "mathEditable"
  override def setSize(size:Bounds) = {
    this.layoutX = size.minX
    this.layoutY = size.minY
    setPrefWidth(size.width)
    setPrefHeight(size.height)
    resize(size.width,size.height)
  }
}
