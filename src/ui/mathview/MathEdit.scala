package ui.mathview

import ui.mathview.MathViewFX.{CursorLeft, CursorRight}

import scalafx.Includes._
import scalafx.scene.input.KeyEvent

class MathEdit extends MathViewFX {
  focusTraversable = true

  onKeyPressed = { e:KeyEvent =>
    import scalafx.scene.input.KeyCode._
    e.code match {
      case Right =>
        val newPos = rightOf(cursorPos.value, jump=e.controlDown)
        newPos.foreach( cursorPos.value = _ )
      case Left =>
        val newPos = leftOf(cursorPos.value, jump=e.controlDown)
        newPos.foreach( cursorPos.value = _ )
      case _ => println("Key pressed: "+e)
    }
  }
}
