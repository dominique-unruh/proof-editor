package misc

import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.Event
import javafx.scene.web.WebView

import com.sun.javafx.webkit.WebConsoleListener

object Utils {
  /** Makes a copy of xs with sep interspersed. E.g., intersperse(ArrayBuffer(x,y),sep) = List(x,sep,y). */
  @Pure
  def intersperse[T](xs:Seq[T],sep:T) : List[T] = {
    var result : List[T] = List.empty
    val it = xs.reverseIterator
    result = it.next() :: result
    for (x <- it) { result = x :: sep :: result }
    return result
  }

  object JavaFXImplicits {
    implicit def lambdaToEventHandler[T<:Event](handler: T => Unit) = new javafx.event.EventHandler[T] {
      override def handle(dEvent: T): Unit = handler(dEvent)
    }

    implicit def lambdaToWebConsoleListener(handler: (WebView, String, Int, String) => Unit) = new WebConsoleListener() {
      override def messageAdded(webView: WebView, message: String, lineNumber: Int, sourceId: String): Unit = handler(webView,message,lineNumber,sourceId)
    }

    implicit def lambdaToChangeListener[T](handler: (ObservableValue[_ <: T], T, T) => Unit) = new ChangeListener[T]() {
      override def changed(observable : ObservableValue[_ <: T], oldValue : T, newValue : T) = handler(observable, oldValue, newValue)
    }
  }
}
