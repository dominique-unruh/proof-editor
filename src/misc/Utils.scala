package misc

import javafx.event.{ActionEvent, Event}
import javafx.scene.web.WebView

import com.sun.javafx.webkit.WebConsoleListener

import scala.collection.mutable.ArrayBuffer
import scala.xml.{Elem, Node}

/**
  * Created by unruh on 7/3/16.
  */
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
  }
}
