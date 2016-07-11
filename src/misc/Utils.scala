package misc

import java.io.FileNotFoundException
import java.lang.Thread.UncaughtExceptionHandler
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

  @Pure
  def resourceFile(name:String*): String = {
    for (n <- name) {
      val res = getClass.getResource(n)
      if (res != null && res.getProtocol == "file") return res.getFile
    }
    throw new FileNotFoundException("Resource "+name.mkString(" or ")+" as file")
  }

  object JavaFXImplicits {
    import scala.language.implicitConversions
    implicit def lambdaToEventHandler[T <: Event](handler: T => Unit) = new javafx.event.EventHandler[T] {
      override def handle(dEvent: T): Unit = handler(dEvent)
    }

    implicit def lambdaToWebConsoleListener(handler: (WebView, String, Int, String) => Unit) = new WebConsoleListener() {
      override def messageAdded(webView: WebView, message: String, lineNumber: Int, sourceId: String): Unit = handler(webView, message, lineNumber, sourceId)
    }

    implicit def lambdaToChangeListener[T](handler: T => Unit) = new ChangeListener[T]() {
      override def changed(observable: ObservableValue[_ <: T], oldValue: T, newValue: T) = handler(newValue)
    }

    implicit def lambdaToRunnable(f: () => Unit) = new Runnable {
      override def run(): Unit = f()
    }

    implicit def lambdaToThreadUncaughtExceptionHandler(handler: (Thread, Throwable) => Unit) =
      new UncaughtExceptionHandler {
        override def uncaughtException(t: Thread, e: Throwable): Unit = handler(t, e)
      }
  }
}
