package misc

import java.io.FileNotFoundException
import java.lang.Thread.UncaughtExceptionHandler
import java.security.{AccessController, PrivilegedAction}
import java.util.function.Predicate
import javafx.beans.property.{ObjectProperty, ObjectPropertyBase, Property, SimpleObjectProperty}
import javafx.beans.property.adapter.JavaBeanObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.Event
import javafx.scene.shape.Rectangle
import javafx.scene.web.{HTMLEditor, WebView}

import scala.reflect.runtime.universe.TypeTag
import com.sun.javafx.webkit.WebConsoleListener

import scala.collection.mutable
import scala.xml.Elem


object Utils {
  def max(nums: Seq[Double]) = {
    val it = nums.iterator
    var m = it.next()
    for (n <- it)
      if (n > m) m = n
    m
  }

  def firstElementIn(xml: Elem): Elem =
    xml.child.find(_.isInstanceOf[Elem]).get.asInstanceOf[Elem]

  def elementsIn(xml: Elem) : Seq[Elem] =
    xml.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

  /** Makes a copy of xs with sep interspersed. E.g., intersperse(ArrayBuffer(x,y),sep) = List(x,sep,y). */
  @Pure
  def intersperse[T](xs:Seq[T],sep:T) : List[T] = {
    var result : List[T] = List.empty
    val it = xs.reverseIterator
    result = it.next() :: result
    for (x <- it) { result = x :: sep :: result }
    result
  }

  @Pure
  def cast[A,B](x : A)(implicit a: TypeTag[A], b: TypeTag[B]) : B =
    cast(a,b,x)
  @Pure
  def cast[A,B](a: TypeTag[A], b: TypeTag[B], x:A) : B =
    if (a!=null && b!=null && a==b) x.asInstanceOf[B] else throw new ClassCastException(s"$a != $b in type cast")

  @Pure
  def resourceFile(name:String*): String = {
    for (n <- name) {
      val res = getClass.getResource(n)
      if (res != null && res.getProtocol == "file") return res.getFile
    }
    throw new FileNotFoundException("Resource "+name.mkString(" or ")+" as file")
  }

  object ImplicitConversions {
    import scala.language.implicitConversions
    implicit def lambdaToEventHandler[T <: Event](handler: T => Unit) : javafx.event.EventHandler[T] = new javafx.event.EventHandler[T] {
      override def handle(dEvent: T): Unit = handler(dEvent)
    }

    implicit def lambdaToWebConsoleListener(handler: (WebView, String, Int, String) => Unit) : WebConsoleListener = new WebConsoleListener() {
      override def messageAdded(webView: WebView, message: String, lineNumber: Int, sourceId: String): Unit = handler(webView, message, lineNumber, sourceId)
    }

    implicit def lambdaToChangeListener[T](handler: T => Unit) : ChangeListener[T] = new ChangeListener[T]() {
      override def changed(observable: ObservableValue[_ <: T], oldValue: T, newValue: T) = handler(newValue)
    }

    implicit def lambdaToRunnable(f: () => Unit) : Runnable = new Runnable {
      override def run(): Unit = f()
    }

    implicit def lambdaToThreadUncaughtExceptionHandler(handler: (Thread, Throwable) => Unit) : UncaughtExceptionHandler =
      new UncaughtExceptionHandler {
        override def uncaughtException(t: Thread, e: Throwable): Unit = handler(t, e)
      }

    implicit def lambdaToPredicate[E](pred: E => Boolean) : Predicate[E] = new Predicate[E] {
      override def test(t: E): Boolean = pred(t)
    }
  }
}

/** A simple way of implementing properties. To implement a property, subclass [[GetterSetterProperty]]
  * and override [[get]] (must return the current value of the property), [[setter]] (must set the value).
  * When the value of the property changes (i.e., if [[getter]] would return a different value from now on),
  * invoke [[fireValueChangedEvent]]. (Do not invoke [[fireValueChangedEvent]] if the value was changed via [[setter]].)
  *
  * @tparam T type of the value contained in the property
  */
abstract class GetterSetterProperty[T] extends SimpleObjectProperty[T] {
  override def set(value: T) : Unit = {
    setter(value)
    fireValueChangedEvent()
  }
  override def bind(obs : ObservableValue[_ <: T]) = sys.error("Binding not supported")
  override def get() : T = getter
  protected def getter : T
  protected def setter(value:T): Unit
  override def fireValueChangedEvent() = super.fireValueChangedEvent()
}


