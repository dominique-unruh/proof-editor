package misc

import java.io.FileNotFoundException
import java.lang.Thread.UncaughtExceptionHandler
import java.security.{AccessController, PrivilegedAction}
import java.util.function.Predicate
import java.util.regex.Pattern
import javafx.beans.property.{ObjectProperty, ObjectPropertyBase, Property, SimpleObjectProperty}
import javafx.beans.property.adapter.JavaBeanObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.Event
import javafx.scene.shape.Rectangle
import javafx.scene.web.{HTMLEditor, WebView}

import cmathml.CMathML

import scala.reflect.runtime.universe.TypeTag
import com.sun.javafx.webkit.WebConsoleListener

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.xml.{Atom, Comment, Elem, Text}


object Utils {
  def splitString2(str: String, sep: Char) = {
    val idx = str.indexOf(sep)
    assert(idx>=0)
    (str.substring(0,idx), str.substring(idx+1))
  }

  def lowerFirst(s: String) = if (s.isEmpty) s else s(0).toLower + s.substring(1)

  def xmlAddNewlines(nodes : Seq[scala.xml.Node]) = intersperse(nodes, Text("\n"))

  def invokeListeners[T](listeners: Iterable[T], invoke: T => Unit) =
    for (l <- listeners)
      try {
        invoke(l)
      } catch {
        case e: Throwable =>
          Log.stackTrace("Exception in listener", e)
      }


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

  private def normalizeNodeList(nodes : Seq[scala.xml.Node]) : Seq[scala.xml.Node] = {
    var result = Nil : List[scala.xml.Node]
    for (node <- nodes) {
      node match {
        case a : Atom[_] =>
          val str = a.text
          if (str!="")
            result match {
              case Text(prev)::rest => result = Text(prev+str) :: rest
              case _ => result = Text(str) :: result
            }
        case _ =>
          result = node :: result
      }
    }
    result.reverse
  }

  private val startsWithNL = """(?s)\s*\n.*""".r
  private val lineStartSpace = """\n\s*""".r
  private val finalNL = """(?s)\s*$""".r
  def prettyXML(xml: Elem, indent : Int = 0) : Elem = {
    if (xml.attribute("http://www.w3.org/XML/1998/namespace", "space").map(_.text).contains("preserve"))
      return xml
    if (xml.child.isEmpty) return xml
    lazy val indentStr = "\n"+"  "*(indent+1)
    val children = ArrayBuffer(normalizeNodeList(xml.child) :_*)
    val startsWithNewline =
      children.head match { case Text(`startsWithNL`()) => true; case _ => false }

//    if (xml.label=="formulas")
//      for (c <- children)
//        Log.debug("child",c.getClass,c)

    for (i <- children.indices) {
      children(i) match {
        case Text(str) =>
          val newText = Text(lineStartSpace.replaceAllIn(str, indentStr))
          children.update(i, newText)
        case e : Elem => children.update(i, prettyXML(e,indent+1))
        case c : Comment =>
        case n => Log.warn("Don't know how to handle: ",n,n.getClass); ???
      }
    }

    val last = children.length-1
    if (startsWithNewline) {
      children(last) match {
        case Text(str) =>
          val newText = Text(finalNL.replaceFirstIn(str,"\n"+"  "*indent))
          children.update(last, newText)
        case c => children.append(Text("\n"+"  "*indent))
      }
    } else {
      children(last) match {
        case Text(str) =>
          val newText = Text(finalNL.replaceFirstIn(str,""))
          children.update(last, newText)
        case _ =>
      }
    }

    xml.copy(child=children.toList)
  }

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

  object Typed {
    def unapply[T](t:T) = Some(t)
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
  override def get : T = getter
  protected def getter : T
  protected def setter(value:T): Unit
  override def fireValueChangedEvent() = super.fireValueChangedEvent()
}


