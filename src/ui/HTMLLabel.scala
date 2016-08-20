package ui

import javafx.scene.text
import javafx.scene.text.TextFlow

import misc.Log

import scala.collection.mutable
import scala.xml.{Atom, Elem, EntityRef, Node}

class HTMLLabel extends TextFlow {
  def this(html : Elem) = {
    this()
    setHTML(html)
  }

  def setHTML(html : Elem) : Unit = {
    getChildren.clear()
    appendHTML(html)
  }

  private val styles = mutable.HashMap[String,String](
    "b" -> "-fx-font-weight:bold",
    "span" -> "",
    "i" -> "-fx-font-style:italic",
    "plain" -> "-fx-font-weight:normal"
  )

  private def appendHTML(html: Node, style : List[String] = Nil) : Unit = html match {
    case _: Atom[_] | _:EntityRef =>
      val txt = new text.Text(html.text)
      txt.setStyle(style.reverse.filter(!_.isEmpty).mkString(";"))
      getChildren.add(txt)
    case Elem(_, label, _, _, children @ _*) =>
      val style2 = styles.getOrElse(label, { Log.warn(s"unsupported tag $label"); "" }) :: style
      for (n <- children) appendHTML(n,style2)
  }
}

