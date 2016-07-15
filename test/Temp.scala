import javafx.animation.{KeyFrame, Timeline}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.layout.HBox
import javafx.scene.web.WebView

import netscape.javascript.JSObject
import org.w3c.dom.Document
import ui.TestFxApp

import scala.collection.JavaConverters
import scala.xml.Elem

class WebLabel extends HBox {
  val web = new WebView
  getChildren.add(web)

  web.getEngine.documentProperty.addListener(new ChangeListener[Document] {
    override def changed(observable: ObservableValue[_ <: Document], oldValue: Document, newValue: Document): Unit =
      refreshSize()
  })

  private final def toDouble(x:AnyRef) : Double = x match {
    case y : Integer => y.toDouble
    case y : java.lang.Double => y
  }

  final def getBodySize() = {
    val result : JSObject = web.getEngine.executeScript("document.getElementById('rootspan').getBoundingClientRect()").asInstanceOf[JSObject]
    val right = toDouble(result.getMember("right"))
    val bottom = toDouble(result.getMember("bottom"))
    (right,bottom)
  }

  final def refreshSize(): Unit = {
    val (width,height) = getBodySize()
    setPrefSize(width,height)
  }

  def setHTML(html: Elem) = {
    val doc = <html><body style="border: solid 1px blue; margin: 0 0 0 0; overflow-x: hidden; overflow-y: hidden;"><span id="rootspan">{html}</span></body></html>
    web.getEngine.loadContent(doc.toString)
  }
}

object Temp {

  def main(args: Array[String]): Unit = {
    TestFxApp.run {
      val snippet = <span id="h">hello</span>
      val label = new WebLabel
      label.setHTML(snippet)

      val tl = new Timeline(new KeyFrame(javafx.util.Duration.millis(1000), new EventHandler[javafx.event.ActionEvent] {
        override def handle(event: ActionEvent): Unit = {
          println("timer", label.getBodySize)
          val img = label.web.getEngine.getDocument.createElement("b")
          img.setTextContent(" hehe ")
          label.web.getEngine.getDocument.getElementById("h").appendChild(img)
          label.refreshSize()
        }
      }))
      tl.setCycleCount(1000)
      tl.play()

      label
    }
  }
}

/*      val web = new WebView()
      val snippet = <span id="h">hello</span>
      val html = <html><body style="border: solid 1px blue;"><span id="rootspan">{snippet}</span></body></html>
      println(html)
      web.getEngine.loadContent(html.toString())

      println(web.getPrefWidth,web.getPrefHeight)

      web.getEngine.documentProperty.addListener(new ChangeListener[Document] {
        override def changed(observable: ObservableValue[_ <: Document], oldValue: Document, newValue: Document): Unit =
          println("document changed")
      })

      val engine = web.getEngine



      val tl = new Timeline(new KeyFrame(javafx.util.Duration.millis(1000), new EventHandler[javafx.event.ActionEvent] {
        override def handle(event: ActionEvent): Unit = {
          println("timer",getSizes)
          val img = engine.getDocument.createElement("b")
          img.setTextContent(" hehe ")
          engine.getDocument.getElementById("h").appendChild(img)
        }}))
      tl.setCycleCount(1000)
      tl.play()

      web
    }
  }
}*/