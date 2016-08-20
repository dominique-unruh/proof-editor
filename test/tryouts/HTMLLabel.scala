package tryouts

import javafx.scene.{Group, text}
import javafx.scene.control.Label
import javafx.scene.layout.StackPane
import javafx.scene.text.TextFlow
import javafx.scene.web.WebView

import misc.Log

import scala.collection.mutable
import scala.compat.Platform
import scala.xml._
import scalafx.application
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Scene, layout}
import scalafx.scene.control.ScrollPane
import scalafx.scene.layout.VBox
import ui.HTMLLabel

class HTMLLabel1 extends Group {
  private val webview = new WebView()
  getChildren.add(webview)

  def setHTML(html : String): Unit =
    webview.getEngine.loadContent(html)

}

class HTMLLabel2 extends Label {
  def setHTML(html : String): Unit =
    setText(html)

}


object TestHTMLLabel extends JFXApp {
  val vbox = new VBox()
  val scroll = new ScrollPane()
  scroll.content = vbox
  scroll.pannable = true
  scroll.hbarPolicy = ScrollPane.ScrollBarPolicy.Never
//  scroll.hbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded

  stage = new PrimaryStage {
    width = 800
    height = 600
    scene = new Scene(new layout.StackPane) {
      content = scroll
    }
  }

  application.Platform.runLater(benchmark())

  def benchmark(): Unit = {
    val startTime = Platform.currentTime

    vbox.fillWidth = true

    for (i <- 1 to 100) {
      val label = new HTMLLabel()
      label.setHTML(<span>Hello <b>{i}<plain>pl<i>ai</i>n</plain></b> etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc etc </span>)
      vbox.children.add(label)
    }

    val endTime = Platform.currentTime

    val passed = (endTime.toDouble-startTime)/1000

    println("passed",passed)
  }
}