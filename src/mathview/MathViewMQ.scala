package misc

import javafx.scene.layout.BorderPane
import javafx.scene.web.WebView

import netscape.javascript.JSObject

class MathViewMQ(math:String) extends BorderPane {
  private val web = new WebView
  installBridge()
//  println(MathViewMQ.mathjaxPage(MathViewMQ.base,math))
  web.getEngine.loadContent(MathViewMQ.mathjaxPage(MathViewMQ.base,math))
  setCenter(web)
  getStyleClass.add("mathview")

  def installBridge() = {
    val window = web.getEngine.executeScript("window").asInstanceOf[JSObject]
    window.setMember("controller", JSBridge)
  }

  object JSBridge {
    def onMathDeselected() = println("onMathDeselected")
    def onMathSelection(path:String) = println("onMathSelection",path)
    def onMathRendered(w:Double, h:Double) = { web.setMinSize(w,h); web.setPrefSize(w,h); web.resize(w,h); println("onMathRendered",w,h) }
  }
}


object MathViewMQ {
  val base = getClass.getResource("/").toString
  checkResources

  def checkResources = {
    def checkResource(name:String) =
      if (getClass.getResource(name)==null) throw new RuntimeException("missing resource "+name)
    checkResource("/mathquill/mathquill.css")
    checkResource("/jquery.js")
    checkResource("/mathview.css")
    checkResource("/mathviewmq.js")
  }

  @Pure
  def mathjaxPage(base:String,math:String): String =
    s"""
<html>
  <head>
    <meta charset="utf-8" />
    <base href="$base" />
    <link href="mathquill/mathquill.css" rel="stylesheet" type="text/css">
    <link href="mathview.css" rel="stylesheet" type="text/css">
    <script src="jquery.js"></script>
    <script src="mathquill/mathquill.js"></script>
    <script src="mathviewmq.js"></script>
  </head>
  <body onload="onLoad()">
    <span id="formula-span">$math</span>
  </body>
</html>"""

}