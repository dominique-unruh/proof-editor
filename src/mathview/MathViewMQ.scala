package mathview

import javafx.scene.layout.BorderPane
import javafx.scene.web.WebView

import cmathml.CMathML
import misc.Pure
import netscape.javascript.JSObject
import misc.Utils.JavaFXImplicits._

/** JavaFX widget for showing and editing math formulas.
  * TODO: editing not yet implemented.
  * Math is represented as CMathML objects
 *
  * @see [[setMath]] for setting the displayed math
  * @see [[CMathML]] for describing math
  * */
class MathViewMQ extends BorderPane {
  private val web = new WebView
  private var math = null : CMathML
  private var loaded = false
//  installBridge()
//  println(MathViewMQ.mathjaxPage(MathViewMQ.base,math))
  web.getEngine.loadContent(MathViewMQ.mathjaxPage(MathViewMQ.base,"remove-me"))
  private val window = web.getEngine.executeScript("window").asInstanceOf[JSObject]
  window.setMember("controller", JSBridge)
//  private val setMathJS = web.getEngine.executeScript("setMath").asInstanceOf[JSObject]
  setCenter(web)
  getStyleClass.add("mathview")

  /** Sets the currently displayed math */
  def setMath(m:CMathML) = {
    math = m
    if (loaded) sendMathToJS // TODO: must only happen in FX thread
  }

  /** Returns the currently shown math */
  def getMath = math

  private def sendMathToJS: Unit = {
    val tex = MQLatex.cmathmlToLatex(math)
    window.call("setMath",tex)
  }

  private object JSBridge {
    def onMathDeselected() = println("onMathDeselected")
    def onMathSelection(path:String) = println("onMathSelection",path)
    def onMathRendered(w:Double, h:Double) = { web.setMinSize(w,h); web.setPrefSize(w,h); web.resize(w,h); println("onMathRendered",w,h) }
    def onLoad() = { println("onLoad"); loaded = true; if (math!=null) sendMathToJS }
  }
}


private object MathViewMQ {
  private val base = getClass.getResource("/").toString
  checkResources

  private def checkResources = {
    def checkResource(name:String) =
      if (getClass.getResource(name)==null) throw new RuntimeException("missing resource "+name)
    checkResource("/mathquill/mathquill.css")
    checkResource("/jquery.js")
    checkResource("/mathview.css")
    checkResource("/mathviewmq.js")
  }

  @Pure
  private def mathjaxPage(base:String,math:String): String =
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