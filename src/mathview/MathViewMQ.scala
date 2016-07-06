package mathview

import javafx.application.Platform
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.scene.Node
import javafx.scene.layout.BorderPane
import javafx.scene.web.WebView

import cmathml.{CMathML, Path, PathRev}
import misc.Pure
import netscape.javascript.JSObject
import misc.Utils.JavaFXImplicits._

import scala.collection.mutable

/** JavaFX widget for showing and editing math formulas.
  * TODO: editing not yet implemented.
  * Math is represented as CMathML objects
  *
  * Must only be created in JavaFX application thread.
  *
  * @see [[setMath]] for setting the displayed math
  * @see [[CMathML]] for describing math
  * */
class MathViewMQ extends BorderPane {
  assert(Platform.isFxApplicationThread,"not in JavaFX application thread")
  private val web = new WebView
  private var math = null : CMathML
  private var editPath = None : Option[PathRev]
  private var loaded = false
//  web.getEngine.loadContent(MathViewMQ.mathjaxPage)
  web.getEngine.load(getClass.getResource("/mathview.html").toString)
  private val window = web.getEngine.executeScript("window").asInstanceOf[JSObject]
  window.setMember("controller", JSBridge)
  setCenter(web)
  getStyleClass.add("mathview")


  private val editedListeners = mutable.MutableList[CMathML=>Unit]()

  /** The listener is called whenever the user presses ENTER in the editable part of the formula.
    * The editing process is not automatically stopped at that point.
    * To stop editing, call [[setMath]].
    * @param listener
    */
  def addEditedListener(listener: CMathML => Unit) : Unit = editedListeners += listener
  private def fireEdited(math: CMathML) : Unit = for (l <- editedListeners) l(math)


  /** Sets the currently displayed math.
    * Must only be called in JavaFX application thread.
    *
    * @param m the formula to display
    * @param path a path to a subformula that should be editable
    * */
  def setMath(m:CMathML, path:Option[PathRev]=None) = {
    assert(Platform.isFxApplicationThread,"not in JavaFX application thread")
    math = m
    editPath = path
    if (loaded) sendMathToJS()
  }

  /** Returns the currently shown math */
  def getMath = math

  /** Must only be called in JavaFX application thread. */
  private def sendMathToJS(): Unit = {
    assert(Platform.isFxApplicationThread,"not in JavaFX application thread")
    val tex = MQLatex.cmathmlToLatex(math, MQLatex.Options(editAt=editPath))
    println("setMath:",tex)
    window.call("setMath",tex)
  }

  private def setSize(w:Double,h:Double) {
    setMinSize(w,h); setPrefSize(w,h); setMaxSize(w,h); resize(w,h)
  }

  private object JSBridge {
    def onMathDeselected() = println("onMathDeselected")
    def onMathSelection(path:String) = println("onMathSelection",path)
    def onResize(w:Double, h:Double) = { Platform.runLater{() => setSize(w+3,h+4)}; println("onResize",w,h) }
    def onMathRendered() = println("onMathRendererd")
    def onLoad() = { println("onLoad"); Platform.runLater{() => loaded = true; if (math!=null) sendMathToJS()} }
    def onEnter(tex:String) = { println("onEnter "+tex); Platform.runLater { () => fireEdited(MQLatex.parseLatex(tex)) } }
//      Platform.runLater(new Runnable {
//        override def run(): Unit = print("RUNNABLE")
//      })
  }
}


private object MathViewMQ {
//  private val base = getClass.getResource("/").toString
  checkResources

  private def checkResources() = {
    def checkResource(name:String) =
      if (getClass.getResource(name)==null) throw new RuntimeException("missing resource "+name)
    checkResource("/mathquill/mathquill.css")
    checkResource("/mathview.html")
    checkResource("/jquery.js")
    checkResource("/mathview.css")
    checkResource("/mathviewmq.js")
  }

//  @Pure
//  private val mathjaxPage : String =
//    s"""
//<html>
//  <head>
//    <meta charset="utf-8" />
//    <base href="$base" />
//    <link href="mathquill/mathquill.css" rel="stylesheet" type="text/css">
//    <link href="mathview.css" rel="stylesheet" type="text/css">
//    <script src="jquery.js"></script>
//    <script src="mathquill/mathquill.js"></script>
//    <script src="mathviewmq.js"></script>
//  </head>
//  <body onload="onLoad()">
//    <span id="tracksize">
//      <span id="formula-span"></span>
//      <object id="tracksize-obj" type="text/html" data="about:blank"></object>
//    </span>
//    <span id="tracksize">
//  </body>
//</html>"""

}