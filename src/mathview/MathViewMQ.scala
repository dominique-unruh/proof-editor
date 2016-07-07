package mathview

import java.lang.Boolean
import javafx.application.Platform
import javafx.beans.property.{ObjectProperty, SimpleObjectProperty}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.{Group, Node}
import javafx.scene.control.DialogPane
import javafx.scene.layout.{BorderPane, Pane}
import javafx.scene.web.WebView
import javafx.stage.Stage

import cmathml.{CMathML, Path, PathRev}
import misc.Pure
import netscape.javascript.JSObject
import misc.Utils.JavaFXImplicits._

import scala.collection.mutable
import misc.Utils.JavaFXImplicits._

/** JavaFX widget for showing and editing math formulas.
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
  private var _editPath = None : Option[Path]
  private var loaded = false
//  web.getEngine.loadContent(MathViewMQ.mathjaxPage)
  web.getEngine.load(getClass.getResource("/mathview.html").toString)
  private val window = web.getEngine.executeScript("window").asInstanceOf[JSObject]
  window.setMember("controller", JSBridge)
  setCenter(web)
  getStyleClass.add("mathview")
  selectedProperty.addListener({(selected:Boolean) =>
    if (selected) getStyleClass.add("selected") else getStyleClass.removeAll("selected"); ()})


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
  def setMath(m:CMathML, path:Option[Path]=None) = {
    assert(Platform.isFxApplicationThread,"not in JavaFX application thread")
    math = m
    _editPath = path
    if (loaded) sendMathToJS()
  }

  /** Whether the editor is selected. Currently, this means that it (or a subelement) has the focus. */
  def selectedProperty = web.focusedProperty

  def getSelection : Option[Path] = {
    assert(Platform.isFxApplicationThread,"not in JavaFX application thread")
    val path = window.call("getSelection").asInstanceOf[String]
    if (path==null) return None
    println("selection path ",path,path.getClass)
    assert(path.startsWith("path-"))
    return Some (Path.fromString(path.substring(5)))
  }

  /** Returns the currently shown math */
  def getMath = math

  /** Returns the path to the currently edited subformula */
  def editPath = _editPath

  /** Must only be called in JavaFX application thread. */
  private def sendMathToJS(): Unit = {
    assert(Platform.isFxApplicationThread,"not in JavaFX application thread")
    val tex = MQLatex.cmathmlToLatex(math, MQLatex.Options(editAt=_editPath map (_.toPathRev)))
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

}