package ui.mathview

import javafx.application.Platform
import javafx.scene.web.WebView

import cmathml.{CMathML, Path}
import misc.Utils.JavaFXImplicits._
import netscape.javascript.JSObject

import scala.collection.mutable
import scalafx.Includes._
import scalafx.beans.property.ReadOnlyBooleanProperty
import scalafx.scene.layout.BorderPane



/** JavaFX widget for showing and editing math formulas.
  * Math is represented as CMathML objects
  *
  * Must only be created in JavaFX application thread.
  *
  * @see [[setMath]] for setting the displayed math
  * @see [[cmathml.CMathML]] for describing math
  * */
class MathView extends BorderPane {
  // TODO port to ScalaFX

  assert(Platform.isFxApplicationThread,"not in JavaFX application thread")
  private val web = new WebView
  private var math = null : CMathML
  private var _editPath = None : Option[Path]
  private var loaded = false
  /** Whether the editor is selected. Currently, this means that it (or a subelement) has the focus. */
  val selectedProperty : ReadOnlyBooleanProperty = web.focusedProperty
  MathView // Causes the singleton object to be initialized
  web.getEngine.load(MathView.mathviewHTML.toString)
  private val window = web.getEngine.executeScript("window").asInstanceOf[JSObject]
  window.setMember("controller", JSBridge)
  center = web
  styleClass += "mathview"

  selectedProperty.onChange {(_,_,selected) =>
    if (selected) styleClass += "selected" else styleClass.delegate.remove("selected"); ()}

//  selectedProperty.addListener({(selected:Boolean) =>
//    if (selected) getStyleClass.add("selected") else getStyleClass.removeAll("selected"); ()})


  private val editedListeners = mutable.MutableList[CMathML=>Unit]()

  /** The listener is called whenever the user presses ENTER in the editable part of the formula.
    * The editing process is not automatically stopped at that point.
    * To stop editing, call [[setMath]].
    *
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
    minWidth = w
    minHeight = h
    prefWidth = w
    prefHeight = h
    maxWidth = w
    maxHeight = h
//    setMinSize(w,h); setPrefSize(w,h); setMaxSize(w,h);
    resize(w,h)
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


private object MathView {
//  private val base = getClass.getResource("/").toString
//  checkResources
  private val mathviewHTML = checkResource("mathview.html")
  val jQueryJs = checkResource("jquery.js")
  val mathQuillCss = checkResource("mathquill/mathquill.css")
  val mathQuillJs = checkResource("mathquill/mathquill.js")
  val mathViewCss = checkResource("mathview.css")
  val mathViewJs = checkResource("mathview.js")

  private def checkResource(name:String) = {
    val res = getClass.getResource(name)
    if (res == null) throw new RuntimeException("missing resource " + name)
    res
  }
}