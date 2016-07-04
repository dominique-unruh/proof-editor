package misc

import javafx.scene.layout.BorderPane
import javafx.scene.web.WebView

import mathview.MQLatexParser.{MathContext, PlusContext}
import netscape.javascript.JSObject
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.{AbstractSeq, mutable}
import scala.xml.{Elem, Node, Null, UnprefixedAttribute}

class MathView(math:String) extends BorderPane {
  private val web = new WebView
  installBridge()
  println(MathView.mathjaxPage(MathView.base,math))
  web.getEngine.loadContent(MathView.mathjaxPage(MathView.base,math))
  setCenter(web)

  def installBridge() = {
    val window = web.getEngine.executeScript("window").asInstanceOf[JSObject]
    window.setMember("controller", JSBridge)
  }

  object JSBridge {
    def onMathDeselected() = println("onMathDeselected")
    def onMathSelection(path:String) = println("onMathSelection",path)
    def onMathRendered() = println("onMathRendered")
  }
}

object MathView {
  val base = getClass.getResource("/").toString

  @Pure
  def mathjaxPage(base:String,math:String): String = {
    s"""
<html>
  <head>
    <meta charset="utf-8" />
    <base href="$base" />
    <link href="mathview.css" rel="stylesheet" type="text/css">
    <script src='mathview.js'></script>
    <script src='mathjax/MathJax.js'></script>
  </head>
  <body onload="onLoad()">
    <span style="font-size: 0.75cm" id="formula-span">
      <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">$math</math>
    </span>
    <span id="selection-rect"></span>
  </body>
</html>"""
  }

  @Pure
  private def renderList(args:Seq[CMathML],path:PathRev) : Seq[Elem] =
    args.zipWithIndex.map {case(a,i) => cmathmlToMathjaxXML(a,path.append(i+1))}

  @Pure
  private def cmathmlToMathjaxApplyDefault(hd:Elem,args:Seq[Elem]) : Elem =
    <mrow class="apply">
      {hd}
      <mo form="infix">&ApplyFunction;</mo>
      <mrow>
        <mo fence="true">(</mo>
        {Utils.intersperse(args, <mo separator="true">,</mo>)}
        <mo fence="true">)</mo>
      </mrow>
    </mrow>

  val applyRenderers : Map[(String,String),Seq[Elem] => Elem] = Map(
    ("arith1","plus") -> {case Seq(x,y) => <mrow class="apply">{x}<mo class="leaf">+</mo>{y}</mrow>},
    ("arith1","minus") -> {case Seq(x,y) => <mrow class="apply">{x}<mo class="leaf">-</mo>{y}</mrow>}
  )

  @Pure
  def cmathmlToMathjaxXML(m:CMathML, path:PathRev) : Elem = m match {
    case CN(n) => <mn class="leaf number" path={path.toString()}>{n}</mn>
    case CI(v) => <mi class="leaf variable" path={path.toString()}>{v}</mi>
    case CSymbol(cd,name) => <mi class="leaf symbol" path={path.toString()}>{cd}.{name}</mi>
    case Apply(hd,args @ _*) =>
      val renderer = hd match { case CSymbol(cd,name) => applyRenderers.get((cd,name)); case _ => None }
      val renderedArgs = renderList(args,path)
      val result : Option[Elem] = renderer match { case None => None;
                      case Some(r) => try { Some(r(renderedArgs)) } catch { case _ : MatchError => None } }
      val result2 : Elem = result match {
        case None => cmathmlToMathjaxApplyDefault(cmathmlToMathjaxXML(hd,path.append(0)),renderedArgs)
        case Some(r) => r }
      result2 % new UnprefixedAttribute("path",path.toString(),Null)
  }
  def cmathmlToMathjax(m:CMathML) : String = scala.xml.Utility.trim(cmathmlToMathjaxXML(m,Path.emptyRev)).toString()
}