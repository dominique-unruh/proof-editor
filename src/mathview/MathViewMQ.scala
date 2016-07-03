package misc

import javafx.scene.layout.BorderPane
import javafx.scene.web.WebView

import misc._
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

  @Pure
  private def renderList(args:Seq[CMathML],path:PathRev) : Seq[String] =
    args.zipWithIndex.map {case(a,i) => cmathmlToLatex(a,path.append(i+1))}

  @Pure
  private def cmathmlToLatexApplyDefault(hd:String,args:Seq[String]) : String =
    s"$hd(${args.mkString(",")})"

  val applyRenderers : Map[(String,String),Seq[String] => String] = Map(
    ("arith1","plus") -> {case Seq(x,y) => s"{$x}+{$y}"},
    ("arith1","minus") -> {case Seq(x,y) => s"{$x}-{$y}"}
  )

  @Pure
  def addPath(tex:String,path:PathRev) : String = s"\\class{path-$path}{$tex}"

  @Pure
  def cmathmlToLatex(m:CMathML, path:PathRev) : String = m match {
    case CN(n) => addPath(s"\\class{number}{$n}",path)
    case CI(v) => addPath(s"\\class{variable}{$v}",path)
    case CSymbol(cd,name) => s"\\class{symbol}{\\text{$cd.$name}}"
    case Apply(hd,args @ _*) =>
      val renderer = hd match { case CSymbol(cd,name) => applyRenderers.get((cd,name)); case _ => None }
      val renderedArgs = renderList(args,path)
      val result : Option[String] = renderer match { case None => None;
                      case Some(r) => try { Some(r(renderedArgs)) } catch { case _ : MatchError => None } }
      val result2 : String = result match {
        case None => cmathmlToLatexApplyDefault(cmathmlToLatex(hd,path.append(0)),renderedArgs)
        case Some(r) => r }
      addPath(result2,path)
  }
  def cmathmlToLatex(m:CMathML) : String = cmathmlToLatex(m,Path.emptyRev)
}