package tryouts

import java.io.StringReader
import java.util
import javax.print.Doc
import javax.swing.{JApplet, JFrame}

import org.apache.batik.anim.dom.{SAXSVGDocumentFactory, SVGDOMImplementation, SVGOMDocument, SVGOMSVGElement}
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.swing.svg._
import org.apache.batik.util.{XMLConstants, XMLResourceDescriptor}
import org.w3c.dom.{DOMImplementation, Element, Node, NodeList}
import org.w3c.dom.events.{Event, EventListener}
import org.w3c.dom.svg._

import scala.collection.mutable
import scala.xml.{Elem, MinimizeMode, Utility}
import scala.collection.JavaConversions._
import scala.reflect.ClassTag

object Batik {
  var svgComp: JSVGCanvas = null
  var frame: JFrame = null
  val bboxes = new util.IdentityHashMap[SVGLocatable, BBoxHandler]()

  // TODO Weak references?

  final class BBoxHandler(val elem: SVGLocatable) {
    var x: Float = Float.NaN
    var y: Float = Float.NaN
    var width: Float = Float.NaN
    var height: Float = Float.NaN
    var listeners = new mutable.MutableList[() => Unit]
    check() // To initialize x,y,width,height

    def check() = {
      val bbox = elem.getBBox
      if (!(bbox.getWidth == width && bbox.getHeight == height && bbox.getX == x && bbox.getY == y)) {
        x = bbox.getX
        y = bbox.getY
        width = bbox.getWidth
        height = bbox.getHeight
        for (l <- listeners) l()
      }
    }

    def addListener(listener: () => Unit) = listeners += listener
  }

  def addBBoxListener(elem: SVGLocatable, listener: () => Unit): Unit = {
    var handler = bboxes.get(elem)
    if (handler==null) { handler = new BBoxHandler(elem); bboxes.update(elem, handler) }
    handler.addListener(listener)
  }

  def main(args: Array[String]): Unit = {
    frame = new JFrame("Batik")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    svgComp = new JSVGCanvas()
    svgComp.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC)
    frame.add(svgComp)

    val src = <svg xmlns="http://www.w3.org/2000/svg" version="1.1">
      <g id="hbox0" box="hbox">
        <g id="vbox1" box="vbox">
          <rect x="25" y="25" width="200" height="200" fill="lime" stroke-width="4" stroke="pink"/>
          <circle cx="125" cy="-125" r="20" fill="orange"/>
          <polyline points="50,150 50,200 200,200 200,100" stroke="red" stroke-width="4" fill="none"/>
          <line x1="50" y1="50" x2="200" y2="200" stroke="blue" stroke-width="4"/>
        </g>
        <g id="vbox2" box="vbox">
          <rect x="25" y="25" width="200" height="200" fill="lime" stroke-width="4" stroke="pink"/>
          <circle cx="125" cy="125" r="75" fill="orange"/>
          <polyline points="50,150 50,200 200,200 200,100" stroke="red" stroke-width="4" fill="none"/>
          <line x1="50" y1="50" x2="200" y2="200" stroke="blue" stroke-width="4"/>
        </g>
      </g>
    </svg>

    svgComp.setSize(500, 500)

    val xmlParser = XMLResourceDescriptor.getXMLParserClassName
    val svg = SVGDOMImplementation.getDOMImplementation.
      createDocument(SVGDOMImplementation.SVG_NAMESPACE_URI, "svg", null).asInstanceOf[SVGDocument]
//    val svg = new SAXSVGDocumentFactory(xmlParser).createSVGDocument("about:blank",
//      new StringReader(src.toString))

    val rect = svg.createElementNS(SVGDOMImplementation.SVG_NAMESPACE_URI,"rect")
    rect.setAttribute("x","25")
    rect.setAttribute("y","25")
    rect.setAttribute("width","100")
    rect.setAttribute("height","100")
    rect.setAttribute("fill","lime")
    svg.getRootElement.appendChild(rect)

    println(org.apache.batik.dom.util.DOMUtilities.getXML(svg.getRootElement))

    svgComp.addGVTTreeBuilderListener(new GVTTreeBuilderListener {
      override def gvtBuildFailed(e: GVTTreeBuilderEvent): Unit = ???
      override def gvtBuildCancelled(e: GVTTreeBuilderEvent): Unit = ???
      override def gvtBuildCompleted(e: GVTTreeBuilderEvent): Unit = onBuild(frame, svg)
      override def gvtBuildStarted(e: GVTTreeBuilderEvent): Unit = {}
    })

    svgComp.setSVGDocument(svg)
    assert(svg eq svgComp.getSVGDocument)

    svg.asInstanceOf[SVGOMDocument].addEventListenerNS(XMLConstants.XML_EVENTS_NAMESPACE_URI,
      "DOMAttrModified",
      new EventListener {
        override def handleEvent(evt: Event): Unit = {
          println("attr mod", evt, evt.getTarget.asInstanceOf[SVGElement].getId)
        }
      }, true, null)

    frame.setSize(500, 500)
    frame.setVisible(true)
  }

  def registerVBox(vbox: SVGGElement) = {
    val listener = { () => alignVBox(vbox) }
    for (child <- iterate[SVGLocatable](vbox.getChildNodes))
      addBBoxListener(child, listener)
    listener()
  }

  def registerHBox(vbox: SVGGElement) = {
    val listener = { () => alignHBox(vbox) }
    for (child <- iterate[SVGLocatable](vbox.getChildNodes))
    //      if (child.getNodeType == Node.ELEMENT_NODE)
      addBBoxListener(child, listener)
    listener()
  }

  def registerBoxes(elem: SVGElement) : Unit = {
    elem.getAttribute("box") match {
      case "" => {}
      case "vbox" => registerVBox(elem.asInstanceOf[SVGGElement])
      case "hbox" => registerHBox(elem.asInstanceOf[SVGGElement])
    }
    for (child <- iterate[SVGElement](elem.getChildNodes))
      registerBoxes(child)
  }

  def onBuild(frame: JFrame, svg: SVGDocument) = {
    registerBoxes(svg.getRootElement)

    for (i <- 1 to 10)
      inUpdateManager {
        println(s"Polling $i. time")
        pollBBoxes()
      }
  }

  def pollBBoxes(): Unit = {
    for (b <- bboxes.values) {
      b.check()
    }
  }

  def alignHBox(hbox: SVGGElement): Unit = {
    assert(hbox!=null)
    println("Aligning hbox", hbox.getId)
    var first = true
    var x = 0: Float
    for (i <- 0 until hbox.getChildNodes.getLength) {
      val child = hbox.getChildNodes.item(i)
      if (child.getNodeType == Node.ELEMENT_NODE) {
        val bbox = child.asInstanceOf[SVGLocatable].getBBox
        if (first) {
          first = false
          x = bbox.getWidth + bbox.getX
        } else {
          child.asInstanceOf[SVGGElement].setAttribute("transform", s"translate(${x - bbox.getX},0)")
          x += bbox.getWidth
        }
      }
    }
  }

  def alignVBox(vbox: SVGGElement): Unit = {
    assert(vbox != null)
    println("Aligning vbox", vbox.getId)
    var first = true
    var y = 0: Float
    for (child <- iterate[SVGElement](vbox.getChildNodes)) {
      val bbox = child.asInstanceOf[SVGLocatable].getBBox
      if (first) {
        first = false
        y = bbox.getHeight + bbox.getY
      } else {
        child.setAttribute("transform", s"translate(0,${y - bbox.getY})")
        y += bbox.getHeight
      }
    }
  }

  def inUpdateManager(c: => Unit) =
//    try {
      svgComp.getUpdateManager.getUpdateRunnableQueue.invokeLater(new Runnable {
        override def run(): Unit = c
      })
//    } catch {
//      case e => e.printStackTrace()
//    }

  def iterate[A](nodeList: NodeList)(implicit classTag: ClassTag[A]): Traversable[A] =
    new Traversable[A]() {
      override def foreach[U](f: A => U): Unit = {
        for (i <- 0 until nodeList.getLength) {
          val child = nodeList.item(i)
          if (classTag.runtimeClass.isInstance(child))
            f(child.asInstanceOf[A])
        }
      }
    }


}
