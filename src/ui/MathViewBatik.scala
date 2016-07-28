package ui // TODO: sub package

import java.util

import cmathml.{CMathML, MCNone, MutableCMathMLDocument}
import org.apache.batik.anim.dom.SVGDOMImplementation
import org.apache.batik.swing.svg.JSVGComponent
import org.w3c.dom.svg.SVGDocument

class MathViewBatik extends JSVGComponent {
  val math = new MutableCMathMLDocument(new MCNone())
  private val handledMath = new util.IdentityHashMap[CMathML,Unit]() // TODO: use weak references?
  val svg = SVGDOMImplementation.getDOMImplementation.
    createDocument(SVGDOMImplementation.SVG_NAMESPACE_URI, "svg", null).asInstanceOf[SVGDocument]
  setDocument(svg)
  assert(svg eq getSVGDocument)
  setDocumentState(JSVGComponent.ALWAYS_DYNAMIC)



}
