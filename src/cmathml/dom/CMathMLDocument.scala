package cmathml.dom

import cmathml._
import org.apache.batik.dom.GenericDocument
import org.apache.batik.dom.util.DOMUtilities
import org.w3c.dom.{DOMException, Element}

/**
  * Created by unruh on 7/24/16.
  */
class CMathMLDocument(implementation: CMathMLDOMImplementation) extends GenericDocument(null, implementation) {
  import CMathMLDocument._
  override def createElementNS(namespaceURI: String, qualifiedName: String): Element = {
    if (CMATHML_NAMESPACE_URI == namespaceURI) {
      val name: String = DOMUtilities.getLocalName(qualifiedName)
      val factory = factories.getOrElse(name,
        throw createDOMException(DOMException.NOT_FOUND_ERR, "invalid.element", Array[AnyRef](namespaceURI, qualifiedName)))
      factory(this)
    } else
      super.createElementNS(namespaceURI, qualifiedName)
  }

  def elementFromCMathML(math:CMathML) : CMathMLElement = math match {
    case Apply(hd, args @ _*) =>
      val elem = new ApplyElement(this)
      elem.appendChild(elementFromCMathML(hd))
      for (child <- args)
        elem.appendChild(elementFromCMathML(child))
      elem
    case CI(v) =>
      val ci = new CIElement(this)
      ci.appendChild(createTextNode(v))
      ci
    case CN(n) =>
      val cn = new CNElement(this)
      cn.appendChild(createTextNode(n.toString))
      cn
    case CSymbol(cd, name) =>
      val sym = new CSymbolElement(this)
      sym.setAttribute("cd",cd)
      sym.appendChild(createTextNode(name))
      sym
    case CError(cd, name, args @ _*) =>
      val err = new CErrorElement(this)
      err.appendChild(elementFromCMathML(CSymbol(cd,name)))
      for (a <- args)
        err.appendChild(elementFromCMathML(a.asInstanceOf[CMathML]))
      err
    case CNone() => ???
  }
}

object CMathMLDocument {
  val CMATHML_NAMESPACE_URI = "http://www.w3.org/1998/Math/MathML"
  private val factories = Map(
    "math" -> { MathElement(_) },
    "apply" -> { ApplyElement(_) },
    "ci" -> { CIElement(_) },
    "cn" -> { CNElement(_) },
    "csymbol" -> { CSymbolElement(_) },
    "cerror" -> { CErrorElement(_) }
  )
}