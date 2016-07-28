package cmathml.dom

import org.apache.batik.dom.GenericDOMImplementation
import org.w3c.dom.{DOMException, DocumentType}

class CMathMLDOMImplementation extends GenericDOMImplementation {
  @throws[DOMException]
  override def createDocument(namespaceURI: String, qualifiedName: String, doctype: DocumentType): CMathMLDocument = {
    val result = new CMathMLDocument(this)
    if (qualifiedName != null) result.appendChild(result.createElementNS(namespaceURI, qualifiedName))
    return result
  }
}
