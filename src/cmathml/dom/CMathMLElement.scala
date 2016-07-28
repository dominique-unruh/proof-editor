package cmathml.dom

import org.apache.batik.dom._
import org.apache.batik.dom.util.DOMUtilities
import org.w3c.dom.{DOMException, DocumentType, Element}

sealed abstract class MathMLElement protected (name:String, document:CMathMLDocument) extends GenericElement(name,document)
sealed abstract class CMathMLElement protected (name:String, document:CMathMLDocument) extends MathMLElement(name,document)
final case class MathElement protected (document:CMathMLDocument) extends MathMLElement("math",document)
final case class ApplyElement protected[dom] (document:CMathMLDocument) extends CMathMLElement("apply",document)
final case class CIElement protected[dom] (document:CMathMLDocument) extends CMathMLElement("ci",document)
final case class CNElement protected[dom] (document:CMathMLDocument) extends CMathMLElement("cn",document)
final case class CSymbolElement protected[dom] (document:CMathMLDocument) extends CMathMLElement("csymbol",document)
final case class CErrorElement protected[dom] (document:CMathMLDocument) extends CMathMLElement("cerror",document)






