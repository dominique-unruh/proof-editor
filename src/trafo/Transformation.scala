package trafo

import cmathml.{CMathML, Logic}
import relation.Relation
import theory.Formula

import scala.collection.immutable.Vector
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.xml.Elem

/** A [[Transformation]] models a way how one gets formulas from other formulas.
  * Each [[Transformation]]-class is associated with [[TrafoInstance]]-classes which encode particular instantiations of the transformation.
  *
  */
abstract class Transformation {
  def createInteractive : Interaction[TrafoInstance]
}

/** Immutable class that represents the relationship between various formulas.
  * The precise relationship depends on the subclass of [[TrafoInstance]], but
  * it must only refer to the formulas listed in [[TrafoInstance.formulas]].
  */
abstract class TrafoInstance {
  def update(id: Int, formulas: Seq[Formula]) : TrafoInstance
  val id: Int
  val formulas : IndexedSeq[Formula]
  val isValid  : Boolean
  val relation : Relation
  /** Returns as a mathemtical formula the relation expressed by this TrafoInstance */
  def relationFormula : CMathML =
  Logic.instantiateLambda(relation.relatingFormula, formulas.map(_.math) :_*)
  def toXML : Elem

  /** A short human-readable description (name) of this trafo / trafo instance.
    * E.g., "modus ponens"
    */
  val shortDescription : String
}
object TrafoInstance {
  private val trafoXMLParsers = mutable.HashMap[String,Elem => TrafoInstance] (
    "modusPonens" -> ModusPonensTrafo.fromXML,
    "trivial" -> TrivialTrafo.fromXML,
    "editFormula" -> EditFormulaTrafo.fromXML
  )
  def fromXML(xml: Elem) = trafoXMLParsers.synchronized {
    trafoXMLParsers.get(xml.label) match {
      case Some(parser) => parser(xml)
      case _ => sys.error(s"Don't know how to parser transformations <${xml.label}>")
    }
  }

  def addXMLParser(tagName : String)(parser : Elem => TrafoInstance) = trafoXMLParsers.synchronized {
    assert(!trafoXMLParsers.contains(tagName))
    trafoXMLParsers.update(tagName,parser)
  }
}