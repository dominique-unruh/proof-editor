package trafo

import cmathml.{CMathML, Logic}
import relation.Relation
import theory.Formula

import scala.collection.immutable.Vector

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
  val formulas : IndexedSeq[Formula]
  val isValid  : Boolean
  val relation : Relation
  /** Returns as a mathemtical formula the relation expressed by this TrafoInstance */
  def relationFormula : CMathML =
  Logic.instantiateLambda(relation.relatingFormula, formulas.map(_.math) :_*)
}
