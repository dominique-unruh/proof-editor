package theory

import cmathml.{CMathML, Path}
import misc.{Log, Utils}
import relation._
import theory.CaseDistinctionCoverage.allDone
import theory.Formula.{Axiom, Property, Proven}
import theory.Theory.{FormulaId, NO_ID, NO_T_ID, TrafoId}
import trafo.TrafoInstance

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.existentials
import scala.reflect.runtime.universe._
import scala.xml.{Attribute, Comment, Elem, UnprefixedAttribute}

// TODO Should have a private constructor (and private copy/apply methods)
final case class Theory(counter : Int,
                        /** Invariants:
                          * - for any (i->f) in this map, f.id==i.
                          * - for any (i->f), i<[[Theory!.counter counter]] */
                        formulas : Map[FormulaId,Formula],
                        transformations : Map[TrafoId,TrafoInstance]
                       ) {
  def deleteFormula(formula: Formula) : (Theory,Formula) = {
    val form2 = formulas(formula.id)
    val thy2 = copy(formulas = formulas - formula.id)
    (thy2,form2)
  }

  def toXML = {
    def formulaNL(f:Formula) = Seq(f.toXML,scala.xml.Text("\n"))
    def trafoNL(t:TrafoInstance) = Seq(t.toXML,scala.xml.Text("\n"))
    val sortedFormulas = formulas.values.toSeq.sortBy(_.id)
    val sortedTrafos = transformations.values.toSeq.sortBy(_.id)
    <theory xmlns="http://unruh.de/proof-editor" counter={counter.toString}>
      <formulas>
        {sortedFormulas.flatMap(formulaNL)}
      </formulas>
      <transformations>
        {sortedTrafos.flatMap(trafoNL)}
      </transformations>
    </theory>
  }

  def addAxiom(formula:Formula) : (Theory,Formula) = {
    addFormula(formula.setProperties(Axiom->true, Proven->allDone))
  }

  private def addFormula(formula:Formula) : (Theory,Formula) = {
    assert(formula.id==NO_ID)
    val id = new FormulaId(counter)
    val formula2 = formula.copy(id=id)
    val thy = copy(counter = counter+1, formulas = formulas.updated(id, formula2))
    (thy, formula2)
  }

  def addTrafoInstance(trafo: TrafoInstance) : (Theory,TrafoInstance,Seq[Formula]) = {
    assert(trafo.id==NO_T_ID)

    val formulas = trafo.formulas
    var theory = this
    val trafoId = new TrafoId(theory.counter)
    theory = theory.copy(counter=theory.counter+1)
    val mappedFormulas = ArrayBuffer(formulas :_*)

    // Load existing formulas from theory / add id's to new ones
    mappedFormulas.transform { f =>
      if (f.id==NO_ID) {
        val (thy,f2) = theory.addFormula(f)
        theory = thy
        f2
      } else {
        val f2 = theory.formulas(f.id)
        assert(f2.logicallyEquivalent(f))
        f2
      }
    }

    // Find out which formulas have been proven (can probably be done in some more general way?)
    val premises = trafo.relation match {
      case Implication(prems,_) => mappedFormulas.slice(0,prems)
      case Trivial => Nil
      case Equality => List(mappedFormulas.head)
      case OneOf(_) => mappedFormulas
    }
    val premisesProven = premises.foldLeft(allDone) { (cases, f) => cases.intersection(f(Proven)) }
    mappedFormulas.transform { f =>
      if (!premises.exists(_.id==f.id))
        f.setProperty(Proven,premisesProven.union(f(Proven)).normalForm)
      else
        f
    }

    // Mark formulas as case distinction
    if (trafo.relation.isInstanceOf[OneOf]) {
      val ids = mappedFormulas.toList.map(_.id)
      mappedFormulas.transform { f =>
        f.setProperty(Proven, CaseDistinctionCoverage.oneOf(trafoId,ids,f.id))
      }
    }

    // Put changed back into theory
    for (f <- mappedFormulas) {
      assert(f.id!=NO_ID)
      val (thy,_) = theory.updateFormula(f)
      theory = thy
    }

    val newFormulas = mappedFormulas.toList.filter(f => !this.formulas.contains(f.id))

    val trafo2 = trafo.update(trafoId,mappedFormulas)
    theory = theory.copy(transformations=transformations.updated(trafoId,trafo2))

    (theory,trafo2,newFormulas)
  }

  def isMember(formula:Formula) : Boolean = {
    val own = formulas.get(formula.id)
    own.isDefined && own.get == formula
  }

  /** Replaces a formula stored in the theory by a new formula.
    * The formula to be replaced is specified by the id of the new formula.
    *
    * @param formula th new formula
    * @return (thy,oldFormula): thy is the new theory, oldFormula is the formula that was replaced,
    *         newFormula is the just added formula (newFormula may or may not be equal to the parameter formula,
    *         but the logical content is guaranteed to be the same)
    *
    *  TODO version of this that is public (with restrictions on the change done to the formula)
    */
  private[Theory] def updateFormula(formula:Formula) : (Theory,Formula) = {
    val oldFormula = formulas.getOrElse(formula.id, throw new IllegalArgumentException("trying to update non-existing formula"))
    val thy2 = copy(formulas = formulas.updated(formula.id, formula))
    (thy2,oldFormula)
  }
}
object Theory {
  def apply() : Theory = Theory(0,Map.empty,Map.empty)
  final case class FormulaId(id:Int) extends AnyVal with Ordered[FormulaId] {
    override def compare(that: FormulaId): Int = id compare that.id
    override def toString = id.toString
  }
  object FormulaId {
    def apply(str: String) = new FormulaId(str.toInt)
  }
  final case class TrafoId(id:Int) extends AnyVal with Ordered[TrafoId] {
    override def compare(that: TrafoId): Int = id compare that.id
    override def toString = id.toString
  }
  object TrafoId {
    def apply(str: String) = new TrafoId(str.toInt)
  }
  val NO_ID : FormulaId = FormulaId(-1)
  val NO_T_ID : TrafoId = TrafoId(-1)
  def fromXML(xml:Elem) : Theory = {
    assert(xml.label=="theory")
    val formulas = Map[FormulaId,Formula]((xml \ "formulas" \ "formula").map { x => val f = Formula.fromXML(x.asInstanceOf[Elem]); f.id -> f } : _*)
    val trafos = Map[TrafoId,TrafoInstance]((xml \ "transformations" \ "_").map { x => val t = TrafoInstance.fromXML(x.asInstanceOf[Elem]); t.id -> t } : _*)
    val counter = xml.attribute("counter").get.text.toInt
    val thy = new Theory(counter,formulas,trafos)
    for (t <- trafos.values; f <- t.formulas)
      assert(thy.formulas.contains(f.id))
    thy
  }
}

final case class Formula private[theory] (id : FormulaId = NO_ID,
                                          properties : Map[Property[_],Any] = Map.empty,
                                          math : CMathML) {
  def logicallyEquivalent(f: Formula): Boolean = this==f

  import Formula._
  def detach: Formula = copy(id=NO_ID)
  def toXML = {
    val propXML = for {
      (prop, value) <- properties // .asInstanceOf[Iterable[(Property[T],T) forSome {type T}]]
      name : String = prop.name
      text : String = prop.asInstanceOf[Property[Any]].toText(value)
    } yield new UnprefixedAttribute(name, text, scala.xml.Null)
    val tag = <formula id={id.toString}>{Comment(" "+math.toPopcorn+" ")}{math.toXMLMath}</formula>
    propXML.foldLeft(tag)(_%_)
  }
  def apply[T](prop:Property[T]) : T = properties.getOrElse(prop,prop.default).asInstanceOf[T]
  def setProperty[T](prop:Property[T], value:T) : Formula =
  if (apply(prop)==value) this
    else if (prop.default==value) {
      val props = properties - prop
      if (props eq properties) this
      else this.copy(properties=props)
    } else
      this.copy(properties=properties.updated(prop,value))
  def setProperties(keyvals: ((Property[T], T) forSome {type T})*): Formula =
    keyvals.foldLeft(this) { (f,kv) => kv match { case (k,v) => f.setProperty(k,v) } }
}

object Formula {
  sealed abstract class Property[T](val default : T)(implicit val valueType : TypeTag[T]) {
    val name = Utils.lowerFirst(getClass.getSimpleName.stripSuffix("$"))
    @inline protected def toText$(value : Boolean) : String = value.toString
    def toText(value : T) : String
    def fromText(text: String) : T
    def humanReadable(value: T): Elem = <span>{toText(value)}</span>
  }
  private val properties = mutable.HashMap[String,Property[_]](findProperties : _*)

  private def findProperties : Seq[(String,Property[_])] = {
    val universeMirror = runtimeMirror(getClass.getClassLoader)
    for { prop <- typeOf[Property[_]].typeSymbol.asClass.knownDirectSubclasses.toSeq
          module = prop.asClass.module.asModule
          instance = universeMirror.reflectModule(module).instance.asInstanceOf[Property[_]] }
      yield instance.name -> instance
  }

  case object Axiom extends Property[Boolean](default=false) {
    override def toText(value: Boolean): String = value.toString
    override def fromText(str: String) = str.toBoolean
  }
//  case object Proven extends Property[Boolean](default=false) {
//    override def toText(value: Boolean): String = value.toString
//    override def fromText(str: String) = str.toBoolean
//  }
//  case object CaseDistinction extends Property[CaseDistinctionCoverage](default=CaseDistinctionCoverage.allDone) {
//    override def toText(value: CaseDistinctionCoverage): String = value.toText
//    override def fromText(text: String): CaseDistinctionCoverage = CaseDistinctionCoverage.fromText(text)
//  }
  case object Proven extends Property[CaseDistinctionCoverage](default=CaseDistinctionCoverage.nothingDone) {
      override def toText(value: CaseDistinctionCoverage): String = value.toText
      override def fromText(text: String): CaseDistinctionCoverage = CaseDistinctionCoverage.fromText(text)
  override def humanReadable(value : CaseDistinctionCoverage) = value.humanReadable
  }
  def apply(math : CMathML) = new Formula(id=NO_ID, math=math)

  def fromXML(xml:Elem) : Formula = {
    assert(xml.label=="formula")
    val id = FormulaId(xml.attribute("id").get.text)

    val props = for {
      attr <- xml.attributes.toSeq if attr.key!="id"
      prop = properties(attr.key)
      valueStr = attr.value.map(_.text).mkString
      value = prop.fromText(valueStr)
    } yield prop -> value

    val math = CMathML.fromXML((xml \ "math").head.asInstanceOf[Elem])
    new Formula(id=id,math=math, properties=Map(props:_*))
  }
}


