package theory

import cmathml.{CMathML, Path}
import misc.{Log, Utils}
import theory.Formula.Property
import theory.Theory.NO_ID
import trafo.TrafoInstance

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.xml.{Attribute, Comment, Elem, UnprefixedAttribute}

// TODO Should have a private constructor (and private copy/apply methods)
final case class Theory(counter : Int,
                        /** Invariants:
                          * - for any (i->f) in this map, f.id==i.
                          * - for any (i->f), i<[[Theory!.counter counter]] */
                        formulas : Map[Int,Formula],
                        transformations : Map[Int,TrafoInstance]
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

  def addFormula(formula:Formula) : (Theory,Formula) = {
    assert(formula.id==NO_ID)
    val formula2 = formula.copy(id=counter)
    val thy = copy(counter = counter+1, formulas = formulas.updated(counter, formula2))
    (thy, formula2)
  }

  def addTrafoInstance(trafo: TrafoInstance) : (Theory,TrafoInstance,Seq[Formula]) = {
    assert(trafo.id==NO_ID)

    val formulas = trafo.formulas
    var theory = this
    val trafoCounter = theory.counter
    theory = theory.copy(counter=trafoCounter+1)
    var mappedFormulas = ListBuffer() : ListBuffer[Formula]
    var newFormulas = ListBuffer() : ListBuffer[Formula]

    for (f <- formulas) {
      if (f.id==NO_ID) {
        val (thy, newFormula) = theory.addFormula(f)
        newFormulas += newFormula
        mappedFormulas += newFormula
        theory = thy
      } else {
        assert(isMember(f))
        mappedFormulas += f
      }
    }

    val trafo2 = trafo.update(trafoCounter,mappedFormulas)
    theory = theory.copy(transformations=transformations.updated(trafoCounter,trafo2))

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
    * @return (thy,newFormula,oldFormula): thy is the new theory, oldFormula is the formula that was replaced,
    *         newFormula is the just added formula (newFormula may or may not be equal to the parameter formula,
    *         but the logical content is guaranteed to be the same)
    */
  def updateFormula(formula:Formula) : (Theory,Formula,Formula) = {
    val oldFormula = formulas.getOrElse(formula.id, throw new IllegalArgumentException("trying to update non-existing formula"))
    val thy2 = copy(formulas = formulas.updated(formula.id, formula))
    (thy2,formula,oldFormula)
  }
}
object Theory {
  def apply() : Theory = Theory(0,Map.empty,Map.empty)
  val NO_ID = -1
  def fromXML(xml:Elem) : Theory = {
    assert(xml.label=="theory")
    val formulas = Map[Int,Formula]((xml \ "formulas" \ "formula").map { x => val f = Formula.fromXML(x.asInstanceOf[Elem]); f.id -> f } : _*)
    val trafos = Map[Int,TrafoInstance]((xml \ "transformations" \ "_").map { x => val t = TrafoInstance.fromXML(x.asInstanceOf[Elem]); t.id -> t } : _*)
    val counter = xml.attribute("counter").get.text.toInt
    val thy = new Theory(counter,formulas,trafos)
    for (t <- trafos.values; f <- t.formulas)
      assert(thy.isMember(f))
    thy
  }
}

final case class Formula private[theory] (id : Int = NO_ID,
                                          properties : Map[Property[_],Any] = Map.empty,
                                          math : CMathML) {
  import Formula._
  def detach: Formula = copy(id=NO_ID)
  def toXML = {
    val propXML = for {
      (prop, value) <- properties
      name : String = prop.name
      text : String = prop.asInstanceOf[Property[Any]].toText(value)
    } yield new UnprefixedAttribute(name, text, scala.xml.Null)
    val tag = <formula id={id.toString}>{Comment(" "+math.toPopcorn+" ")}{math.toXMLMath}</formula>
    propXML.foldLeft(tag)(_%_)
  }
  def apply[T](prop:Property[T]) : T = properties.getOrElse(prop,prop.default).asInstanceOf[T]
  def setProperty[T](prop:Property[T], value:T) : Formula = {
    if (apply(prop)==value) this
    else if (prop.default==value) {
      val props = properties - prop
      if (props eq properties) this
      else this.copy(properties=props)
    } else
      this.copy(properties=properties.updated(prop,value))
  }
}

object Formula {
  sealed abstract class Property[T](val default : T) {
    val name = Utils.lowerFirst(getClass.getSimpleName.stripSuffix("$"))
    @inline protected def toText$(value : Boolean) : String = value.toString
    def toText(value : T) : String
    def fromText(text: String) : T
    type ValueType = T
  }
  /*private */val properties = mutable.HashMap[String,Property[_]](findProperties : _*)

  private def findProperties : Seq[(String,Property[_])] = {
    val universeMirror = runtimeMirror(getClass.getClassLoader)
    for { prop <- typeOf[Property[_]].typeSymbol.asClass.knownDirectSubclasses.toSeq
          module = prop.asClass.module.asModule
          instance = universeMirror.reflectModule(module).instance.asInstanceOf[Property[_]] }
      yield instance.name -> instance
  }
//  val tt = weakTypeTag[Property[_]]
//  val tt2 = typeOf[Property[_]].typeSymbol.asClass.knownDirectSubclasses

  case object Axiom extends Property[Boolean](default=false) {
    override def toText(value: Boolean): String = value.toString
    override def fromText(str: String) = str.toBoolean
  }
  def apply(math : CMathML) = new Formula(id=NO_ID, math=math)

  def fromXML(xml:Elem) : Formula = {
    assert(xml.label=="formula")
    val id = xml.attribute("id").get.text.toInt

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

//case class FormulaRef(val id : Int, val formula : Formula) {
//  def math = formula.math
//}
