package theory

import misc.{Log, Utils}
import theory.CaseDistinctionCoverage._
import theory.Theory.{FormulaId, NO_ID, TrafoId}

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer
import scala.xml.Elem

final case class CaseDistinctionCoverage(splits : SplitList, coveredCases : List[Case])
{
  def toText: String =
      s"${splits.toText}#${coveredCases.map(_.toText).mkString(";")}"
  override def toString = s"CaseDistinctionCoverage($toText)"

  private def factorOut(split: Split) : Option[CaseDistinctionCoverage] = {
    val idx = splits.splits.indexOf(split)
    assert(idx>=0)
    val cases = split.cases
    def caseToIdx(f:FormulaId) = cases.indexOf(f)
    val projections = (1 to cases.length).map(_ => mutable.Set[Case]()).toArray
    for (cas <- coveredCases) {
      val buff = cas.formulas.to[ListBuffer]
      val caseIdx = caseToIdx(buff.remove(idx))
      projections(caseIdx) += Case(buff.toList)
    }
    for (i <- 1 until projections.length)
      if (projections(i)!=projections(0)) return None
    val puncturedSplits = SplitList({ val b = splits.splits.to[ListBuffer]; b.remove(idx); b.toList})
    val projectedCases = projections(0).toList
    Some(CaseDistinctionCoverage(puncturedSplits,projectedCases))
  }

  def normalForm = {
    var result = this
    for (split <- splits.splits)
      for (newResult <- result.factorOut(split))
        result = newResult
    result
  }

  def humanReadable: Elem = {
    def caseToStr(cas:Case) =
      "(" + cas.formulas.map(_.toString).mkString(",") + ")"
    if (isAllDone) <span>all cases</span>
    else if (isNothingDone) <span>no case</span>
    else
      <span>case{if (coveredCases.length>1) "s" else ""} {coveredCases.map(caseToStr).mkString(" and ")}</span>
  }

  /**
    * @param splits (must be a superset of this.splits)
    * @return A copy of this, with this.splits set to splits, but logically equivalent to this
    */
  def extendTo(splits : SplitList) : CaseDistinctionCoverage = {
    if (this.coveredCases.isEmpty) return CaseDistinctionCoverage(splits,Nil)
    // Since splits must be a superset of this.splits, this texts for equality (but faster)
    if (this.splits.splits.length==splits.splits.length) return this
    var covered = coveredCases.map(_.asMap(this.splits))
    for (s <- splits.splits if !this.splits.splits.exists(_.trafo==s.trafo)) {
      covered = for {
        cas <- covered
        newCase <- s.cases
      } yield cas.updated(s.trafo,newCase)
    }
    val covered2 = for (cas <- covered) yield Case(splits.splits.map(s => cas(s.trafo)))
    CaseDistinctionCoverage(splits,covered2)
  }

  /**
    * Note: does not recognize if this is logically equivalent to allDone (to check this, normalize first (TODO: implement))
    * @return whether this is allDone
    */
  def isAllDone = splits.splits.isEmpty && coveredCases.nonEmpty

  /**
    * @return whether this is logically equivalent to nothingDone
    */
  def isNothingDone = coveredCases.isEmpty

  def intersection(that: CaseDistinctionCoverage) =
    if (this.isAllDone) that
    else if (that.isAllDone) this
    else if (this.isNothingDone) nothingDone
    else if (that.isNothingDone) nothingDone
    else {
      val joinSplits = this.splits.union(that.splits)
      val this2 = this.extendTo(joinSplits)
      val that2 = that.extendTo(joinSplits)
      val intersectedCases = this2.coveredCases.toSet.intersect(that2.coveredCases.toSet).toList
      CaseDistinctionCoverage(joinSplits,intersectedCases)
    }

  def union(that: CaseDistinctionCoverage) =
    if (this.isAllDone) allDone
    else if (that.isAllDone) allDone
    else if (this.isNothingDone) that
    else if (that.isNothingDone) this
    else {
      val joinSplits = this.splits.union(that.splits)
      val this2 = this.extendTo(joinSplits)
      val that2 = that.extendTo(joinSplits)
      val unionCases = (this2.coveredCases.toSet ++ that2.coveredCases).toList
      CaseDistinctionCoverage(joinSplits,unionCases)
    }
}

object CaseDistinctionCoverage {
  /**
    * @param trafoId
    * @param cases
    * @param thisCase (must be element of [[cases]])
    * @return (in normal form)
    */
  def oneOf(trafoId: TrafoId, cases: Seq[FormulaId], thisCase: FormulaId): CaseDistinctionCoverage =
    if (cases.length==1) allDone
    else CaseDistinctionCoverage(
      SplitList(Split(trafoId,cases.toList)),
      List(Case(thisCase))
    )

  def fromText(text: String) : CaseDistinctionCoverage = {
    val (splitsTxt,coveredCasesTxt) = Utils.splitString2(text,'#')
    val splits = SplitList.fromText(splitsTxt)
    val coveredCases = coveredCasesTxt.split(';').toList.map(Case.fromText)
    CaseDistinctionCoverage(splits,coveredCases)
  }

  /** Refers to a transformation that did a case distinction, and lists all the different cases
    * (by referring to the formulas arising from those cases).
    * We assume that all splits within one theory that have the same trafo have also the same cases (in the same order) */
  final case class Split(trafo: TrafoId, cases: immutable.Seq[FormulaId]) {
    def toText = s"$trafo:${cases.mkString(",")}"
  }

  object Split {
    def fromText(text: String): Split = {
      val (trafoTxt,casesTxt) = Utils.splitString2(text,':')
      val trafo = TrafoId(trafoTxt)
      val cases = casesTxt.split(',').toList.map(FormulaId(_))
      Split(trafo,cases)
    }
  }

  /** A SplitList never has two [[Split]]s with the same trafoId */
  final case class SplitList(splits : List[Split]) extends AnyVal {
    def toText = splits.map(_.toText).mkString(";")
    def union(that: SplitList) : SplitList =
      if (this.splits.isEmpty) that
      else if (that.splits.isEmpty) this
      else if (this==that) this
      else SplitList((this.splits.toSet ++ that.splits).toList)
    override def toString = s"SplitList($toText)"
  }
  object SplitList {
    val empty = SplitList(Nil)
    def apply(splits : Split*) : SplitList = SplitList(splits.toList)

    def fromText(text: String) =
      if (text.isEmpty) SplitList.empty
      else SplitList(text.split(';').toList.map(Split.fromText))
  }

  /** Refers to a combination of cases for a list of case distinctions (Seq[Split]).
    * [[formulas]] is ordered according to that list of cases. */
  final case class Case(formulas: List[FormulaId]) extends AnyVal {
    def asMap(splits: SplitList): Map[TrafoId,FormulaId] = {
      assert(splits.splits.length == formulas.length)
      Map( splits.splits.zip(formulas).map {case (s,f) => s.trafo->f} :_*)
    }

    def toText = formulas.mkString(",")
  }
  object Case {
    def fromText(text: String) : Case =
      if (text.isEmpty) Case(Nil)
      else Case(text.split(',').toList.map(FormulaId(_)))

    def apply(formula:FormulaId) : Case = Case(List(formula))
  }

  val allDone = CaseDistinctionCoverage(SplitList.empty,List(Case(Nil)))
  val nothingDone = CaseDistinctionCoverage(SplitList.empty,Nil)
}


