package trafo

import cmathml.CN
import test.UnitSpec
import theory.{Formula, Theory}

class TransformationTest extends UnitSpec {
  test("EditFormulaTrafo") {
    val inter = new EditFormulaTrafo().createInteractive
    val thy0 = Theory()
    val (thy1,f1)= thy0.addFormula(Formula(CN(1)))
//    var (thy2,f2)= thy1.addFormula(Formula(CN(1)))
    val f2 = CN(0)+CN(1)
    println(f1.id)
    val (trafoInst,msgs) = inter.quickInteract(Some(f1),f2)
//    assert(msgs.isEmpty)
    assert(trafoInst.isInstanceOf[EditFormulaTrafo.Instance])
    assert(trafoInst.isValid)
    assertResult( List(CN(1), CN(0)+CN(1)) ) { trafoInst.formulas map (_.math) }
  }
}
