package trafo

import cmathml.CN
import test.UnitSpec
import theory.{Formula, Theory}

class TransformationTest extends UnitSpec {
  test("IdentityTransformation") {
    val inter = new IdentityTransformation().createInteractive
    var thy0 = Theory()
    var (thy1,f1)= thy0.addFormula(Formula(CN(1)))
    var (thy2,f2)= thy1.addFormula(Formula(CN(1)))
    println(f1.id, f2.id)
    val (trafoInst,msgs) = inter.quickInteract(Some(f1),Some(f2))
    assert(msgs.isEmpty)
    assert(trafoInst.isInstanceOf[IdentityTrafoInstance])
    assert(trafoInst.isValid)
    assertResult( List(CN(1), CN(1)) ) { trafoInst.formulas map (_.math) }
  }
}
