package cmathml

import test.UnitSpec

import cmathml.CMathML._

class MutableCMathMLTest extends UnitSpec {

  test("roundTrip") {
    val expressions = List(
      plus(CN(1),CN(2)),
      CI("x"),
      equal(CI("x"),CN(2)),
      CN(1.23),
      divide(CN(2),CN(2)),
      minus(CN(3),CN(2)),
      times(minus(CN(4),CN(2)),CN(1.2)),
      uminus(CI("x")),
      uminus(CN("123")),
      CN(-234)
    )

    for (e<-expressions) {
      val root = new MutableCMathMLDocument(e)
      assert(root.toCMathML==e)
    }
  }

  test("globalChangeListener") {
    val e1 = MCSymbol("x","y")
    val e2 = new MApply(e1,new MCN(1),new MCN(2))
    val doc = new MutableCMathMLDocument(e2)
    var seen = false
    doc.addGlobalChangeListener{m => assert(m eq e1); seen = true }
    doc.addChangeListener(() => assert(false))
    e1.name = "fritz"
    assert(seen)
  }

  test("subterm") {
    for ((m,p,s) <- CMathMLTest.subtermTests) {
      val mm = MutableCMathML.fromCMathML(m)
      val ms = mm.subterm(p)
      val cs = ms.toCMathML
      assert(cs==s)
    }
  }

  test("getPath") {
    for (((m, p, s),i) <- CMathMLTest.subtermTests.zipWithIndex) {
      val mm = new MutableCMathMLDocument(m)
      val ms = mm.subterm(p)
      val p2 = ms.getPath
      assert(p2 == p,(i,m,s))
    }
  }
}
