package cmathml

import test.UnitSpec
import CMathML._
import misc.Log
import _root_.z3.Z3

class CMathMLTest extends UnitSpec {
  test("CMathML.replace") {
    val m = CN(1) / CN(2)
    val p = Path(1)
    assertResult(CN(99) / CN(2)) {
      m.replace(p,CN(99))
    }
  }

  test("CMathML.subterm") {
    for ((m,p,s) <- CMathMLTest.subtermTests)
      assert(m.subterm(p)==s)
  }

  /** A few dummy lines to achieve higher test coverage */
  test("missing-coverage") {
//    assertResult(Some(BigDecimal(1,CN.MATHCONTEXT))) { CN.unapply(CN(1)) }
  }

  test("CN.isNegative") {
    assert(!CN(1).isNegative)
    assert(!CN(0).isNegative)
    assert(CN(-1).isNegative)
  }

  test("XML") {
    assertResult("<ci>x</ci>") { CI("x").toXML.toString }
    assertResult("""<csymbol cd="arith1">times</csymbol>""") { arith1.times.toXML.toString }
    assertResult("""<cn type="integer">123</cn>""") { CN(123).toXML.toString }
    assertResult("""<cn type="real">0.5</cn>""") { CN(0.5).toXML.toString }
  }

  test("fromPopcorn whitespace") {
    assert( CMathML.fromPopcorn("$x + $y") == CI("x")+CI("y") )
    assert( CMathML.fromPopcorn(" 1 ") == CN(1) )
    assert( CMathML.fromPopcorn("""" 1 """") == CS(" 1 ") )
  }

  test("fromPopcorn") {
    val x = CMathML.fromPopcorn("0f3DDB7CDFD9D7BDBB")
    assert(x==CN(1e-10))
  }

  test("xml roundtrip") {
    for (e <- CMathMLTest.cMathMLRoundtrips) {
      val x = e.toXML
      val e2 = CMathML.fromXML(x)
      assertResult(e) { e2 }
    }
  }

  test("popcorn roundtrip") {
    for (e <- CMathMLTest.cMathMLRoundtrips) {
      Log.debug("e",e.toXML)
      val p = e.toPopcorn
      Log.debug("p",p)
      val e2 = CMathML.fromPopcorn(p)
      Log.debug("e2",e2.toXML)
      assertResult(e) { e2 }
    }
  }
}

object CMathMLTest {
  import CMathML.relation1._
  import CMathML.arith1._
  val cMathMLRoundtrips = List(
    CN(1) + CN(2),
    CI("x"),
    equal(CI("x"),CN(2)),
//    Apply(CSymbol("test","name"),CN(1)),
    CN("1.23"),
    CN(1.23), // This will not be exactly 1.23, because CN is initialized with (Double)1.23!
    divide(CN(2),CN(2)),
    minus(CN(3),CN(2)),
    times(minus(CN(4),CN(2)),CN(1.2)),
    uminus(CI("x")),
    uminus(CN("123")),
    CN(-234),
    power(CN(3),CI("x")),
    CS("hello"),
    quant1.forall(List(CI("x"),CI("y")), equal(CI("x"),CI("y"))),
    CBytes(1,2,3),
    logic1.equivalent(CI("A"),CI("B")),
    logic1.trueSym,
    logic1.falseSym
  )

  val subtermTests = List(
    ( divide(CN(1),CN(2)), Path(1), CN(1) ),
    ( equal(CI("x")+CI("y"), CI("y")+CN(-1)), Path(2,2), CN(-1) ),
    ( equal(CI("x")+CI("y"), CI("y")+CN(-1)), Path(2,1), CI("y") )
  )
}