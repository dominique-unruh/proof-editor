package z3

import com.microsoft.z3.Version
import test.UnitSpec

class Z3Test extends UnitSpec {
  "Z3" should "initialize" in {
    println("version",Z3.version)
  }
}