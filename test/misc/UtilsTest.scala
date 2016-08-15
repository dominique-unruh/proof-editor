package misc

import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}

import test.UnitSpec

class UtilsTest extends UnitSpec {
  test("prettyXML") {
    import Utils.prettyXML
    assert(prettyXML(<x xml:space="preserve">
    </x>) == <x xml:space="preserve">
    </x>)

    assert(prettyXML(<x>Bla</x>) == <x>Bla</x>)

    assert(prettyXML(<x>
    <y></y></x>) ==
<x>
  <y></y>
</x>)

    assert(prettyXML(<x>{"\n"}{"\n"}</x>) == <x>
</x>)
  }

  test("GetterSetterProperty") {
//    import scala.language.reflectiveCalls
    var setCount = 0
    var value = ""
    var valueCopy = value
    val prop = new GetterSetterProperty[String] {
      override protected def getter: String = value
      override protected def setter(newValue: String): Unit =
        { value = newValue; setCount += 1 }
    }

    prop.addListener(new ChangeListener[String] {
      override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
        assert(valueCopy == oldValue)
        valueCopy = newValue
      }
    })

    prop.set("hello")
    assertResult("hello") {value}
    assertResult("hello") {valueCopy}
    assertResult("hello") {prop.get}
    assertResult(1) {setCount}

    setCount = 0
    value = "hey"
    prop.fireValueChangedEvent()
    assertResult("hey") {value}
    assertResult("hey") {valueCopy}
    assertResult("hey") {prop.get}
    assertResult(0) {setCount}
  }

  test("GetterSetterProperty - bind") {
    var value = ""
    val prop = new GetterSetterProperty[String] {
      override protected def getter: String = value
      override protected def setter(newValue: String): Unit = value = newValue
    }
    val prop2 = new SimpleStringProperty("")

    intercept[RuntimeException] { prop.bind(prop2) }
  }
}
