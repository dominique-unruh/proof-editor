package ui.mathview

import java.lang.System._
import java.lang.Thread._
import javafx.application.Application
import javafx.scene.layout.Pane
import javafx.scene.web.WebView
import javafx.scene.{Node, Scene}
import javafx.stage.Stage

import cmathml.CMathML.{divide, times}
import cmathml._


//class TestFxApp extends Application {
//  override def start(st: Stage): Unit = {
//    import TestFxApp._
//
//    stage = st
//
//    pane = new Pane
//    stage.setScene(new Scene(TestFxApp.pane,640,480))
//    if (callMe!=null) callMe()
//    stage.show()
//  }
//}
//object TestFxApp {
//  //  var showThisNode : Node = null
//  var pane : Pane = null
//  var callMe : () => Unit = null
//  var stage : Stage = null
//
//  def run(init: =>Node) = {
//    callMe = { () => val node = init; pane.getChildren.add(node) }
//    Application.launch(classOf[TestFxApp])
//  }
//}


class MathViewFXExample extends Application {
//  def mainX(args: Array[String]) = {
//    TestFxApp.run {
//      val mw = new MathViewFX()
//
//      var s3 : MCSymbol = new MCSymbol(times)
//      var z = new MCI("z")
//      val h1 = new MCI("h1")
//      val h2 = new MCI("h2")
//      val binop = new MApply(times,h1,h2)
//      var w = new MApply(divide,new MCI("q"),binop)
//      var a2 = new MApply(s3,z,w)
//
//      mw.setMath(a2)
//
//      val u = new MCI("u")
//      h1.replaceWith(u) // TODO why does this throw an exception?
//
//      sys.exit()
//    }
//  }

  override def start(primaryStage: Stage): Unit = {
    val mw = new MathViewFX()

    var s3 : MCSymbol = new MCSymbol(times)
    var z = new MCI("z")
    val h1 = new MCI("h1")
    val h2 = new MCI("h2")
    val binop = new MApply(times,h1,h2)
    var w = new MApply(divide,new MCI("q"),binop)
    var a2 = new MApply(s3,z,w)

    mw.setMath(a2)

    val u = new MCI("u")
    h1.replaceWith(u) // TODO why does this throw an exception?

    sys.exit()
  }
}
