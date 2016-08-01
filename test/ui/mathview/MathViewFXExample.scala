package ui.mathview

import javafx.application.Application
import javafx.stage.Stage

import cmathml.CMathML.{divide, times}
import cmathml._


class MathViewFXExample extends Application {
  override def start(primaryStage: Stage): Unit = {
    val mw = new MathViewFX()

    var s3 : MCSymbol = new MCSymbol(times)
    var z = new MCI("z")
    val h1 = new MCI("h1")
    val h2 = new MCI("h2")
    val binop = new MApply(times,h1,h2)
    var w = new MApply(divide,new MCI("q"),binop)
    var a2 = new MApply(s3,z,w)

//    mw.setMath(a2)
    mw.mathDoc.setRoot(a2)

    val u = new MCI("u")
    h1.replaceWith(u) // TODO why does this throw an exception?

    sys.exit()
  }
}
