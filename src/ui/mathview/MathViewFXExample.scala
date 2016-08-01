package ui.mathview

import java.lang.Double
import java.util.concurrent.Callable
import javafx.application.Application
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.shape.Line
import javafx.scene.text.Text
import javafx.scene.{Group, Node}
import javafx.stage.Stage

import scalafx.Includes._



class MathViewFXExample extends Application {
  override def start(primaryStage: Stage): Unit = {
    val nh2 = new Text("x")
    val nbinopc = new HBox(nh2)
    val nbinop = new Group(nbinopc)
//    nbinop.getChildren.setAll(nbinopc)



    val nw = new Group()
    val nwc = new Fraction(nbinop)
    nwc.boundsInLocalProperty().onChange({})
    nw.getChildren.setAll(nwc)

    new HBox(nw)
    nw.layoutBoundsProperty().onChange({})
//    new BinOp(nw)

//    val parent = nbinopc
    nbinopc.getChildren.remove(nh2)

    sys.exit()
  }
}

object MathViewFXExample {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MathViewFXExample])
  }
}

class Fraction(b:Node) extends VBox {
  val line = new Line()
  getChildren.addAll(line, b)

  val innerWidth = javafx.beans.binding.Bindings.createDoubleBinding(
    new Callable[Double] {
      override def call(): Double = b.layoutBounds.get.getWidth
    },
    b.layoutBounds)

  line.startX = 0
  line.endX <== innerWidth
}
