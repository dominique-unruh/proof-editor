package ui.mathview

import java.lang.Double
import java.util.concurrent.Callable
import javafx.application.Application
import javafx.scene.Group
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.shape.Line
import javafx.scene.text.Text
import javafx.stage.Stage

import scalafx.Includes._



class MathViewFXExample extends Application {
  override def start(primaryStage: Stage): Unit = {
    val nh2 = new Text("x")
    val nbinopc = new HBox(nh2)
    val nbinop = new Group(nbinopc)

    val line = new Line()
    val innerWidth = javafx.beans.binding.Bindings.createDoubleBinding(
      new Callable[Double] {
        override def call(): Double = nbinop.layoutBounds.get.getWidth
      },
      nbinop.layoutBounds)
    line.endX <== innerWidth

    val nwc = new VBox(line, nbinop)
    nwc.boundsInLocalProperty().onChange({})

    val nw = new Group(nwc)
    nw.layoutBoundsProperty().onChange({})

    nbinopc.getChildren.remove(nh2)

    sys.exit()
  }
}

object MathViewFXExample {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MathViewFXExample])
  }
}
