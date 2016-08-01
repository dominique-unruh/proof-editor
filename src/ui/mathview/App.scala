package ui.mathview

import java.lang.Double
import java.util.concurrent.Callable
import javafx.application.Application
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.geometry.Bounds
import javafx.scene.Group
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.shape.Line
import javafx.scene.text.Text
import javafx.stage.Stage

class App extends Application {
  override def start(primaryStage: Stage): Unit = {
    val t = new Text("x")
    val h = new HBox(t)
    val g = new Group(h)

    val l = new Line()
    val innerWidth = javafx.beans.binding.Bindings.createDoubleBinding(
      new Callable[Double] {
        override def call(): Double = g.layoutBoundsProperty().get.getWidth
      },
      g.layoutBoundsProperty())
    l.endXProperty().bind(innerWidth)

    val v = new VBox(l, g)
    v.boundsInLocalProperty().addListener(new ChangeListener[Bounds] {
      override def changed(observable: ObservableValue[_ <: Bounds], oldValue: Bounds, newValue: Bounds): Unit = {}
    })

    val g2 = new Group(v)
    g2.layoutBoundsProperty().addListener(new ChangeListener[Bounds] {
      override def changed(observable: ObservableValue[_ <: Bounds], oldValue: Bounds, newValue: Bounds): Unit = {}
    })

    h.getChildren.remove(t)

    sys.exit()
  }
}

object App {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[App])
  }
}
