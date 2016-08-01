package ui.mathview

import java.lang.Double
import java.util.concurrent.Callable
import javafx.application.Application
import javafx.geometry.Pos
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.shape.Line
import javafx.scene.text.Text
import javafx.scene.{Group, Node, layout}
import javafx.stage.Stage

import scalafx.Includes._


class MathNode() extends Group {
  var child: javafx.scene.Node = null
}

class MathViewFXExample extends Application {
  def deattachJFXNode(node:Node) = {
    val parent = node.parent.value
    if (parent!=null)
      parent.asInstanceOf[layout.Pane].getChildren.remove(node)
  }


  override def start(primaryStage: Stage): Unit = {
    val root = new MathNode()
    val nz = new Group()



    val nh1 = new Text("x")
    val nh2 = new Text("x")
    val nh3 = new Text("x")
    nh2.boundsInLocalProperty().onChange({})


    val nbinop = new MathNode()
    nbinop.child = new BinOp(nh1,nh2)
    nbinop.child.boundsInLocalProperty().onChange({})
    nbinop.getChildren.setAll(nbinop.child)

    val nw = new MathNode()
    nw.child = new Fraction(nh3, nbinop)
    nw.child.boundsInLocalProperty().onChange({})
    nw.getChildren.setAll(nw.child)


    new BinOp(nz, nw)

    deattachJFXNode(nh2)

    sys.exit()
  }
}

object MathViewFXExample {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MathViewFXExample])
  }
}


class BinOp(a:Node, b:Node) extends HBox {
  a.layoutBounds.onChange({})
  b.layoutBounds.onChange({})
  getChildren.addAll(a,b)
}

class Fraction(a:Node, b:Node) extends VBox {
//  id = Integer.toHexString(hashCode()) // TODO: remove
  alignmentProperty.set(Pos.CENTER)
  val line = new Line()
  getChildren.addAll(a, line, b)

  val innerWidth = javafx.beans.binding.Bindings.createDoubleBinding(
    new Callable[Double] {
      override def call(): Double = b.layoutBounds.get.getWidth
    },
    b.layoutBounds)

  line.startX = 0
  line.endX <== innerWidth
  line.strokeWidth = 2
}
