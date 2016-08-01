package ui.mathview

import java.lang.Double
import java.util.concurrent.Callable
import javafx.application.Application
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.geometry.Bounds
import javafx.scene.layout
import javafx.scene.layout.Pane
import javafx.scene.text.Text
import javafx.stage.Stage

import scalafx.Includes._
import cmathml.CMathML.{divide, times}
import cmathml._

import scala.collection.mutable
import scalafx.beans.binding.Bindings
import scalafx.beans.property.ObjectProperty
import javafx.geometry.Pos
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.shape.Line
import javafx.scene.{Group, Node}


class MathNode(val math : MutableCMathML) extends Group {

//  val size = ObjectProperty[Bounds](null : Bounds)
//  size.onChange { (_, _, s) => }
//
//  def getNodeForEmbedding(mathChild: MutableCMathML): Node =
//    mathView.getNodeForEmbedding(this, mathChild)

  var child: javafx.scene.Node = null
  var invalid = true

}

class MathViewFXExample extends Application {
  def deattachJFXNode(node:Node) = {
    val parent = node.parent.value
    if (parent!=null)
      parent.asInstanceOf[layout.Pane].getChildren.remove(node)
  }


  def getNodeForEmbedding(mathChild: MutableCMathML): Node = {

    if (mathChild.node==null) mathChild.node = new MathNode(mathChild)
    deattachJFXNode(mathChild.node)
    if (mathChild.node.invalid) { updateMe  = mathChild.node; update() }
    mathChild.node
  }

  var updateMe : MathNode = null
  def update() = {
    val t = updateMe
    t.invalid = false
    t.math match {
      case MApply(hd@MCSymbol("arith1", "times"), x, y) =>
          t.child = new BinOp(getNodeForEmbedding(x),getNodeForEmbedding(y))
      case MCNone() =>
        t.child = new Text("x")
    }
    t.child.boundsInLocalProperty().onChange({})
    t.getChildren.setAll(t.child)
  }


  override def start(primaryStage: Stage): Unit = {
//    val h1 = new MCNone()
    val h2 = new MCNone()
//    val binop = new MApply(times,h1,h2)
//    val h3 = new MCNone()

    val root = new MathNode(null)
    val nz = new MathNode(null)

    val nw = new MathNode(null)
    deattachJFXNode(nw)

//    nw.invalid = false
    val nh3 = new Text("x") // getNodeForEmbedding(h3)

    val nbinop = new MathNode(null)

    val nh1 = new Text("x") // getNodeForEmbedding(h1)

    val nh2 = new MathNode(h2)
    h2.node = nh2
    deattachJFXNode(nh2)
    updateMe  = h2.node
//    update()

//    nh2.invalid = false
//    nh2.math match {
//      case MApply(hd@MCSymbol("arith1", "times"), x, y) =>
//        nh2.child = new BinOp(getNodeForEmbedding(x),getNodeForEmbedding(y))
//      case MCNone() =>
        nh2.child = new Text("x")
//    }
    nh2.child.boundsInLocalProperty().onChange({})
    nh2.getChildren.setAll(nh2.child)


//    val nh2 = getNodeForEmbedding(h2)

    nbinop.child = new BinOp(nh1,nh2)
    nbinop.child.boundsInLocalProperty().onChange({})
    nbinop.getChildren.setAll(nbinop.child)

    nw.child = new Fraction(nh3, nbinop)
    nw.child.boundsInLocalProperty().onChange({})
    nw.getChildren.setAll(nw.child)


    new BinOp(nz, nw)

    if (h2.node==null) h2.node = new MathNode(h2)
    deattachJFXNode(h2.node)
    if (h2.node.invalid) { updateMe  = h2.node; update() }
    h2.node

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
