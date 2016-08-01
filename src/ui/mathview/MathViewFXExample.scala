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


class MathNode(mathView : MathViewFXExample, val math : MutableCMathML) extends Group {

  val size = ObjectProperty[Bounds](null : Bounds)
  size.onChange { (_, _, s) => }

  def getNodeForEmbedding(mathChild: MutableCMathML): Node =
    mathView.getNodeForEmbedding(this, mathChild)

  var child: javafx.scene.Node = null
  var invalid = true

}

class MathViewFXExample extends Application {
  mathView =>

  val mathDoc = new MutableCMathMLDocument(CNone())

  def getInfoWithNewNode(cmml: MutableCMathML) = {
    if (cmml.node==null) cmml.node = new MathNode(this,cmml)
    cmml.addChangeListener{() => updateMe = cmml.node; update()}
    cmml
  }

  def deattachJFXNode(node:Node) = {
    val parent = node.parent.value
    if (parent!=null)
      parent.asInstanceOf[layout.Pane].getChildren.remove(node) // TODO: is there a better way?
  }


  def getNodeForEmbedding(requestingNode: MathNode, mathChild: MutableCMathML): Node = {
    val info = getInfoWithNewNode(mathChild)
    if (info.embeddedIn != null) info.embeddedIn.invalid = true
    deattachJFXNode(info.node) // TODO: is this needed?
    info.embeddedIn = requestingNode
    if (info.node.invalid) { updateMe  = info.node; update() }
    info.node
  }

//  def disembed(node : MathNode, mathChild : MutableCMathML) : Unit = {
//    val info = mathChild // getInfoWithNewNode(mathChild)
//    if (info.embeddedIn==node) info.embeddedIn = null
//  }

  var updateMe : MathNode = null
  def update() = {
    val t = updateMe
//    for (n <- t.embedded) disembed(t, n)
//    t.embedded.clear()
    t.invalid = false
    t.math match {
      case MApply(hd@MCSymbol("arith1", "times"), x, y) =>
          t.child = new BinOp(t.getNodeForEmbedding(x),t.getNodeForEmbedding(y))
      case MApply(hd@MCSymbol("arith1", "divide"), x, y) =>
        t.child = new Fraction(t.getNodeForEmbedding(x), t.getNodeForEmbedding(y))
      case MCNone() =>
        t.child = new Text("x")
    }
    t.size <== t.child.boundsInLocalProperty()
    t.getChildren.setAll(t.child)
  }


  override def start(primaryStage: Stage): Unit = {
    var z = new MCNone()
    val h1 = new MCNone()
    val h2 = new MCNone()
    val binop = new MApply(times,h1,h2)
    var w = new MApply(divide,new MCNone(),binop)
    var a2 = new MApply(times,z,w)

    mathDoc.setRoot(a2)
    val root = new MathNode(this,mathDoc.root)
    mathDoc.root.node = root
    val nz = getNodeForEmbedding(root,z)
    val nw = getNodeForEmbedding(root,w)
    new BinOp(nz, nw)

    getNodeForEmbedding(binop.node,h2)

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
