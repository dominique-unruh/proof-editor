package ui.mathview

import javafx.application.Application
import javafx.geometry.Bounds
import javafx.scene.layout
import javafx.scene.layout.Pane
import javafx.scene.text.Text
import javafx.stage.Stage

import cmathml.CMathML.{divide, times}
import cmathml._

import scala.collection.mutable
import scalafx.beans.property.ObjectProperty
import scalafx.scene.{Group, Node}


class MathViewFXExample extends Application {
  mathView =>

  val mathDoc = new MutableCMathMLDocument(CNone())

  def getInfoWithNewNode(cmml: MutableCMathML) = {
    if (cmml.node==null) cmml.node = new MathNode(cmml)
    cmml.addChangeListener(() => cmml.node.update())
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
    if (info.node.invalid) info.node.update()
    info.node
  }

  def disembed(node : MathNode, mathChild : MutableCMathML) : Unit = {
    val info = mathChild // getInfoWithNewNode(mathChild)
    if (info.embeddedIn==node) info.embeddedIn = null
  }

  class MathNode(val math : MutableCMathML) extends Group {

    val size = ObjectProperty[Bounds](null : Bounds)
    size.onChange { (_, _, s) => }

    val embedded = new mutable.MutableList[MutableCMathML]

    def getNodeForEmbedding(mathChild: MutableCMathML): Node = {
      val node = mathView.getNodeForEmbedding(this, mathChild)
      embedded += mathChild
      node
    }

    var child: javafx.scene.Node = null
    var invalid = true

    def update() = {
      for (n <- embedded) mathView.disembed(this, n)
      embedded.clear()
      invalid = false
      math match {
        case MApply(hd@MCSymbol("arith1", "times"), x, y) =>
          child = new BinOp("*",getNodeForEmbedding(x),getNodeForEmbedding(y))
        case MApply(hd@MCSymbol("arith1", "divide"), x, y) =>
          child = new Fraction(getNodeForEmbedding(x), getNodeForEmbedding(y))
        case MCNone() =>
          child = new Text("x")
      }
      size <== child.boundsInLocalProperty()
      children.setAll(child)
    }
  }

  override def start(primaryStage: Stage): Unit = {
    var s3 : MCSymbol = new MCSymbol(times)
    var z = new MCNone()
    val h1 = new MCNone()
    val h2 = new MCNone()
    val binop = new MApply(times,h1,h2)
    var w = new MApply(divide,new MCNone(),binop)
    var a2 = new MApply(s3,z,w)

    mathDoc.setRoot(a2)
      if (mathDoc.root.node == null) mathDoc.root.node = new MathNode(mathDoc.root)
      mathDoc.root.addChangeListener(() => mathDoc.root.node.update())
      if (mathDoc.root.embeddedIn != null) mathDoc.root.embeddedIn.invalid = true
      mathDoc.root.embeddedIn = null
      if (mathDoc.root.node.invalid) mathDoc.root.node.update()
      //    children.setAll(mathDoc.root.node)

//    binop._args.update(0,u)

    binop.node.getNodeForEmbedding(h2)

    sys.exit()
  }
}
object MathViewFXExample {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MathViewFXExample])
  }
}