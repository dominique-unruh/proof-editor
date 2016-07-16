package ui

import java.lang.Boolean
import javafx.beans.binding.{BooleanBinding, ObjectBinding}
import javafx.beans.property.{SimpleBooleanProperty, SimpleObjectProperty}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.geometry.{Bounds, Point2D}
import javafx.scene.Node
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.scene.shape.Line
import javafx.scene.transform.Transform


/** Displays a connecting line between two components.
  * [[ConnectingLine.setLeft]] and [[ConnectingLine.setRight]] set those two components.
  *
  * The two components and the [[overlay]] must be part of the same [[javafx.scene.Scene]].
  *
  * @param owner The component that logically contains this line (if owner is removed, the line is removed, if owner is invisible, the line is invisible)
  * @param overlay A [[Pane]] into which the line will be drawn. It is expected that [[overlay]] covers the whole scene.
  *                (Or at least that its local coordinates coincide with the scene coordinates)
  */
class ConnectingLine(val owner : Node, val overlay : Pane) {
  val line = new Line()
  //    private var lineAdded = true
  val leftProperty = new SimpleObjectProperty[Node]
  val rightProperty = new SimpleObjectProperty[Node]
  /** Property is true when the line should be added to the overlay */
  private val addedProperty = new BooleanBinding {
    bind(owner.sceneProperty, leftProperty, rightProperty)
    override protected def computeValue() =
      owner.sceneProperty.get!=null && rightProperty.get!=null && leftProperty.get!=null
  }
  val visibleProperty = new SimpleBooleanProperty(true)
  private val leftBoundsProperty = new SimpleObjectProperty[Bounds]()
  private val leftTrafoProperty = new SimpleObjectProperty[Transform]()
  private val rightBoundsProperty = new SimpleObjectProperty[Bounds]()
  private val rightTrafoProperty = new SimpleObjectProperty[Transform]()
  private val leftPointProperty = new ObjectBinding[Point2D] {
    bind(leftBoundsProperty, leftTrafoProperty)
    override def computeValue(): Point2D = {
      val bounds = leftBoundsProperty.get
      val trafo = leftTrafoProperty.get
      if (bounds==null || trafo==null) return Point2D.ZERO
      trafo.transform(bounds.getMaxX,bounds.getMinY+bounds.getHeight/2)
    }
  }
  private val rightPointProperty = new ObjectBinding[Point2D] {
    bind(rightBoundsProperty, rightTrafoProperty)
    override def computeValue(): Point2D = {
      val bounds = rightBoundsProperty.get
      val trafo = rightTrafoProperty.get
      if (bounds==null || trafo==null) return Point2D.ZERO
      trafo.transform(bounds.getMinX,bounds.getMinY+bounds.getHeight/2)
    }
  }

  leftPointProperty.addListener(new ChangeListener[Point2D] {
    override def changed(observable: ObservableValue[_ <: Point2D], oldValue: Point2D, p: Point2D): Unit = {
      line.setStartX(p.getX); line.setStartY(p.getY) }
  })
  rightPointProperty.addListener(new ChangeListener[Point2D] {
    override def changed(observable: ObservableValue[_ <: Point2D], oldValue: Point2D, p: Point2D): Unit = {
      line.setEndX(p.getX); line.setEndY(p.getY) }
  })

  line.setStrokeWidth(4)
  line.setStroke(Color.BLUE.deriveColor(0,1,1,.3))

  line.disableProperty.bind(owner.disabledProperty)
  line.visibleProperty.bind(owner.visibleProperty and visibleProperty)

  addedProperty.addListener(new ChangeListener[Boolean] {
    override def changed(observable: ObservableValue[_ <: Boolean], oldValue: Boolean, newValue: Boolean): Unit =
      if (oldValue && !newValue) overlay.getChildren.remove(line)
      else if (!oldValue && newValue) overlay.getChildren.add(line)
  })
  assert(addedProperty.get==false) // If it would be true, we would have missed that we need to add the line

  leftProperty.addListener(new ChangeListener[Node] {
    override def changed(observable: ObservableValue[_ <: Node], oldValue: Node, newValue: Node): Unit = {
      if (oldValue eq newValue) return
      if (newValue==null) {
        leftBoundsProperty.unbind()
        leftTrafoProperty.unbind()
      } else {
        leftBoundsProperty.bind(newValue.boundsInLocalProperty)
        leftTrafoProperty.bind(newValue.localToSceneTransformProperty)
      }
    }
  })

  rightProperty.addListener(new ChangeListener[Node] {
    override def changed(observable: ObservableValue[_ <: Node], oldValue: Node, newValue: Node): Unit = {
      if (oldValue eq newValue) return
      if (newValue==null) {
        rightBoundsProperty.unbind()
        rightTrafoProperty.unbind()
      } else {
        rightBoundsProperty.bind(newValue.boundsInLocalProperty)
        rightTrafoProperty.bind(newValue.localToSceneTransformProperty)
      }
    }
  })
}
