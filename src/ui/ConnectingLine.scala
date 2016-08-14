package ui

import javafx.geometry.{BoundingBox, Bounds, Point2D}
import javafx.scene.control.ScrollPane
import javafx.scene.paint.Color
import javafx.scene.transform.Transform

import misc.Log

import scalafx.Includes._
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.geometry
import scalafx.scene.{Node, paint}
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Line

/** Displays a connecting line between two components.
  * [[setLeft(node:scalafx\.scene\.Node):Unit* setLeft]] and [[setRight(node:scalafx\.scene\.Node):Unit* setRight]]
  * set those two components.
  *
  * The two components and the [[overlay]] must be part of the same [[javafx.scene.Scene]].
  *
  * @param owner The component that logically contains this line (if owner is removed, the line is removed, if owner is invisible, the line is invisible)
  * @param overlay A [[Pane]] into which the line will be drawn. It is expected that [[overlay]] covers the whole scene.
  *                (Or at least that its local coordinates coincide with the scene coordinates)
  */
class ConnectingLine(val owner : Node, val overlay : Pane) {
//  def this(owner : javafx.scene.Node, overlay : javafx.scene.layout.Pane) =
//    this(jfxNode2sfx(owner), jfxPane2sfx(overlay))

  val line = new Line()
  //    private var lineAdded = true
  val leftProperty = ObjectProperty[Node](initialValue=null)
  val rightProperty = ObjectProperty[Node](initialValue=null)

  /** Property is true when the line should be added to the overlay */
  private val addedProperty = Bindings.createBooleanBinding(
    () => owner.sceneProperty.get!=null && rightProperty.get!=null && leftProperty.get!=null,
    owner.sceneProperty, leftProperty, rightProperty)

  private def intersection(a:Bounds, b:Bounds) = {
    val minX = math.max(a.getMinX,b.getMinX)
    val minY = math.max(a.getMinY,b.getMinY)
    val maxX = math.min(a.getMaxX,b.getMaxX)
    val maxY = math.min(a.getMaxY,b.getMaxY)
//    val minZ = a.getMinZ
//    val depth = a.getDepth
    val width = maxX-minX
    val height = maxY-minY

    new BoundingBox(minX,minY,width,height)
  }

  val visibleProperty = BooleanProperty(true)
  val leftScrollPane = ObjectProperty[ScrollPane](initialValue=null)
  private val leftBoundsProperty = ObjectProperty[Bounds](initialValue=null)
  private val leftTrafoProperty = ObjectProperty[Transform](null)
  private val rightBoundsProperty = ObjectProperty[Bounds](initialValue=null)
  private val rightTrafoProperty = ObjectProperty[Transform](null)
  private val leftScrollViewportBounds = ObjectProperty[Bounds](null)
  private val leftScrollTransform = ObjectProperty[Transform](null)
  private val leftPointProperty = Bindings.createObjectBinding[Point2D](
    {() =>
      val leftPoint = for {
        bounds: Bounds <- Option(leftBoundsProperty.get)
        trafo: Transform <- Option(leftTrafoProperty.get)
        trafoedBounds: Bounds = trafo.transform(bounds)
        clippedBounds: Bounds = (for {
//          scroll <- Option(leftScrollPane.value)
          viewBounds <- Option(leftScrollViewportBounds.value)
          scrollTrafo <- Option(leftScrollTransform.value)
          trafoedViewBounds = scrollTrafo.transform(viewBounds)
//          _ = Log.debug("leftPoint",trafoedBounds, trafoedViewBounds)
        } yield intersection(trafoedBounds, trafoedViewBounds)).getOrElse(trafoedBounds)
      } yield new Point2D(clippedBounds.getMaxX,clippedBounds.getMinY+clippedBounds.getHeight/2)

      leftPoint.getOrElse(Point2D.ZERO)},
    leftBoundsProperty, leftTrafoProperty, leftScrollViewportBounds, leftScrollTransform)

  private val rightPointProperty = Bindings.createObjectBinding[Point2D](
    {() =>
      val bounds = rightBoundsProperty.get
      val trafo = rightTrafoProperty.get
      if (bounds==null || trafo==null) Point2D.ZERO
      else trafo.transform(bounds.getMinX,bounds.getMinY+bounds.getHeight/2) },
    rightBoundsProperty, rightTrafoProperty)

  leftScrollPane.onChange { (_,_,scroll) =>
    if (scroll==null) {
      leftScrollTransform.unbind; leftScrollTransform.value = null
      leftScrollViewportBounds.unbind; leftScrollViewportBounds.value = null
    } else {
      leftScrollTransform <== scroll.localToSceneTransformProperty
      leftScrollViewportBounds <== scroll.viewportBoundsProperty
    }
  }

  leftPointProperty.onChange
    { (observable, oldValue, p) => line.setStartX(p.getX); line.setStartY(p.getY) }

  rightPointProperty.onChange
    { (observable, oldValue, p) => line.setEndX(p.getX); line.setEndY(p.getY) }

  line.setStrokeWidth(4)
  line.setStroke(Color.BLUE.opacity(.3))

//  line.disableProperty <== owner.disabledProperty
  line.visibleProperty <== owner.visibleProperty && visibleProperty

  addedProperty.onChange { (_, oldValue, newValue) =>
      if (oldValue && !newValue) overlay.getChildren.remove(line)
      else if (!oldValue && newValue) overlay.getChildren.add(line) }

  assert(!addedProperty.get) // If it would be true, we would have missed that we need to add the line

  leftProperty.onChange { (_, oldValue, newValue) =>
      if (oldValue eq newValue) ()
      else if (newValue==null) {
        leftBoundsProperty.unbind()
        leftTrafoProperty.unbind()
      } else {
        leftBoundsProperty <== newValue.boundsInLocal
        leftTrafoProperty <== newValue.localToSceneTransformProperty
      }}

  rightProperty.onChange { (_, oldValue, newValue) =>
      if (oldValue eq newValue) ()
      else if (newValue==null) {
        rightBoundsProperty.unbind()
        rightTrafoProperty.unbind()
      } else {
        rightBoundsProperty.bind(newValue.boundsInLocalProperty)
        rightTrafoProperty.bind(newValue.localToSceneTransformProperty)
      }}


  def setLeft(node : Node) = leftProperty.set(node)
  def setLeft(node : javafx.scene.Node) = leftProperty.set(jfxNode2sfx(node))
  def setRight(node : Node) = rightProperty.set(node)
  def setRight(node : javafx.scene.Node) = rightProperty.set(jfxNode2sfx(node))
}
