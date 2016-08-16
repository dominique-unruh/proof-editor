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

class VisibleBox {
  val node = ObjectProperty[Node](initialValue=null)
  private val bounds = ObjectProperty[Bounds](initialValue=null)
  private val trafo = ObjectProperty[Transform](null)
  private val scrollViewportBounds = ObjectProperty[Bounds](null)
  private val scrollTransform = ObjectProperty[Transform](null)
  val scrollPane = ObjectProperty[ScrollPane](initialValue=null)

  scrollPane.onChange { (_, _, scroll) =>
    if (scroll==null) {
      scrollTransform.unbind; scrollTransform.value = null
      scrollViewportBounds.unbind; scrollViewportBounds.value = null
    } else {
      scrollTransform <== scroll.localToSceneTransformProperty
      scrollViewportBounds <== scroll.viewportBoundsProperty
    }
  }

  // Clipped bounding box in Scene coordinates
  val box = Bindings.createObjectBinding[Option[Bounds]](
    {() => for {
        bounds: Bounds <- Option(bounds.get)
        trafo: Transform <- Option(trafo.get)
        trafoedBounds: Bounds = trafo.transform(bounds)
        clippedBounds: Bounds = (for {
        //          scroll <- Option(leftScrollPane.value)
          viewBounds <- Option(scrollViewportBounds.value)
          scrollTrafo <- Option(scrollTransform.value)
          trafoedViewBounds = scrollTrafo.transform(viewBounds)
        //          _ = Log.debug("leftPoint",trafoedBounds, trafoedViewBounds)
        } yield intersection(trafoedBounds, trafoedViewBounds)).getOrElse(trafoedBounds)
      } yield clippedBounds},
    bounds, trafo, scrollViewportBounds, scrollTransform)


//  val connectingPoint = Bindings.createObjectBinding[Point2D](
//    {() => box.value match {
//        case Some(b) => new Point2D(b.getMaxX,b.getMinY+b.getHeight/2)
//        case None => Point2D.ZERO }},
//    box)

  node.onChange { (_, oldValue, newValue) =>
    if (oldValue eq newValue) ()
    else if (newValue==null) {
      bounds.unbind()
      trafo.unbind()
    } else {
      bounds <== newValue.boundsInLocal
      trafo <== newValue.localToSceneTransformProperty
    }}


  /** Intersection of a and b. If the intersection is empty, a zero-area box at the border of b is returned. */
  private def intersection(a:Bounds, b:Bounds) = {
    val minX = math.min(math.max(a.getMinX,b.getMinX),b.getMaxX)
    val minY = math.min(math.max(a.getMinY,b.getMinY),b.getMaxY)
    val maxX = math.max(math.min(a.getMaxX,b.getMaxX),b.getMinX)
    val maxY = math.max(math.min(a.getMaxY,b.getMaxY),b.getMinY)
    //    val minZ = a.getMinZ
    //    val depth = a.getDepth
    val width = maxX-minX
    val height = maxY-minY

    val res = new BoundingBox(minX,minY,width,height)
    Log.debug("Intersect:",a,b,res)
    res
  }


}

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
//  val rightProperty = ObjectProperty[Node](initialValue=null)

  private val left = new VisibleBox
  val leftProperty = left.node
  val leftScrollPane = left.scrollPane

  private val right = new VisibleBox
  val rightProperty = right.node
  val rightScrollPane = right.scrollPane

  left.box.onChange { (observable, oldValue, bounds) => bounds match {
    case Some(b) =>
      line.setStartX(b.getMaxX)
      line.setStartY(b.getMinY+b.getHeight/2)
    case None =>
  }}

  right.box.onChange { (observable, oldValue, bounds) => bounds match {
    case Some(b) =>
      line.setEndX(b.getMinX)
      line.setEndY(b.getMinY+b.getHeight/2)
    case None =>
  }}

  /** Property is true when the line should be added to the overlay */
  private val addedProperty = Bindings.createBooleanBinding(
    () => owner.sceneProperty.get!=null && rightProperty.get!=null && leftProperty.get!=null,
    owner.sceneProperty, leftProperty, rightProperty)

  val visibleProperty = BooleanProperty(true)
//  private val rightBoundsProperty = ObjectProperty[Bounds](initialValue=null)
//  private val rightTrafoProperty = ObjectProperty[Transform](null)
//
//  private val rightPointProperty = Bindings.createObjectBinding[Point2D](
//    {() =>
//      val bounds = rightBoundsProperty.get
//      val trafo = rightTrafoProperty.get
//      if (bounds==null || trafo==null) Point2D.ZERO
//      else trafo.transform(bounds.getMinX,bounds.getMinY+bounds.getHeight/2) },
//    rightBoundsProperty, rightTrafoProperty)
//
//  rightPointProperty.onChange
//    { (observable, oldValue, p) => line.setEndX(p.getX); line.setEndY(p.getY) }

  line.setStrokeWidth(4)
  line.setStroke(Color.BLUE.opacity(.3))

//  line.disableProperty <== owner.disabledProperty
  line.visibleProperty <== owner.visibleProperty && visibleProperty

  addedProperty.onChange { (_, oldValue, newValue) =>
      if (oldValue && !newValue) overlay.getChildren.remove(line)
      else if (!oldValue && newValue) overlay.getChildren.add(line) }

  assert(!addedProperty.get) // If it would be true, we would have missed that we need to add the line

//  rightProperty.onChange { (_, oldValue, newValue) =>
//      if (oldValue eq newValue) ()
//      else if (newValue==null) {
//        rightBoundsProperty.unbind()
//        rightTrafoProperty.unbind()
//      } else {
//        rightBoundsProperty.bind(newValue.boundsInLocalProperty)
//        rightTrafoProperty.bind(newValue.localToSceneTransformProperty)
//      }}


  def setLeft(node : Node) = leftProperty.set(node)
  def setLeft(node : javafx.scene.Node) = leftProperty.set(jfxNode2sfx(node))
  def setRight(node : Node) = rightProperty.set(node)
  def setRight(node : javafx.scene.Node) = rightProperty.set(jfxNode2sfx(node))
}
