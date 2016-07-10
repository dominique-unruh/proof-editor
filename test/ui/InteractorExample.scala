package ui

import java.lang.Thread.currentThread
import javafx.animation.{KeyFrame, Timeline}
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.scene.control.ListView
import javafx.scene.{Node, Parent, Scene}
import javafx.scene.layout.{AnchorPane, Pane, Region}
import javafx.stage.Stage
import javafx.util

import trafo.{IntQ, Interaction, StringQ}

class TestFxApp extends Application {
  override def start(stage: Stage): Unit = {
    TestFxApp.pane = new AnchorPane()
    stage.setScene(new Scene(TestFxApp.pane))
//    if (TestFxApp.showThisNode!=null) TestFxApp.pane.getChildren.add(TestFxApp.showThisNode)
    stage.setTitle(TestFxApp.name)
    if (TestFxApp.callMe!=null) TestFxApp.callMe()
    stage.show()
  }
}
object TestFxApp {
//  var showThisNode : Node = null
  var pane : Pane = null
  var callMe : () => Unit = null
  var name : String = "TestFxApp"

  def run(init: =>Node) = {
    callMe = { () => val node = init; pane.getChildren.add(node) }
    name = currentThread.getStackTrace()(2).getClassName
    Application.launch(classOf[TestFxApp])
  }
}

import Interaction._
import misc.Utils.JavaFXImplicits._

object InteractorExample {
  def main(args: Array[String]) = {
    TestFxApp.run {
      val int = for {i <- ask(new StringQ(<span>Nr 1?</span>))
                     j <- if (i.length<10) ask(new StringQ(<span>Nr 2 (first was "{i}")</span>)) else returnval("toolong")
      } yield i + "," + j
      val actor = new Interactor(int)
//      actor.answer(0,"hello")

      val timeline = new Timeline(
        new KeyFrame(util.Duration.millis(1000), {(_:ActionEvent) => actor.answer(0,"hello")}),
        new KeyFrame(util.Duration.millis(2000), {(_:ActionEvent) => actor.answer(1,"there")})
//        new KeyFrame(util.Duration.millis(4000), {(_:ActionEvent) => actor.answer(0,"this")}),
//        new KeyFrame(util.Duration.millis(5000), {(_:ActionEvent) => actor.answer(1,"is")}),
//        new KeyFrame(util.Duration.millis(6000), {(_:ActionEvent) => actor.answer(0,"a")}),
//        new KeyFrame(util.Duration.millis(7000), {(_:ActionEvent) => actor.answer(1,"test")})
      )
      timeline.play()

      actor
    }
  }
}
