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

import Interaction._
import misc.Utils.JavaFXImplicits._

object InteractorExample {
  def main(args: Array[String]) = {
    TestFxApp.run {
      def manyQ(i:Int, j:Int=1) : Interaction[List[String]] = i match {
        case 0 => returnval(List.empty)
        case _ => for {x <- ask("i" + j, new StringQ(<node>String nr. $j</node>))
                       xs <- manyQ(i - 1, j + 1)
        } yield (x.getOrElse("?") :: xs)
      }

      val int = for {i <- ask("int", new IntQ(<span>Nr 1?</span>))
                     i2 : Int = if (i.isEmpty) 0 else i.get
                     strs <- manyQ(i2)
        } yield ""+i+"#"+strs.mkString(",")

      val actor = new Interactor(int)
//      actor.setAnswer(0,"hello")

      val timeline = new Timeline(
        new KeyFrame(util.Duration.millis(1000), {(_:ActionEvent) => actor.setAnswer(0,Some(1.asInstanceOf[Integer]))}),
        new KeyFrame(util.Duration.millis(2000), {(_:ActionEvent) => actor.setAnswer(1,Some("there"))}),
        new KeyFrame(util.Duration.millis(4000), {(_:ActionEvent) => actor.setAnswer(0,Some(2.asInstanceOf[Integer]))}),
        new KeyFrame(util.Duration.millis(5000), {(_:ActionEvent) => actor.setAnswer(1,Some("is"))}),
        new KeyFrame(util.Duration.millis(6000), {(_:ActionEvent) => actor.setAnswer(0,Some(3.asInstanceOf[Integer]))}),
        new KeyFrame(util.Duration.millis(7000), {(_:ActionEvent) => actor.setAnswer(1,Some("test"))})
      )
      timeline.play()

      actor
    }
  }
}
