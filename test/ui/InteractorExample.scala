package ui

import javafx.animation.{KeyFrame, Timeline}
import javafx.event.ActionEvent
import javafx.util

import misc.Utils.ImplicitConversions._
import trafo.Interaction._
import trafo.{IntQ, Interaction, StringQ}

object InteractorExample {
  def main(args: Array[String]) = {
    TestFxApp.run {
      def manyQ(i:Int, j:Int=1) : Interaction[List[String]] = i match {
        case 0 => returnval(List.empty)
        case _ if i >= 0 =>
          for {x <- ask("i" + j, new StringQ(<span>String nr. <b>{j}</b></span>))
               xs <- manyQ(i - 1, j + 1)
        } yield x :: xs
      }

      val int = for {i <- ask("int", new IntQ(<span>Nr 1?</span>))
                     strs <- manyQ(i)
        } yield ""+i+"#"+strs.mkString(",")

      val actor = new Interactor(int)
//      actor.setAnswer(0,"hello")

      actor.setAnswer(0,-1.asInstanceOf[Integer])

      val timeline = new Timeline(
        new KeyFrame(util.Duration.millis(1000), {(_:ActionEvent) => actor.setAnswer(0,1.asInstanceOf[Integer])}),
        new KeyFrame(util.Duration.millis(2000), {(_:ActionEvent) => actor.setAnswer(1,"there")}),
        new KeyFrame(util.Duration.millis(4000), {(_:ActionEvent) => actor.setAnswer(0,2.asInstanceOf[Integer])}),
        new KeyFrame(util.Duration.millis(5000), {(_:ActionEvent) => actor.setAnswer(1, "is")}),
        new KeyFrame(util.Duration.millis(6000), {(_:ActionEvent) => actor.setAnswer(0, 3.asInstanceOf[Integer])}),
        new KeyFrame(util.Duration.millis(7000), {(_:ActionEvent) => actor.setAnswer(1, "test")})
      )
//      timeline.play()

      actor
    }
  }
}
