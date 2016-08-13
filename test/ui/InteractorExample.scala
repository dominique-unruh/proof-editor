package ui

import scalaz._
import Scalaz._
import javafx.animation.{KeyFrame, Timeline}
import javafx.event.ActionEvent
import javafx.util

import com.thoughtworks.each.Monadic._
import misc.Log
import misc.Utils.ImplicitConversions._
import trafo.Interaction._
import trafo.{IntQ, Interaction, StringQ}

import scala.language.postfixOps

object InteractorExample {
//  implicit object interactionInstance extends Monad[Interaction] {
//    override def bind[A, B](fa: Interaction[A])(f: (A) => Interaction[B]): Interaction[B] = fa.flatMap(f)
//    override def point[A](a: => A): Interaction[A] = returnval(a)
//  }
//  implicit object seqTraverse extends Traverse[Seq] {
//    override def traverseImpl[F[_], A, B](l: Seq[A])(f: (A) => F[B])(implicit applicative: Applicative[F]): F[Seq[B]] =
//      listInstance.traverseImpl(l.toList)(f)
//  }
//  listInstance
  // MonadPlus[Interactor] /* with IsEmpty[Interactor] */ =


  def main(args: Array[String]) = {
    TestFxApp.run {
      val int = interaction {
        val i : Int = ask("int", new IntQ(<span>Nr 1?</span>)).each
        if (i>10) failWithU("i>10", <span>i must be at most 10</span>).each
        val strs = for (j <- (1 to i).toList.monadicLoop) yield
          ask("i" + j, new StringQ(<span>String nr. <b>{j}</b></span>)).each

        if (strs.contains("")) failU.each
        ""+i+"#"+strs.mkString(",")
      }

      val xxx = monadic[List] {
        val i = (Nil:List[Unit]).each
        55
      }

      val actor = new Interactor(int)
//      actor.setAnswer(0,"hello")
      Log.info("initial result",actor.result)

      actor.result.onChange { (_,_,res) => Log.info("result",res) }

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
