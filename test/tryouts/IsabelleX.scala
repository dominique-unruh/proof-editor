package tryouts

import java.net.ConnectException

import cmathml.CN
import com.twitter.finagle.{ChannelException, ChannelWriteException}
import isabelle.Isabelle.toCMathML
import isabelle.{Isabelle, IsabelleLocal, IsabelleRemote}

import scala.concurrent._
import scala.concurrent.duration._

object IsabelleX {
//  val testTerm1 = disj(`true`,`false`)
//  val testTerm2 = plus(Free("x",dummy),Free("y",int))

  def loadIsabelle() = {
    try {
      val isabelle = new IsabelleRemote()
      isabelle.ping()
      isabelle
    } catch {
      case e:ChannelException =>
        println("exn",e)
        new IsabelleLocal()
    }
  }

  def w[U](fut:Future[U]) = Await.result(fut,Duration.Inf)
  def main(args: Array[String]): Unit = {
    val math = CN(10)+CN(0)+CN(1)+CN(-1)+CN(-10)
//    val math = fns1.lambda(CI("x"),CI("x")+CI("y")+CN(1))
    val term1 =  Isabelle.fromCMathML(math)
    println("term1",toCMathML(term1),term1)
    val isabelle = loadIsabelle()
    val term3 = isabelle.typeInference(term1)
    println("term3",isabelle.termToString(term3),toCMathML(term3))
    val term4 = isabelle.simplifyTerm(term3)
    println("term4",isabelle.termToString(term4),toCMathML(term4))
//    val str = isabelle.termToString(term4)
//    println("str",str)

    val disp = isabelle.dispose()
  }
}
