
import RefState.{T, newRef}

import scalaz._
import Scalaz._
//import effectful._
//import language.postfixOps
import com.thoughtworks.each.Monadic._
import scala.language.higherKinds

object Temp {
  def main(args: Array[String]) = {
    import TempMacro._
//    printf("hello %s!", "world")

    val l = List(1,2,3)

//    val xs = List(1,2,3)
//    val ys = List(true,false)
//
//    effectfully { (xs!, ys!) }

//    val e = effectfully {
//      (l!, l!)
//    }
//    println(s"e: $e")

    val m = monadic[List] {
      var x = 0
      x = l.each+x
      x
    }
    println(s"m: $m")

    type state = Int
    type aux[T] = StateT[List,state,T]
    type trans[F[_],A] = StateT[F,state,A]

    val l2 : aux[Int] = l.liftM[trans]
//      StateT.apply( s => l.map((s,_)) )

    val m2$ : aux[String] = monadic[aux] {
      (State.put[state](0).lift[List]).each
      val xval = State.get[state].lift[List].each
      State.put[state](l2.each + xval).lift[List].each
      "done" + State.get[state].lift[List].each
    }
    val m2 = m2$.run(-1)
    println(s"m2: $m2")

    type aux3[T] = RefState.T[List,T]
    val l3 : aux3[Int] = RefState.liftM(l)

    val m3$ : aux3[String] = monadic[(scala.AnyRef {
      type l[A] = RefState.T[List, A]
    })#l] {
      val x : RefState.Ref[Int] = newRef[List,Int](0).each // var x = 0
//      val xval = x.get[List].each   // val xval = x
//      x.put[List](l3.each + xval).each  // x = l2.each + xval
//      "done" + x.get[List].each
      "done"
    }
    val m3 = m3$.run(RefState.empty)
    println(s"m3: $m3")

    val m4$ : aux3[String] = monadicRef[List] apply {
//      val x = newRef[List,Int](0).each // var x = 0
      var x = 0
//      val xval = x.get[List].each   // val xval = x
//      x.put[List](l3.each + xval).each  // x = l2.each + xval
//      "done" + x.get[List].each
      "done"
    }
    val m4 = m4$.run(RefState.empty)
    println(s"m4: $m4")

  }
}

//Warning:scala: monadic[scala.AnyRef {
//  type l[A] = RefState.T[List, A]
//}#l]({
//  val x: RefState.Ref[Int] = newRef[List, Int](0).each;
//  "done"
//})
