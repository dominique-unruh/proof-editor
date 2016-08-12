
import RefState.{T, newRef}

import scalaz._
import Scalaz._
//import effectful._
//import language.postfixOps
import com.thoughtworks.each.Monadic._
import scala.language.higherKinds

object Temp {
  def main(args: Array[String]) = {

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
      State.put[state](0).lift[List].each
      val xval = State.get[state].lift[List].each
      State.put[state](l2.each + xval).lift[List].each
      "done" + State.get[state].lift[List].each
    }
    val m2 = m2$.run(-1)
    println(s"m2: $m2")

    type aux3[T] = RefState.T[List,T]
    val l3 : aux3[Int] = RefState.liftM(l)

    val m3$ : aux3[String] = monadic[aux3] {
      val x = newRef[List,Int](0).each // var x = 0
      val xval = x.get[List].each   // val xval = x
      x.put[List](l3.each + xval).each  // x = l2.each + xval
      "done" + x.get[List].each
    }
    val m3 = m3$.run(RefState.empty)
    println(s"m3: $m3")

  }
}

object RefState {
  type T[M[_],A] = StateT[M,St,A]
//  implicit val monadTrans : MonadTrans[T] = ???
  val empty = new St(Map.empty)
  def liftM[M[_],A](m:M[A])(implicit monad: Monad[M]) : T[M,A] = m.liftM[RefState.T]
  class St private[RefState] (private val mem: Map[Ref[_],Any]) extends AnyVal {
    def get[A](ref:Ref[A]) : A = mem.get(ref) match {
      case None => throw new IllegalArgumentException("reference from wrong state (or from future version of it)")
      case Some(v) => v.asInstanceOf[A]
    }
    def modify[A](ref:Ref[A], f:A=>A) : St = mem.get(ref) match {
      case None => throw new IllegalArgumentException("reference from wrong state (or from future version of it)")
      case Some(v) => new St(mem.updated(ref, f(v.asInstanceOf[A])))
    }
    def put[A](ref:Ref[A], a:A) : St = modify(ref, (_:A) => a)
    def newRef[A](a:A) : (St,Ref[A]) = {
      val ref = new Ref[A]()
      (new St(mem.updated(ref,a)), ref)
    }
  }
  def newRef0[A](a:A) : State[St,Ref[A]] = for {
    st <- State.get[St]
    (st2,ref) = st.newRef(a)
    _ <- State.put(st2)
  } yield ref

  def newRef[M[_],A](a:A)(implicit applicative : Applicative[M]) = newRef0(a).lift[M]
  final class Ref[A] private[RefState] () {
    def put0(a:A) : State[St,Unit] = State.modify( st => st.put(this,a) )
    def put[M[_]](a:A)(implicit applicative : Applicative[M]) : T[M,Unit] = put0(a).lift[M]
    def modify0(f:A=>A) : State[St,Unit] = State.modify( st => st.modify(this,f) )
    def modify[M[_]](f:A=>A)(implicit applicative : Applicative[M]) : T[M,Unit] = modify0(f).lift[M]
    def get0 : State[St,A] = State.get[St].map(_.get(this))
    def get[M[_]](implicit applicative : Applicative[M]) = get0.lift[M]
  }
}
