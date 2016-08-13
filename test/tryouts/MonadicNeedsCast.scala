import scalaz._
import Scalaz._
import com.thoughtworks.each.Monadic._

object MonadicNeedsCast {
  def main(args: Array[String]): Unit = {
    val monad = monadic[Option] {
      None.each // Normally, this would be inside an if and represent a failure of the computation
//      (None:Option[Unit]).each // This one works
      55
    }
    println(monad) // Should be None
  }
}
