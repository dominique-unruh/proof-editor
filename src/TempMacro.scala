import TempMacro.MonadicRefFactory
import com.thoughtworks.each.Monadic
import com.thoughtworks.sde.core.MonadicFactory

import scalaz._
import Scalaz._
import scala.language.higherKinds
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.collection.mutable.{ListBuffer, Stack}
import scala.reflect.internal.{Flags, ModifierFlags}


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


object TempMacro {
  class MonadicRefFactory[M[_]](implicit applicativeM: Applicative[M]) {
    def apply[A](code: => A): RefState.T[M,A] = macro MacroBundle.apply[M[_],A]
  }
  class MacroBundle(val c: Context) {
    import c.universe._

    def removeMutableFlag(flags:FlagSet) : FlagSet = {
      var newFlags = NoFlags
      assert((newFlags | Flag.MUTABLE) == flags)
      newFlags
    }

    def apply[M : WeakTypeTag, A : WeakTypeTag](code: Tree): Tree = {
      val mType = weakTypeTag[M].tpe
      val aType = weakTypeTag[A].tpe

      def instrumentCode(code : Tree) : Tree = code match {
        case Block(stats,expr) =>
          q"{..${stats.map(instrumentCode)};${instrumentCode(expr)}}"
        case c @ q"${Modifiers(flags,privIn,annot)} var $name: $tpe = $init" =>
          val init2 = instrumentCode(init)
          // TODO: add monad stuff
          val mods2 = Modifiers(removeMutableFlag(flags),privIn,annot)
          print(mods2,mods2.getClass)
          q"$mods2 val $name: RefState.Ref[$tpe] = newRef[$mType,$tpe]($init2).each"
        //            q"$mods2 val $name: RefState.Ref[$tpe] = ???"
        case l @ Literal(const) => l
      }

      val code2 = instrumentCode(code)
      //        print(code2)
      val tree = q""" monadic[({type l[A] = RefState.T[$mType,A]})#l] { $code2 } """
      println(tree)
      tree
    }
  }
  object MonadicRefFactory {
//    def apply[A](code: A) : M[A] = macro monadicRef_impl[M,A]
  }
  def monadicRef[M[_]](implicit applicativeM: Applicative[M]) = new MonadicRefFactory[M]




  Monadic.monadic
//  def monadicRef[M[_],A](code:A) : M[A] = macro monadicRef_impl[M,A]





  def printf(format: String, params: Any*): Unit = macro printf_impl
  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._
    val Literal(Constant(s_format: String)) = format.tree
    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = TermName(c.freshName("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }
    val paramsStack = Stack[Tree]((params map (_.tree)): _*)
    val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => precompute(paramsStack.pop, typeOf[Int])
      case "%s" => precompute(paramsStack.pop, typeOf[String])
      case "%%" => Literal(Constant("%"))
      case part => Literal(Constant(part))
    }
    val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
  }
}

