package isabelle

import java.io.File
import java.nio.file.Paths
import java.rmi.{Remote, RemoteException}
import java.rmi.registry.{LocateRegistry, Registry}
import java.rmi.server.UnicastRemoteObject

import cmathml.Apply.Extractor
import cmathml.CMathML.{arith1, fns1, relation1}
import cmathml._
import com.sun.xml.internal.stream.buffer.{MutableXMLStreamBuffer, XMLStreamBufferResult}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finagle.{ChannelException, Http, Service, http}
import info.hupel.isabelle.api.XML.Tree
import info.hupel.isabelle._
import info.hupel.isabelle.api.{Environment, Version, XML}
import info.hupel.isabelle.japi.Codecs
import info.hupel.isabelle.pure.{Abs, App, Bound, Const, Free, Term, Typ, Type}
import info.hupel.isabelle.setup.{Resources, Setup}
import isabelle.Isabelle.types.dummy

import BigInt._
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.language.postfixOps

///** Functions that should be accessible via RMI */
//trait IsabelleRemoteCore extends Remote {
////  @throws(classOf[RemoteException]) def dispose():Unit
//  @throws(classOf[RemoteException])
////  def invokeRaw(opname:String, input:String) :
//  /* TODO private[isabelle] */ def invoke[I,O](op:Operation[I,O],input:I) : Future[O]
//}

abstract class Isabelle {
  def dispose() : Unit

  def invoke[I,O](op:(String,Codec[I],Codec[O]),input:I) : Future[O]

  override def finalize(): Unit = {
    dispose()
    super.finalize()
  }

  private val simplifyTermOperation = ("simplify_term",implicitly[Codec[Term]],implicitly[Codec[Term]]) //Operation.implicitly[Term,Term]("simplify_term")
  def simplifyTermFuture(term:Term) = invoke(simplifyTermOperation,term)
  def simplifyTerm(term:Term) = Await.result(simplifyTermFuture(term),Inf)

  private val typeInferenceOperation = ("type_inference",implicitly[Codec[Term]],implicitly[Codec[Term]]) // Operation.implicitly[Term,Term]("type_inference")
  def typeInferenceFuture(term:Term) = invoke(typeInferenceOperation,term)
  def typeInference(term:Term) = Await.result(typeInferenceFuture(term),Inf)

  private val termToStringOperation = ("term_to_string",implicitly[Codec[Term]],implicitly[Codec[String]])  // Operation.implicitly[Term,String]("term_to_string")
  def termToStringFuture(term:Term) = invoke(termToStringOperation,term)
  def termToString(term:Term) = Await.result(termToStringFuture(term),Inf)

  private val pingOperation = ("ping",implicitly[Codec[Unit]],implicitly[Codec[Unit]])
  def pingFuture() = invoke(pingOperation,())
  def ping() = Await.result(pingFuture(),Inf)
}

class IsabelleLocal(env:IsabelleEnvironment = IsabelleEnvironment.defaultEnvironment) extends Isabelle {
  val systemFuture = for { e<-env.environment; sys <- System.create(e,env.config) } yield sys
  lazy val system = Await.result(systemFuture,Inf)

  def dispose() = for (sys<-systemFuture) sys.dispose

  def invoke[I,O](op:(String,Codec[I],Codec[O]),input:I) : Future[O] = {
    val (name,inCodec,outCodec) = op
    val op2 = Operation.simple(name,inCodec,outCodec)
    for {
      sys <- systemFuture
      result <- sys.invoke(op2)(input)
    } yield result.unsafeGet
  }
}

object IsabelleServer {
  object YXMLCodec extends Codec[String] {
    override val mlType: String = "invalid"
    override def encode(t: String): Tree = XML.fromYXML(t)
    override def decode(tree: Tree): XMLResult[String] = Right(tree.toYXML)
  }
  def main(args: Array[String]): Unit = {
    import com.twitter.bijection.twitter_util.UtilBijections._
    import com.twitter.bijection.Conversion.asMethod

    println("Starting Isabelle")
    val isabelle = new IsabelleLocal()
    val codec = Codecs.TREE
    val service = new Service[http.Request, http.Response] {
      override def apply(request: Request): com.twitter.util.Future[Response] = {
        println(request)
        val opname = request.getParam("op")
        println(s"Request $opname")
//        val input = request.getParam("input")
        val input = request.contentString
//        print("content",input)
        val outputFuture = isabelle.invoke((opname,YXMLCodec,YXMLCodec),input)
        for (output <- outputFuture.as[com.twitter.util.Future[String]])
          yield {
            val res = request.response
            res.contentString = output
            res
          }
      }
    }
    println("Starting server")
    val server = Http.server.serve("localhost:12344",service)
    com.twitter.util.Await.ready(server)
    println("Server stopped")
  }
}

class IsabelleRemote extends Isabelle {
//  val registry = LocateRegistry.getRegistry
//  val remote = registry.lookup(IsabelleRemote.serviceName).asInstanceOf[IsabelleRemoteCore]

  override def dispose(): Unit = ()

  val client = Http.client.newService(":12344")

  override def invoke[I, O](op: (String,Codec[I],Codec[O]), input: I): Future[O] = {
    import com.twitter.bijection.twitter_util.UtilBijections._
    import com.twitter.bijection.Conversion.asMethod

    val (opname,inCodec,outCodec) = op
    val input2 = inCodec.encode(input)
    val req = http.Request(s"http://localhost:12344/", "op" -> opname)
    req.method = Method.Post
    req.contentString = input2.toYXML
//    println(req)
    val response = client(req).map { res =>
      val content = res.contentString
      val tree = XML.fromYXML(content)
      outCodec.decode(tree) match {
        case Right(out) => out
      }
    }
    response.as[Future[O]]
  }
//    remote.invoke(op,input)
}
//
//object IsabelleRemote {
////  val defaultPort = 32135
//  val serviceName = "isabelle-server"
//}
//
//trait IsabelleServerInterface extends Remote {
//
//}

//object IsabelleServer extends IsabelleServerInterface {
//  println("Starting Isabelle Server")
//
//  def runServer(): Unit = {
//    import sys.process._
////    if (java.lang.System.getSecurityManager == null) {
////      println("Setting security manager")
////      java.lang.System.setSecurityManager(new SecurityManager())
////    }
//    try {
//      Process("rmiregistry",new File("target/scala-2.11/classes/")).run
//      Thread.sleep(1000)
//      val name = "Compute"
//      val stub = UnicastRemoteObject.exportObject(this, 0)
//      val registry = LocateRegistry.getRegistry
//      registry.rebind(IsabelleRemote.serviceName,stub)
//      println("Isabelle server started")
//      while (true)
//        Thread.sleep(1000000000)
//    } catch {
//      case e: Exception =>
//        println("Isabelle server exception:")
//        e.printStackTrace()
//        sys.exit(1)
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//    runServer()
//  }
//}

object Isabelle {
  lazy val defaultInstance =
    try {
      val isabelle = new IsabelleRemote()
      isabelle.ping()
      isabelle
    } catch {
      case e:ChannelException =>
        println("exn",e)
        new IsabelleLocal()
    }


  class TypeExtractor(name:String) {
    def unapplySeq(typ:Typ) : Option[Seq[Typ]] = typ match {
      case Type(`name`,args) => Some(args)
      case _ => None
    }
  }

  object types {
    val bool = Type("HOL.bool",Nil)
    val dummy = Type("dummy",Nil)
    val int = Type("Int.int",Nil)
    val nat = Type("Nat.nat",Nil)
    val num = Type ("Num.num", Nil)
    val real = Type ("Real.real", Nil)
    def fun(t1:Typ,t2:Typ) = Type("fun",List(t1,t2))
    val funE = new TypeExtractor(fun(dummy,dummy).name)
  }

  class AppExtractor1(const:String) {
    def unapply(t:App) = t match {
      case App(Const(`const`,_),t1) => Some(t1)
      case _ => None
    }
  }

  class AppExtractor2(const:String) {
    def unapply(t:App) = t match {
      case App(App(Const(`const`,_),t1),t2) => Some((t1,t2))
      case _ => None
    }
  }

  class ConstExtractorT(const:String) {
    def unapply(t:Const) = t match {
      case Const(`const`,typ) => Some(typ)
      case _ => None
    }
  }

  class ConstExtractor(const:String) {
    def unapply(t:Const) = t match {
      case Const(`const`,typ) => true
      case _ => false
    }
  }

  object consts {
    import types._
    val `true` = Const("HOL.True",bool)
    val `false` = Const("HOL.False",bool)

    val disj = Const("HOL.disj",fun(bool,fun(bool,bool)))
    def disj(t1:Term,t2:Term) : App = App(App(disj,t1),t2)

    def plus(typ:Typ) : Const = Const("Groups.plus_class.plus",fun(typ,fun(typ,typ)))
    val plus : Const = plus(dummy)
    def plus(t1:Term,t2:Term) : App = App(App(plus,t1),t2)
    def plus(typ:Typ,t1:Term,t2:Term) : App = App(App(plus(typ),t1),t2)
    val plusE = new AppExtractor2(plus.name)

    def minus(typ:Typ) : Const = Const("Groups.minus_class.minus",fun(typ,fun(typ,typ)))
    val minus : Const = minus(dummy)
    def minus(t1:Term,t2:Term) : App = App(App(minus,t1),t2)
    def minus(typ:Typ,t1:Term,t2:Term) : App = App(App(minus(typ),t1),t2)
    val minusE = new AppExtractor2(minus.name)

    def times(typ:Typ) : Const = Const("Groups.times_class.times",fun(typ,fun(typ,typ)))
    val times : Const = times(dummy)
    def times(t1:Term,t2:Term) : App = App(App(times,t1),t2)
    def times(typ:Typ,t1:Term,t2:Term) : App = App(App(times(typ),t1),t2)
    val timesE = new AppExtractor2(times.name)

    def divide(typ:Typ) : Const = Const("Rings.divide_class.divide",fun(typ,fun(typ,typ)))
    val divide : Const = divide(dummy)
    def divide(t1:Term,t2:Term) : App = App(App(divide,t1),t2)
    def divide(typ:Typ,t1:Term,t2:Term) : App = App(App(divide(typ),t1),t2)
    val divideE = new AppExtractor2(divide.name)

    def powr(typ:Typ) : Const = Const("Transcendental.powr",fun(typ,fun(typ,typ)))
    val powr : Const = powr(dummy)
    def powr(t1:Term,t2:Term) : App = App(App(powr,t1),t2)
    def powr(typ:Typ,t1:Term,t2:Term) : App = App(App(powr(typ),t1),t2)
    val powrE = new AppExtractor2(powr.name)

    def power(typ:Typ) : Const = Const("Power.power_class.power",fun(typ,fun(typ,typ)))
    val power : Const = power(dummy)
    def power(t1:Term,t2:Term) : App = App(App(power,t1),t2)
    def power(typ:Typ,t1:Term,t2:Term) : App = App(App(power(typ),t1),t2)
    val powerE = new AppExtractor2(power.name)

    val num_one = Const("Num.num.One", num)
    val num_oneE = new ConstExtractor(num_one.name)
    def zero(typ:Typ) = Const ("Groups.zero_class.zero", typ)
    val zero : Const = zero(dummy)
    val zeroET = new ConstExtractorT(zero.name)
    def one(typ:Typ) = Const ("Groups.one_class.one", typ)
    val one : Const = one(dummy)
    val oneET = new ConstExtractorT(one.name)

    def ten(typ:Typ) : Term = mk_number(typ,10)
    lazy val ten : Term = ten(dummy)
    
    val bit0 = Const("Num.num.Bit0", fun(num,num))
    val bit0E = new AppExtractor1(bit0.name)
    def bit0(t:Term) : App = App(bit0,t)
    val bit1 = Const ("Num.num.Bit1", fun(num,num))
    def bit1(t:Term) : App = App(bit1,t)
    val bit1E = new AppExtractor1(bit1.name)
    def numeral(typ:Typ) : Const = Const("Num.numeral_class.numeral", fun(num,typ))
    def numeral(typ:Typ, t:Term) : App = App(numeral(typ),t)
    val numeral : Const = numeral(dummy)
    object numeralET {
      def unapply(t:Term) : Option[(Typ,Term)] = t match {
        case App(Const(numeral.name,funE(_,typ)),t2) => Some((typ,t2))
        case App(Const(numeral.name,_),t2) => Some((dummy,t2))
        case _ => None
      }
    }
    def numeral(t:Term) : App = App(numeral,t)

    def uminus(typ: Typ) = Const("Groups.uminus_class.uminus", fun(typ,typ))
    def uminus(typ: Typ, t: Term) : App = App(uminus(typ),t)
    def uminus(t: Term) : App = uminus(dummy,t)
    val uminus : Const = uminus(dummy)
    val uminusE = new AppExtractor1(uminus.name)

    def equal(typ:Typ) : Const = Const("HOL.eq", fun(typ,fun(typ,bool)))
    def equal(typ:Typ, t1: Term, t2: Term) : App = App(App(equal(typ),t1),t2)
    def equal(t1: Term, t2: Term) : App = App(App(equal,t1),t2)
    val equal : Const = equal(dummy)
    val equalE = new AppExtractor2(equal.name)
  }

  def mk_numeral(i:BigInt) = {
    assert(i>0)
    def mk(i:BigInt): Term =
      if (i==1) consts.num_one
      else {
        val t2 = mk(i>>1)
        if (i.testBit(0))
          consts.bit1(t2)
        else
          consts.bit0(t2)
      }
    mk(i)
  }

  object Numeral {
    import consts._
    def unapply(t:Term) : Option[BigInt] = t match {
      case num_oneE() => Some(1)
      case bit0E(Numeral(n)) => Some(n<<1)
      case bit1E(Numeral(n)) => Some((n<<1)+1)
      case _ => None
    }
  }



  //  fun dest_num (Const ("Num.num.One", _)) = 1
//  | dest_num (Const ("Num.num.Bit0", _) $ bs) = 2 * dest_num bs
//    | dest_num (Const ("Num.num.Bit1", _) $ bs) = 2 * dest_num bs + 1
//  | dest_num t = raise TERM ("dest_num", [t]);

//  fun add_numerals (Const ("Num.numeral_class.numeral", Type (_, [_, T])) $ t) = cons (t, T)
//  | add_numerals (t $ u) = add_numerals t #> add_numerals u
//  | add_numerals (Abs (_, _, t)) = add_numerals t
//    | add_numerals _ = I;

  def mk_number(typ:Typ, i:BigInt) : Term =
    if (i==0) consts.zero(typ)
    else if (i==1) consts.one(typ)
    else if (i>0)
      consts.numeral(typ,mk_numeral(i))
    else
      consts.uminus(typ,mk_number(typ,-i))

  object NumberT {
    import consts._
    def unapply(t:Term) : Option[(Typ,BigInt)] = t match {
      case zeroET(typ) => Some((typ,0:BigInt))
      case oneET(typ) => Some((typ,1:BigInt))
      case numeralET(typ,Numeral(n)) => Some((typ,n))
      case uminusE(NumberT(typ,n)) => Some((typ,-n))
      case _ => None
    }
  }

  object Number {
    def unapply(t:Term): Option[BigInt] = t match {
      case NumberT(_,n) => Some(n)
      case _ => None
    }
  }

//  fun dest_number (Const ("Groups.zero_class.zero", T)) = (T, 0)
//  | dest_number (Const ("Groups.one_class.one", T)) = (T, 1)
//  | dest_number (Const ("Num.numeral_class.numeral", Type ("fun", [_, T])) $ t) =
//  (T, dest_num t)
//  | dest_number (Const ("Groups.uminus_class.uminus", Type ("fun", [_, T])) $ t) =
//  apsnd (op ~) (dest_number t)
//  | dest_number t = raise TERM ("dest_number", [t]);



  /**
    * @param bound variable x has deBrujn index boundShift-bound(x)
    */
  def fromCMathML(math:CMathML, bound:Map[String,Int]=Map.empty, boundShift:Int=0) : Term = {
    def f(math:CMathML) = fromCMathML(math,bound,boundShift)
    math match {
      case CN(_,n) =>
        n.toBigIntExact() match {
          case Some(i) => mk_number(types.real, i)
          case None =>
            val nominator = mk_number(types.real, n.bigDecimal.unscaledValue() : BigInt)
            val scale = n.scale
            assert(scale>0)
            val denominator = scale match {
              case 1 => consts.ten(types.real)
              case e => consts.power(consts.ten,mk_number(types.nat, e))
            }
            consts.divide(nominator,denominator)
        }
      case CI(_,name) =>
        bound.get(name) match {
          case Some(i) => Bound(boundShift-i)
          case None => Free(name, dummy)
        }
      case arith1.plusE(x,y) => consts.plus(f(x),f(y))
      case arith1.minusE(x,y) => consts.minus(f(x),f(y))
      case arith1.timesE(x,y) => consts.times(f(x),f(y))
      case arith1.divideE(x,y) => consts.divide(f(x),f(y))
      case arith1.powerE(x,y) => consts.powr(f(x),f(y))
      case arith1.uminusE(x) => consts.uminus(f(x))
      case relation1.equalE(x,y) => consts.equal(f(x),f(y))
      case fns1.lambdaE(Seq(CI(_,name)),body) =>
        Abs(name, dummy, fromCMathML(body,bound.updated(name,boundShift+1),boundShift+1))
      case Apply(_,CSymbol(_,cd,name),_*) =>
        sys.error(s"Unknown symbol $cd.$name")
      case Bind(_,CSymbol(_,cd,name),_,_) =>
        sys.error(s"Unknown binder $cd.$name")
    }
  }

  def toCMathML(term:Term, bound:List[String]=Nil) : CMathML = {
    import consts._
    def f(term:Term) = toCMathML(term,bound)
    term match {
      case plusE(x,y) => arith1.plus(f(x),f(y))
      case minusE(x,y) => arith1.minus(f(x),f(y))
      case timesE(x,y) => arith1.times(f(x),f(y))
      case powrE(x,y) => arith1.power(f(x),f(y))
      case equalE(x,y) => relation1.equal(f(x),f(y))
      case divideE(Number(nominator),Number(denom)) if denom==10 => CN(BigDecimal(nominator,1,CN.MATHCONTEXT))
      case divideE(Number(nominator),powerE(Number(tn),Number(exp))) if tn==10 && exp.isValidInt && exp >= 0 =>
        CN(BigDecimal(nominator,exp.toInt,CN.MATHCONTEXT))
      case divideE(x,y) => arith1.divide(f(x),f(y)) // must be after other divideE-patterns
      case Number(n) => CN(n)
      case uminusE(x) => arith1.uminus(f(x)) // must be after Number
      case Free(name,_) => CI(name)
      case Const(name,_) => sys.error(s"Unknown symbol $name")
    }
  }
}

class IsabelleEnvironment(version : String) {
  private val resourceDirectory = Paths.get(getClass.getClassLoader.getResource("isabelle/").toURI)
  private val resources = new Resources(resourceDirectory)
  val config = resources.makeConfiguration(List(), "ProofEditorSupport")
  val environment : Future[Environment] = Future {
    IsabelleEnvironment.Lock.synchronized {
      val setupFuture = Setup.defaultSetup(Version(version)).fold({msg => throw new RuntimeException(msg.explain)},{fu=>fu})
      val setup = Await.result(setupFuture,Inf)
      val environment = Await.result(setup.makeEnvironment,Inf)
      val built = System.build(environment,config)
      assert(built)
      environment
    }
  }
}

object IsabelleEnvironment {
  private object Lock
  val defaultVersion = "2016"
  lazy val defaultEnvironment = new IsabelleEnvironment(defaultVersion)
}
