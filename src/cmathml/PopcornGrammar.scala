package cmathml

import java.math.BigInteger

import cmathml.CMathML._
import cmathml.PopcornLiterals._
import fastparse.WhitespaceApi
import fastparse.noApi._
import misc.Log

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object PopcornGrammar {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    val comment = "/*" ~ ((!"*/") ~ AnyChar).rep ~ "*/"
    NoTrace((CharPred(_.isWhitespace)|comment).rep)
  }
  import White._

  private def stripID(str:String): String = {
    if (str(0)!='"') str
    else StringContext.treatEscapes(str.substring(1,str.length-1))
  }

  type P = Parser[CMathML]

  def parse(popcorn: String) : CMathML = exprEof.parse(popcorn) match {
    case Parsed.Success(m,_) => m
    case fail : Parsed.Failure => throw ParseError(fail)
  }

  lazy val exprEof : P = P( expr ~ End )

  lazy val expr = blockExpr

  lazy val blockExpr: P = P ( assignExpr ~ (";" ~/ assignExpr).rep ).map
    { case (e,Seq()) => e
      case (e,es) => prog1.block(e+:es : _*) }

  lazy val assignExpr : P = P( implExpr ~ (":=" ~/ implExpr).? ).map
    { case (x,Some(y)) => prog1.assign(x,y)
      case (x,None) => x }

  lazy val implExpr: P = P ( orExpr ~ (("==>" | "<=>").! ~/ orExpr).? ).map
    { case (x,None) => x
      case (x,Some(("==>",y))) => logic1.implies(x,y)
      case (x,Some(("<=>",y))) => logic1.equivalent(x,y)
      case (_,Some((op,_))) => sys.error("Unexpected "+op) }

  lazy val orExpr : P = P ( andExpr ~ ("or" ~/ andExpr).rep ).map
    { case (x,Nil) => x
      case (x,xs) => logic1.or(x+:xs : _*) }

  lazy val andExpr : P = P ( relExpr ~ ("and" ~/ relExpr).rep ).map
    { case (x,Nil) => x
      case (x,xs) => logic1.and(x+:xs : _*) }

  lazy val relExpr : P = P ( intervalExpr ~ (("="|"<"|"<="|">"|">="|"!="|"<>").! ~/ intervalExpr).? ).map
    { case (x,None) => x
      case (x,Some(("=",y))) => relation1.equal(x,y)
      case (x,Some((">",y))) => relation1.gt(x,y)
      case (x,Some(("<",y))) => relation1.lt(x,y)
      case (x,Some((">=",y))) => relation1.geq(x,y)
      case (x,Some(("<=",y))) => relation1.leq(x,y)
      case (x,Some(("!="|"<>",y))) => relation1.neq(x,y)
      case (x,Some((op,_))) => sys.error("Unexpected "+op)
    }


  lazy val intervalExpr : P = P( addExpr ~ (".." ~/ addExpr).? ).map
    { case (x,None) => x
      case (x,Some(y)) => interval1.interval(x,y) }

  /** Consecutive + are grouped into an n-ary +, but - is binary */
  lazy val addExpr : P = P( multExpr ~ (CharIn("+-").! ~/ multExpr).rep ).map
    { case (x,pm) =>
        val plusList = ListBuffer(x)
        def doPlus() = {
          val m = if (plusList.length > 1) arith1.plus(plusList.toList: _*) else plusList.head
          plusList.clear()
          m
        }
        for ((op,m) <- pm) op match {
          case "+" => plusList += m
          case "-" => plusList += arith1.minus(doPlus(),m)
        }
        doPlus()
    }

  /** Consecutive * are grouped into an n-ary *, but / is binary */
  lazy val multExpr : P = P( powerExpr ~ (CharIn("*/").! ~/ powerExpr).rep ).map
    { case (x,pm) =>
      val timesList = ListBuffer(x)
      def doTimes() = {
        val m = if (timesList.length > 1) arith1.times(timesList.toList: _*) else timesList.head
        timesList.clear()
        m
      }
      for ((op,m) <- pm) op match {
        case "*" => timesList += m
        case "/" => timesList += arith1.divide(doTimes(),m)
      }
      doTimes()
    }

  lazy val powerExpr : P = P( complexExpr ~ ("^" ~/ complexExpr).? ).map
    { case (x,None) => x
      case (x,Some(y)) => arith1.power(x,y) }

  lazy val complexExpr : P = P( rationalExpr ~ ("|" ~/ rationalExpr).? ).map
    { case (x,None) => x
      case (x,Some(y)) => complex1.complex_cartesian(x,y) }

  lazy val rationalExpr : P = P( negExpr ~ ("//" ~/ negExpr).? ).map
    { case (x,None) => x
      case (x,Some(y)) => nums1.rational(x,y) }

  lazy val negExpr : P = P( negativeDecimal | ("-" | "not").!.? ~ compExpr ).map
    { case x : CMathML => x
      case (None,x : CMathML) => x
      case (Some("-"), x : CMathML) => arith1.uminus(x)
      case (Some("not"), x : CMathML) => logic1.not(x) }

  lazy val compExpr : P = P( paraExpr | NoCut(call) | NoCut(ecall) | NoCut(attribution) | NoCut(binding) | listExpr | setExpr | anchor ) // TODO: this does unnecessary backtracking for ecall, attribution, binding, anchor


  lazy val commaList : Parser[Seq[CMathML]] = P( expr.rep(sep=",") )

  lazy val varCommaList : Parser[Seq[CILike]] = P( variable.rep(sep=",") )

  lazy val call : P = P( anchor ~ "(" ~/ commaList ~ ")" ).map
    { case (x,args) => Apply(x,args :_*) }

  lazy val ecall : P = P( anchor ~ "!" ~ "(" ~/ commaList ~ ")" ).map
    { case (x,args) => CError(x.asInstanceOf[CSymbol].cd, x.asInstanceOf[CSymbol].name, args :_*) }

  lazy val listExpr : P = P( "[" ~/ commaList ~ "]" ).map
    { args => Apply(list1.list, args :_*) }

  lazy val setExpr : P = P( "{" ~/ commaList ~ "}" ).map
    { args => Apply(set1.set, args :_*) }

  lazy val attribution : P = P ( anchor ~ "{" ~/ attributionList ~ "}" ).map
    { case (m,attr) => m.updateAttributes(attr) }

  lazy val attributionList : Parser[Seq[((String,String),CMathML)]] = P( attributionPair.rep(min=1) )

  lazy val attributionPair : Parser[((String,String),CMathML)] = P( symbol ~ "->" ~/ expr ).map
    { case (sym,arg) => ((sym.cd,sym.name),arg) }

  lazy val binding : P = P( anchor ~ "[" ~/ varCommaList ~ "->" ~/ expr ~ "]" ).map
    { case (hd,vars,body) => Bind(hd,vars,body) }

  lazy val anchor : P = P( atom ~ (":" ~/ id).? ).map
    { case (x,_) => x }  // Ignoring ID (can't be represented)

  lazy val atom : P = P( paraExpr | symbol | shortSymbol | variable | number | ref | PopcornLiterals.omb | PopcornLiterals.string | ifExpr | whileExpr )

  lazy val shortSymbol : Parser[CSymbol] = {
    val abbrevs = CSymbol.popcornAbbrevs.map { case ((cd,name),abbrev) => (abbrev,CSymbol(cd,name)) }
    P( StringIn(abbrevs.keys.toSeq :_*).! ).map(abbrevs)
  }

  lazy val paraExpr : P = P( "(" ~/ expr ~ ")" )

  lazy val ifExpr : P = P( "if" ~/ expr ~ "then" ~/ expr ~ "else" ~/ expr ~ "endif" ).map
    { case (a,b,c) => prog1.`if`(a,b,c) }

  lazy val whileExpr : P = P( "while" ~/ expr ~ "do" ~/ expr ~ "endwhile" ).map
    { case (a,b) => prog1.`while`(a,b) }

  lazy val symbol : Parser[CSymbol] = P( id ~ "." ~/ id ).map
    { case (cd,name) => CSymbol(cd,name) }

  lazy val variable : Parser[CILike] = P ( "$" ~/ id ).map
    { name => CI(stripID(name)) }

  lazy val ref : P = P( ("#" | "##") ~/ id ).map
    { _ => throw new NotImplementedError("references not supported") }

  lazy val number : P = hexint | hexfloat | decimal

  // [1] http://java.symcomp.org/download/org.symcomp-1.5.0-src.zip /org.symcomp-1.5.0-src/openmath/src/main/antlr/org/symcomp/openmath/popcorn/
}

object PopcornLiterals {
  import fastparse.all._
  import PopcornGrammar.P

  val az09_ = CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_")
  val az_ = CharIn('a' to 'z', 'A' to 'Z', "_")
  val digit = CharIn('0' to '9').opaque("digit")
  val hexdigit = CharIn('0' to '9', 'a' to 'f', 'A' to 'F').opaque("hexdigit")

  //  OMB: '%' [a-zA-Z0-9=]+ '%';
  val omb : P = P ( "%" ~/ az09_.rep.! ~ "%").map
    { base64 => CBytes.fromBase64(base64) }

  //  // Following [1] instead of the unclear Popcorn spec
  //  ID: [a-zA-Z_][a-zA-Z0-9_]*  |  '\'' [^\']* '\'' ;
  val id : Parser[String] = P( (az_ ~ az09_.rep).!.opaque("identifier") )

  //  DECINT: [0-9]+;
  //  DECFLOAT: [0-9]+ '.' [0-9]+ ('e' '-'? [0-9]+)? ;
  val decimal : P = P( ("-".? ~ digit.rep(min=1) ~ ("." ~/ digit.rep(min=1)).? ~ ("e" ~/ "-".? ~/ digit.rep(min=1)).?).! ).map
    { str:String => CN(str) }
  val negativeDecimal : P = P( ("-" ~ digit.rep(min=1) ~ ("." ~/ digit.rep(min=1)).? ~ ("e" ~/ "-".? ~/ digit.rep(min=1)).?).! ).map
    { str:String => CN(str) }

  //  HEXINT: '0x' [a-fA-F0-9]+;
  val hexint = P( "0x" ~/ hexdigit.rep(min=1).! ).map
    { hex => CN(new BigInteger(hex,16)) }

  //  HEXFLOAT: '0f' [a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9];
  val hexfloat : P = P( "0f" ~/ hexdigit.rep(min=1).! ).map
    { hex =>
      val bits = java.lang.Long.valueOf(hex,16)
      val double = java.lang.Double.longBitsToDouble(bits)
      CN(double) }

  //  // mix of [1] and spec
  //  STRING: '"' ([^"\\] | '\\"' | '\\\\' | '\\n' | '\\r' | '\\t')* '"';
  val notBackslashOrQuote = P ( !CharIn("\\\"") ~ AnyChar )
  val string : P = P( "\"" ~/ (notBackslashOrQuote | "\\\"" | "\\n" | "\\r" | "\\t").rep.! ~ "\"" ).map
    { str => CS(StringContext.treatEscapes(str)) }
}