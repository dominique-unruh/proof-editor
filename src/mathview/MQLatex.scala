package mathview

import java.util

import cmathml._
import jdk.internal.org.xml.sax.ErrorHandler
import jdk.nashorn.internal.runtime.regexp.joni.exception.SyntaxException
import mathview.MQLatexParser._
import misc._
import misc.Pure
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, RuleNode, TerminalNode}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import scala.collection.mutable

private[mathview] object MQLatex {
  private object Ast {
    def unapplySeq(ast: ParseTree): Option[(ParseTree,Seq[ParseTree])] = {
      Some((ast,new mutable.IndexedSeqView[ParseTree,Any] {
        override def update(idx: Int, elem: ParseTree): Unit = throw new AbstractMethodError("immutable")
        override def length: Int = ast.getChildCount
        override def apply(idx: Int): ParseTree = ast.getChild(idx)
        override protected def underlying: Any = throw new AbstractMethodError("?")
      }))
    }
  }

//  @Pure
//  private def pp(ast:ParseTree) : CMathML = ast match {
//    case ast : ParserRuleContext if ast.exception!=null => throw ast.exception // CError("unruh","parseerror",ast.exception)
//    case Ast(_:PlusContext,x,_,y) => Apply(CSymbol("arith1","plus"),pp(x),pp(y))
//    case Ast(_:MinusContext,x,_,y) => Apply(CSymbol("arith1","minus"),pp(x),pp(y))
//    case Ast(_:TimesContext,x,_,y) => Apply(CSymbol("arith1","times"),pp(x),pp(y))
//    case Ast(_:JuxtaposTimesContext,x,y) => Apply(CSymbol("arith1","times"),pp(x),pp(y))
//    case Ast(_:FracContext,_,_,x,_,_,y,_) => Apply(CSymbol("arith1","divide"),pp(x),pp(y))
//    case Ast(_:BracesContext,_,x,_) => pp(x)
//    case Ast(_:ParensContext,_,_,x,_,_) => pp(x)
//    case _:NumberContext => CN(BigDecimal(ast.getText))
//    case _:VariableContext => CI(ast.getText)
//    case _ => sys.error("unexpected")
//  }

  @Pure
  def parseLatex(tex:String) : CMathML = {
    val stream = new ANTLRInputStream(tex)
    val lexer = new MQLatexLexer(stream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new MQLatexParser(tokens)
    parser.setErrorHandler(new BailErrorStrategy)
    val tree = parser.math_eof()
    tree.cmathml
  }

  /** Used by the generated parser */
  @Pure @annotation.varargs
  def op(cd:String,name:String,args:CMathML*): CMathML = {
import org.antlr.v4.runtime.{Parser, RecognitionException, Recognizer}
    assert(cd!=null); assert(name!=null)
    for (a <- args) assert(a!=null,"parser produced null CMathML")
    Apply(CSymbol(cd,name),args:_*)
  }

  @Pure
  private def renderList(args:Seq[CMathML],path:PathRev,options:Options) : Seq[String] =
    args.zipWithIndex.map {case(a,i) => cmathmlToLatex(a,path.append(i+1),options)}

  @Pure
  private def cmathmlToLatexApplyDefault(hd:String,args:Seq[String]) : String =
    s"$hd(${args.mkString(",")})"

  val applyRenderers : Map[(String,String),Seq[String] => String] = Map(
    ("arith1","plus") -> {case Seq(x,y) => s"\\left({$x}+{$y}\\right)"},
    ("arith1","minus") -> {case Seq(x,y) => s"\\left({$x}-{$y}\\right)"},
    ("arith1","times") -> {case Seq(x,y) => s"\\left({$x}\\cdot{$y}\\right)"},
    ("arith1","divide") -> {case Seq(x,y) => s"\\frac{$x}{$y}"},
    ("arith1","power") -> {case Seq(x,y) => s"\\left({$x}^{$y}\\right)"},
    ("relation1","eq") -> {case Seq(x,y) => s"\\left({$x}={$y}\\right)"}
  )

  @Pure
  def addPath(tex:String,path:PathRev,options:Options) : String = if (options.edit) tex else s"\\class{path-$path}{$tex}"

  case class Options(editAt:Option[PathRev]=None, edit:Boolean=false)

  @Pure
  def cmathmlToLatex(m:CMathML, path:PathRev, options:Options) : String =
    if (!options.edit && options.editAt.contains(path)) {
      val tex = cmathmlToLatex(m,path,options.copy(edit=true))
      s"\\MathQuillMathField[edit]{$tex}"
    } else {
      m match {
      case CN(n) => addPath(if (options.edit) n.toString else s"\\class{number}{$n}",path,options)
      case CI(v) => addPath(if (options.edit) v else s"\\class{variable}{$v}",path,options)
      case CSymbol(cd,name) => if (options.edit) s"\\text{$cd.$name}}" else s"\\class{symbol}{\\text{$cd.$name}}"
      case Apply(hd,args @ _*) =>
        val renderer = hd match { case CSymbol(cd,name) => applyRenderers.get((cd,name)); case _ => None }
        val renderedArgs = renderList(args,path,options)
        val result : Option[String] = renderer match { case None => None;
        case Some(r) => try { Some(r(renderedArgs)) } catch { case _ : MatchError => None } }
        val result2 : String = result match {
          case None => cmathmlToLatexApplyDefault(cmathmlToLatex(hd,path.append(0),options),renderedArgs)
          case Some(r) => r }
        addPath(result2,path,options)
      case CError(_,_,_*) => addPath("\\embed{error}[]",path,options);
      }
    }

  @Pure
  def cmathmlToLatex(m:CMathML, options:Options=Options()) : String = cmathmlToLatex(m,PathRev.empty,options)
}
