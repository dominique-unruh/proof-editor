package mathview

import cmathml._
import mathview.MQLatexParser._
import misc._
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, RuleNode, TerminalNode}
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream, ParserRuleContext}

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

  @Pure
  private def pp(ast:ParseTree) : CMathML = ast match {
    case Ast(_:PlusContext,x,_,y) => Apply(CSymbol("arith1","plus"),pp(x),pp(y))
    case Ast(_:MinusContext,x,_,y) => Apply(CSymbol("arith1","minus"),pp(x),pp(y))
    case Ast(_:TimesContext,x,_,y) => Apply(CSymbol("arith1","times"),pp(x),pp(y))
    case _:NumberContext => CN(BigDecimal(ast.getText))
    case _:VariableContext => CI(ast.getText)
    case ast : ParserRuleContext if ast.exception!=null => System.out.println(ast.exception); CError("unruh","parseerror",ast.exception)
    case _ => sys.error("unexpected")
  }

  @misc.Pure
  def parseLatex(tex:String) : CMathML = {
    val stream = new ANTLRInputStream(tex)
    val lexer = new MQLatexLexer(stream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new MQLatexParser(tokens)
    val tree = parser.math
    pp(tree)
  }


  @Pure
  private def renderList(args:Seq[CMathML],path:PathRev,options:Options) : Seq[String] =
    args.zipWithIndex.map {case(a,i) => cmathmlToLatex(a,path.append(i+1),options)}

  @Pure
  private def cmathmlToLatexApplyDefault(hd:String,args:Seq[String]) : String =
    s"$hd(${args.mkString(",")})"

  val applyRenderers : Map[(String,String),Seq[String] => String] = Map(
    ("arith1","plus") -> {case Seq(x,y) => s"{$x}+{$y}"},
    ("arith1","minus") -> {case Seq(x,y) => s"{$x}-{$y}"}
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
      }
    }

        @Pure
  def cmathmlToLatex(m:CMathML, options:Options=Options()) : String = cmathmlToLatex(m,Path.emptyRev,options)
}
