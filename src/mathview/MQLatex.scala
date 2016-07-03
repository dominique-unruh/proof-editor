package mathview

import mathview.MQLatexParser.{MinusContext, NumberContext, PlusContext, TimesContext}
import misc.{Apply, CMathML, CN, CSymbol}
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import misc.Pure
import scala.collection.mutable

/**
  * Created by unruh on 7/4/16.
  */
object MQLatex {

  object Ast {
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
  def pp(ast:ParseTree) : CMathML = ast match {
    //    case _ : PlusContext => ast.
    case Ast(_:PlusContext,x,_,y) => Apply(CSymbol("arith1","plus"),pp(x),pp(y))
    case Ast(_:MinusContext,x,_,y) => Apply(CSymbol("arith1","minus"),pp(x),pp(y))
    case Ast(_:TimesContext,x,_,y) => Apply(CSymbol("arith1","times"),pp(x),pp(y))
    case _:NumberContext => CN(BigDecimal(ast.getText))
  }

  @misc.Pure
  def parseLatex(tex:String) : CMathML = {
    val stream = new ANTLRInputStream("3+4")
    val lexer = new MQLatexLexer(stream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new MQLatexParser(tokens)
    val tree = parser.math
    pp(tree)
  }

}
