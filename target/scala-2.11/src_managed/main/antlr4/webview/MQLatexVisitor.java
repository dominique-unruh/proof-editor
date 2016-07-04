// Generated from /home/unruh/svn/proof-editor/src/main/antlr4/MQLatex.g4 by ANTLR 4.5.3
package webview;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link MQLatexParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface MQLatexVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code plus}
	 * labeled alternative in {@link MQLatexParser#math}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPlus(MQLatexParser.PlusContext ctx);
	/**
	 * Visit a parse tree produced by the {@code minus}
	 * labeled alternative in {@link MQLatexParser#math}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMinus(MQLatexParser.MinusContext ctx);
	/**
	 * Visit a parse tree produced by the {@code times}
	 * labeled alternative in {@link MQLatexParser#math}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTimes(MQLatexParser.TimesContext ctx);
	/**
	 * Visit a parse tree produced by the {@code braces}
	 * labeled alternative in {@link MQLatexParser#math}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBraces(MQLatexParser.BracesContext ctx);
	/**
	 * Visit a parse tree produced by the {@code number}
	 * labeled alternative in {@link MQLatexParser#math}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumber(MQLatexParser.NumberContext ctx);
}