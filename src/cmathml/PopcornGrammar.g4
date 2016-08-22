grammar PopcornGrammar;

@header {
import cmathml.*;
import cmathml.CMathML.*;
import static cmathml.JavaHelpers.*;
import java.util.stream.Stream;
import scala.NotImplementedError;
import scala.StringContext;
}

@parser::members {
	static String stripID(Token token) {
		String txt = token.getText();
		if (txt.charAt(0)!='"') return txt;
		txt = txt.substring(1,txt.length()-1);
		return StringContext.treatEscapes(txt);
	}
}


expr_eof returns [CMathML cmathml]:
    x=expr EOF { $cmathml = $x.m; };

expr returns [CMathML m]:
    x=blockExpr { $m = $x.m; };

blockExpr returns [CMathML m]:
    bs+=assignExpr (';' bs+=assignExpr)+ { $m = apply("prog1", "block", $bs.stream().map(x->x.m)); }
  | x=assignExpr { $m = $x.m; };

assignExpr returns [CMathML m]:
	x=implExpr ':=' y=implExpr { $m = apply("prog1", "assign", $x.m, $y.m); }
  | x=implExpr { $m = $x.m; };

implExpr returns [CMathML m]:
    x=orExpr '==>' y=orExpr { $m = apply("logic1", "implies", $x.m, $y.m); }
  | x=orExpr '<=>' y=orExpr { $m = apply("logic1", "equivalent", $x.m, $y.m); }
  | x=orExpr { $m = $x.m; };

orExpr returns [CMathML m]:
    x=andExpr 'or' y=andExpr { $m = apply("logic1", "or", $x.m, $y.m); }
  | x=andExpr { $m = $x.m; };

andExpr returns [CMathML m]:
    x=relExpr 'and' y=relExpr { $m = apply("logic1", "and", $x.m, $y.m); }
  | x=relExpr { $m = $x.m; };

relExpr returns [CMathML m]:
	x=intervalExpr '=' y=intervalExpr { $m = apply("relation1", "eq", $x.m, $y.m); }
  | x=intervalExpr '<' y=intervalExpr { $m = apply("relation1", "lt", $x.m, $y.m); }
  | x=intervalExpr '<=' y=intervalExpr { $m = apply("relation1", "leq", $x.m, $y.m); }
  | x=intervalExpr '>' y=intervalExpr { $m = apply("relation1", "gt", $x.m, $y.m); }
  | x=intervalExpr '>=' y=intervalExpr { $m = apply("relation1", "geq", $x.m, $y.m); }
  | x=intervalExpr '!=' y=intervalExpr { $m = apply("relation1", "neq", $x.m, $y.m); }
  | x=intervalExpr '<>' y=intervalExpr { $m = apply("relation1", "neq", $x.m, $y.m); }
  | x=intervalExpr { $m = $x.m; };

intervalExpr returns [CMathML m]:
    x=addExpr '..' y=addExpr { $m = apply("interval1", "interval", $x.m, $y.m); }
  | x=addExpr { $m = $x.m; };

addExpr returns [CMathML m]:
    x=multExpr '-' y=multExpr { $m = apply("arith1", "minus", $x.m, $y.m); }
  | x=multExpr '+' y=multExpr { $m = apply("arith1", "plus", $x.m, $y.m); }
  | x=multExpr { $m = $x.m; };

multExpr returns [CMathML m]:
    x=powerExpr '/' y=powerExpr { $m = apply("arith1", "divide", $x.m, $y.m); }
  | x=powerExpr '*' y=powerExpr { $m = apply("arith1", "times", $x.m, $y.m); }
  | x=powerExpr { $m = $x.m; };

powerExpr returns [CMathML m]:
    x=complexExpr '^' y=complexExpr { $m = apply("arith1", "power", $x.m, $y.m); }
  | x=complexExpr { $m = $x.m; };

complexExpr returns [CMathML m]:
    x=rationalExpr '|' y=rationalExpr { $m = apply("complex1", "complex_cartesian", $x.m, $y.m); }
  | x=rationalExpr { $m = $x.m; };

rationalExpr returns [CMathML m]:
    x=negExpr '//' y=negExpr { $m = apply("num1", "rational", $x.m, $y.m); }
  | x=negExpr { $m = $x.m; };

negExpr  returns [CMathML m]:
    '-' x=compExpr  { $m = apply("arith1", "unary_minus", $x.m); }
  | 'not' x=compExpr { $m = apply("logic1", "not", $x.m); }
  | x=compExpr { $m = $x.m; };

compExpr  returns [CMathML m]:
    p=paraExpr { $m = $p.m; }
  | e=ecall { $m = $e.m; }
  | a=attribution { $m = $a.m; }
  | b=binding { $m = $b.m; }
  | l=listExpr { $m = $l.m; }
  | s=setExpr { $m = $s.m; }
  | an=anchor { $m = $an.m; };

commaList returns [Stream<CMathML> ms]:
  | (xs+=expr (',' xs+=expr)*)? { $ms = $xs.stream().map(x -> x.m); };

varCommaList returns [Stream<CILike> ms]:
  | (xs+=var (',' xs+=var)*)? { $ms = $xs.stream().map(x -> x.m); };

call returns [CMathML m]:
    hd=anchor '(' args=commaList ')' { apply($hd.m, $args.ms); };

ecall returns [CMathML m]:
    hd=anchor '!' '(' args=commaList ')' { error((CSymbol)$hd.m, $args.ms); };

listExpr returns [CMathML m]:
    '[' args=commaList ']' { apply("list1", "list", $args.ms); };

setExpr returns [CMathML m]:
    '{' args=commaList '}' { apply("set1", "set", $args.ms); };

/* foreignExpr  returns [CMathML m]:
    '`' FOREIGN '`' { TODO; }; */

attribution returns [CMathML m]:
    x=anchor '{' attr=attributionList '}' { $m = addAttributes($x.m, $attr.attrs); };

attributionList returns [Stream<Pair<CSymbol,CMathML>> attrs]:
    x+=attributionPair (',' x+=attributionPair)* { $attrs = $x.stream().map(x->x.attr); };

attributionPair returns [Pair<CSymbol,CMathML> attr]:
    x=symbol '->' y=expr { $attr = new Pair($x.m,$y.m); };

binding returns [CMathML m]:
    x=anchor '[' v=varCommaList '->' b=expr ']' { $m = bind($x.m, $v.ms, $b.m); };

anchor returns [CMathML m]:
    x=atom (':' ID)? { $m = $x.m; }; // Ignoring ID (can't be represented)

atom returns [CMathML m]:
    p=paraExpr { $m = $p.m; }
  | s=symbol { $m = $s.m; }
  | ss=shortSymbol { $m = $ss.m; }
  | v=var { $m = $v.m; }
  | i=intt { $m = $i.m; }
  | f=floatt { $m = $f.m; }
  | r=ref { $m = $r.m; }
  | b=OMB { if (true) throw new NotImplementedError("not yet implemented"); }
//  | fo=FOREIGN { TODO; }  // Not implemented
  | str=STRING { $m = cs(stripID($str); }
  | ie=ifExpr { $m = $ie.m; }
  | w=whileExpr { $m = $w.m; };

shortSymbol returns [CSymbol m]:
    'cos' { $m = csymbol("transc1", "cos"); }
  | 'cosh' { $m = csymbol("transc1", "cosh"); }
  | 'cot' { $m = csymbol("transc1", "cot"); }
  | 'coth' { $m = csymbol("transc1", "coth"); }
  | 'csc' { $m = csymbol("transc1", "csc"); }
  | 'csch' { $m = csymbol("transc1", "csch"); }
  | 'exp' { $m = csymbol("transc1", "exp"); }
  | 'sec' { $m = csymbol("transc1", "sec"); }
  | 'sech' { $m = csymbol("transc1", "sech"); }
  | 'sin' { $m = csymbol("transc1", "sin"); }
  | 'sinh' { $m = csymbol("transc1", "sinh"); }
  | 'tan' { $m = csymbol("transc1", "tan"); }
  | 'tanh' { $m = csymbol("transc1", "tanh"); }
  | 'abs' { $m = csymbol("arith1", "abs"); }
  | 'root' { $m = csymbol("arith1", "root"); }
  | 'sum' { $m = csymbol("arith1", "sum"); }
  | 'product' { $m = csymbol("arith1", "product"); }
  | 'diff' { $m = csymbol("calculus1", "diff"); }
  | 'int' { $m = csymbol("calculus1", "int"); }
  | 'defint' { $m = csymbol("calculus1", "defint"); }
  | 'pi' { $m = csymbol("nums1", "pi"); }
  | 'e' { $m = csymbol("nums1", "e"); }
  | 'i' { $m = csymbol("nums1", "i"); }
  | 'infinity' { $m = csymbol("nums1", "infinity"); }
  | 'min' { $m = csymbol("minmax1", "min"); }
  | 'max' { $m = csymbol("minmax1", "max"); }
  | 'lambda' { $m = csymbol("fns1", "lambda"); }
  | 'true' { $m = csymbol("logic1", "true"); }
  | 'false' { $m = csymbol("logic1", "false"); }
  | 'binomial' { $m = csymbol("combinat1", "binomial"); }
  | 'factorial' { $m = csymbol("integer1", "factorial"); };

paraExpr returns [CMathML m]:
    '(' x=expr ')' { $m = $x.m; };

ifExpr returns [CMathML m]:
    'if' x=expr 'then' y=expr 'else' z=expr 'endif' { $m = apply("prog1", "if", $x.m, $y.m, $z.m); };

whileExpr  returns [CMathML m]:
    'while' x=expr 'do' y=expr 'endwhile' { $m = apply("prog1", "while", $x.m, $y.m); };

symbol returns [CSymbol m]:
    cd=ID '.' name=ID { $m = csymbol(stripID($cd),stripID($name)); };

var returns [CILike m]:
    '$' x=ID { $m = ci(stripID($x)); };

ref returns [CMathML m]:
    ('#' | '##') ID { if (true) throw new NotImplementedError("references not supported"); };

intt returns [CN m]:
    x=HEXINT { if (true) throw new NotImplementedError("not yet implemented"); }
  | x=DECINT { $m = cn($x.getText()); };

floatt returns [CN m]:
	x=HEXFLOAT { if (true) throw new NotImplementedError("not yet implemented"); }
  | x=DECFLOAT { $m = cn($x.getText()); };


OMB: '%' [a-zA-Z0-9=]+ '%';

// Following [1] instead of the unclear Popcorn spec
ID: [a-zA-Z_][a-zA-Z0-9_]*  |  '\'' [^\']* '\'' ;

DECINT: [0-9]+;
HEXINT: '0x' [a-fA-F0-9]+;


HEXFLOAT: '0f' [a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9];
DECFLOAT: [0-9]+ '.' [0-9]+ ('e' '-'? [0-9]+)? ;

// mix of [1] and spec
STRING: '"' ([^"\\] | '\\"' | '\\\\' | '\\n' | '\\r' | '\\t')* '"';

ErrorCharacter : . ;


// [1] http://java.symcomp.org/download/org.symcomp-1.5.0-src.zip /org.symcomp-1.5.0-src/openmath/src/main/antlr/org/symcomp/openmath/popcorn/
