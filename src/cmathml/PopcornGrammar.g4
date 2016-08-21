grammar PopcornGrammar;

@header {
import cmathml.*;
import cmathml.CMathML.*;
import cmathml.JavaHelpers.*;
}

expr_eof returns [CMathML cmathml]:
    x=expr EOF { $cmathml = $x.m; };

expr returns [CMathML m]:
    x=blockExpr { $m = $x.m; };

blockExpr returns [CMathML m]:
    bs+=assignExpr (';' bs+=assignExpr)+ { $m = apply(prog1.block(),bs); }
  | x=assignExpr { $m = $x.m; };

assignExpr returns [CMathML m]:
	x=implExpr ':=' y=implExpr { $m = apply(prog1.assign(),$x.m,$y.m); }
  | x=implExpr { $m = $x.m; };

implExpr returns [CMathML m]:
    x=orExpr '==>' y=orExpr { $m = apply(logic1.implies(),$x.m,$y.m); }
  | x=orExpr '<=>' y=orExpr { $m = apply(logic1.equivalent(),$x.m,$y.m); }
  | x=orExpr { $m = $x.m; };

orExpr returns [CMathML m]:
    x=andExpr 'or' y=andExpr { $m = apply(logic1.or(),$x.m,$y.m); }
  | x=andExpr { $m = $x.m; };

andExpr returns [CMathML m]:
    x=relExpr 'and' y=relExpr { $m = apply(logic1.and(),$x.m,$y.m); }
  | x=relExpr { $m = $x.m; };

relExpr returns [CMathML m]:
	x=intervalExpr '=' y=intervalExpr { $m = apply(relation1.eq(),$x.m,$y.m); }
  | x=intervalExpr '<' y=intervalExpr { $m = apply(relation1.lt(),$x.m,$y.m); }
  | x=intervalExpr '<=' y=intervalExpr { $m = apply(relation1.le(),$x.m,$y.m); }
  | x=intervalExpr '>' y=intervalExpr { $m = apply(relation1.gt(),$x.m,$y.m); }
  | x=intervalExpr '>=' y=intervalExpr { $m = apply(relation1.ge(),$x.m,$y.m); }
  | x=intervalExpr '!=' y=intervalExpr { $m = apply(relation1.neq(),$x.m,$y.m); }
  | x=intervalExpr '<>' y=intervalExpr { $m = apply(relation1.neq(),$x.m,$y.m); }
  | x=intervalExpr { $m = $x.m; };

intervalExpr returns [CMathML m]:
    x=addExpr '..' y=addExpr { $m = apply(interval1.interval(),$x.m,$y.m); }
  | x=addExpr { $m = $x.m; };

addExpr returns [CMathML m]:
    x=multExpr '-' y=multExpr { $m = apply(arith1.minus(),$x.m,$y.m); }
  | x=multExpr '+' y=multExpr { $m = apply(arith1.plus(),$x.m,$y.m); }
  | x=multExpr { $m = $x.m; };

multExpr returns [CMathML m]:
    x=powerExpr '/' y=powerExpr { $m = apply(arith1.divide(),$x.m,$y.m); }
  | x=powerExpr '*' y=powerExpr { $m = apply(arith1.times(),$x.m,$y.m); }
  | x=powerExpr { $m = $x.m; };

powerExpr returns [CMathML m]:
    x=complexExpr '^' y=complexExpr { $m = apply(arith1.power(),$x.m,$y.m); }
  | x=complexExpr { $m = $x.m; };

complexExpr returns [CMathML m]:
    x=rationalExpr '|' y=rationalExpr { $m = apply(complex1.complex_cartesian(),$x.m,$y.m); }
  | x=rationalExpr { $m = $x.m; };

rationalExpr returns [CMathML m]:
    x=negExpr '//' y=negExpr { $m = apply(num1.rational(),$x.m,$y.m); }
  | x=negExpr { $m = $x.m; };

negExpr  returns [CMathML m]:
    '-' x=compExpr  { $m = apply(arith1.uminus(),$x.m); }
  | 'not' x=compExpr { $m = apply(logic1.not(),$x.m); }
=======
    bs+=assignExpr (';' bs+=assignExpr)+ { $m = apply(prog1.block,bs); }
  | x=assignExpr { $m = $x.m; };

assignExpr returns [CMathML m]:
	x=implExpr ':=' y=implExpr { $m = apply(prog1.assign,$x.m,$y.m); }
  | x=implExpr { $m = $x.m; };

implExpr returns [CMathML m]:
    x=orExpr '==>' y=orExpr { $m = apply(logic1.implies,$x.m,$y.m); }
  | x=orExpr '<=>' y=orExpr { $m = apply(logic1.equivalent,$x.m,$y.m); }
  | x=orExpr { $m = $x.m; };

orExpr returns [CMathML m]:
    x=andExpr 'or' y=andExpr { $m = apply(logic1.or,$x.m,$y.m); }
  | x=andExpr { $m = $x.m; };

andExpr returns [CMathML m]:
    x=relExpr 'and' y=relExpr { $m = apply(logic1.and,$x.m,$y.m); }
  | x=relExpr { $m = $x.m; };

relExpr returns [CMathML m]:
	x=intervalExpr '=' y=intervalExpr { $m = apply(relation1.eq,$x.m,$y.m); }
  | x=intervalExpr '<' y=intervalExpr { $m = apply(relation1.lt,$x.m,$y.m); }
  | x=intervalExpr '<=' y=intervalExpr { $m = apply(relation1.le,$x.m,$y.m); }
  | x=intervalExpr '>' y=intervalExpr { $m = apply(relation1.gt,$x.m,$y.m); }
  | x=intervalExpr '>=' y=intervalExpr { $m = apply(relation1.ge,$x.m,$y.m); }
  | x=intervalExpr '!=' y=intervalExpr { $m = apply(relation1.neq,$x.m,$y.m); }
  | x=intervalExpr '<>' y=intervalExpr { $m = apply(relation1.neq,$x.m,$y.m); }
  | x=intervalExpr { $m = $x.m; };

intervalExpr returns [CMathML m]:
    x=addExpr '..' y=addExpr { $m = apply(interval1.interval,$x.m,$y.m); }
  | x=addExpr { $m = $x.m; };

addExpr returns [CMathML m]:
    x=multExpr '-' y=multExpr { $m = apply(arith1.minus,$x.m,$y.m); }
  | x=multExpr '+' y=multExpr { $m = apply(arith1.plus,$x.m,$y.m); }
  | x=multExpr { $m = $x.m; };

multExpr returns [CMathML m]:
    x=powerExpr '/' y=powerExpr { $m = apply(arith1.divide,$x.m,$y.m); }
  | x=powerExpr '*' y=powerExpr { $m = apply(arith1.times,$x.m,$y.m); }
  | x=powerExpr { $m = $x.m; };

powerExpr returns [CMathML m]:
    x=complexExpr '^' y=complexExpr { $m = apply(arith1.power,$x.m,$y.m); }
  | x=complexExpr { $m = $x.m; };

complexExpr returns [CMathML m]:
    x=rationalExpr '|' y=rationalExpr { $m = apply(complex1.complex_cartesian,$x.m,$y.m); }
  | x=rationalExpr { $m = $x.m; };

rationalExpr returns [CMathML m]:
    x=negExpr '//' y=negExpr { $m = apply(num1.rational,$x.m,$y.m); }
  | x=negExpr { $m = $x.m; };

negExpr  returns [CMathML m]:
    '-' x=compExpr  { $m = apply(arith1.uminus,$x.m); }
  | 'not' x=compExpr { $m = apply(logic1.not,$x.m); }
>>>>>>> 4bfd5acbad4b4e497c7455ce24dd22bcbadbcf31
  | x=compExpr { $m = $x.m; };

compExpr  returns [CMathML m]:
    p=paraExpr { $m = $p.m; }
  | e=ecall { $m = $e.m; }
  | a=attribution { $m = $a.m; }
  | b=binding { $m = $b.m; }
  | l=listExpr { $m = $l.m; }
  | s=setExpr { $m = $s.m; }
  | an=anchor { $m = $an.m; };

commaList returns [List<CMathML> ms]:
  | (xs+=expr (',' xs+=expr)*)? { $ms = $xs.map(x -> x.m); };

varCommaList returns [List<CILike> ms]:
  | (xs+=var (',' xs+=var)*)? { $ms = $xs.map(x -> x.m); };

call returns [CMathML m]:
    hd=anchor '(' args=commaList ')' { apply(hd,args); };

ecall  returns [CMathML m]:
    hd=anchor '!' '(' args=commaList ')' { error(hd,args); };

listExpr  returns [CMathML m]:
    '[' args=commaList ']' { apply(list1.list,args); };

setExpr  returns [CMathML m]:
    '{' args=commaList '}' { apply(set1.set,args); };

/* foreignExpr  returns [CMathML m]:
    '`' FOREIGN '`' { TODO; }; */

attribution  returns [CMathML m]:
    x=anchor '{' attr=attributionList '}' { $m = addAttributes($x.m, $attr.attrs); };

attributionList returns [List<Pair<CSymbol,CMathML>> attrs]:
    x+=attributionPair (',' x+=attributionPair)* { $attrs = $x; };

attributionPair returns [Pair<CSymbol,CMathML> attr]:
    x=symbol '->' y=expr { $attr = Pair($x.m,$y.m); };

binding  returns [CMathML m]:
    x=anchor '[' v=varCommaList '->' b=expr ']' { $m = bind(x,v,b); };

anchor  returns [CMathML m]:
    x=atom (':' ID)? { $m = $x.m; }; // Ignoring ID (can't be represented)

atom returns [CMathML m]:
    x=paraExpr { $m = $x.m; }
  | x=shortSymbol { $m = $x.m; }
  | x=var { $m = $x.m; }
  | x=intt { $m = $x.m; }
  | x=floatt { $m = $x.m; }
  | x=ref { $m = $x.m; }
  | b=OMB { throw new NotSupportedException("not yet implemented"); }
//  | fo=FOREIGN { TODO; }  // Not implemented
  | str=STRING { throw new NotSupportedException("not yet implemented"); }
  | x=ifExpr { $m = $x.m; }
  | x=whileExpr { $m = $x.m; };

shortSymbol returns [CSymbol m]:
    'cos' { $m = transc1.cos; }
  | 'cosh' { $m = transc1.cosh; }
  | 'cot' { $m = transc1.cot; }
  | 'coth' { $m = transc1.coth; }
  | 'csc' { $m = transc1.csc; }
  | 'csch' { $m = transc1.csch; }
  | 'exp' { $m = transc1.exp; }
  | 'sec' { $m = transc1.sec; }
  | 'sech' { $m = transc1.sech; }
  | 'sin' { $m = transc1.sin; }
  | 'sinh' { $m = transc1.sinh; }
  | 'tan' { $m = transc1.tan; }
  | 'tanh' { $m = transc1.tanh; }
  | 'abs' { $m = arith1.abs; }
  | 'root' { $m = arith1.root; }
  | 'sum' { $m = arith1.sum; }
  | 'product' { $m = arith1.product; }
  | 'diff' { $m = calculus1.diff; }
  | 'int' { $m = calculus1.integral; }
  | 'defint' { $m = calculus1.defint; }
  | 'pi' { $m = nums1.pi; }
  | 'e' { $m = nums1.e; }
  | 'i' { $m = nums1.i; }
  | 'infinity' { $m = nums1.infinity; }
  | 'min' { $m = minmax1.min; }
  | 'max' { $m = minmax1.max; }
  | 'lambda' { $m = fns1.lambda; }
  | 'true' { $m = logic1.trueSym; }
  | 'false' { $m = logic1.falseSym; }
  | 'binomial' { $m = combinat1.binomial; }
  | 'factorial' { $m = integer1.factorial; };

paraExpr  returns [CMathML m]:
    '(' x=expr ')' { $m = $x.m; };

ifExpr  returns [CMathML m]:
    'if' x=expr 'then' y=expr 'else' z=expr 'endif' { $m = apply(prog1.if,x,y,z); };

whileExpr  returns [CMathML m]:
    'while' x=expr 'do' y=expr 'endwhile' { $m = apply(prog1.while,x,y); };

symbol returns [CSymbol m]:
    cd=ID '.' name=ID { $m = new CSymbol($cd,$name); };

var returns [CILike m]:
    '$' x=ID { $m = new CI($x); };

ref returns [CMathML m]:
    ('#' | '##') ID { throw new SyntaxError("references not supported"); };

intt returns [CN m]:
    x=HEXINT { throw new NotSupportedException("not yet implemented"); }
  | x=DECINT { $m = new CN($x); };

floatt returns [CN m]:
	x=HEXFLOAT { throw new NotSupportedException("not yet implemented"); }
  | x=DECFLOAT { $m = new CN($x); };

ref:
    ('#' | '##') ID { throw new SyntaxError("references not supported"); };


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
