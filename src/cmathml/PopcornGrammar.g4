grammar PopcornGrammar;

@header {
import cmathml.*;
}

expr_eof returns [CMathML cmathml]:
    x=expr EOF { $cmathml = $x.m; };

expr returns [CMathML m]:
    x=blockExpr { $m = $x.m; };

blockExpr returns [CMathML m]:
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
  | x=compExpr { $m = $x.m; };

compExpr  returns [CMathML m]:
    p=paraExpr { $m = $p.m; }
  | e=ecall { $m = $e.m; }
  | a=attribution { $m = $a.m; }
  | b=binding { $m = $b.m; }
  | l=listExpr { $m = $l.m; }
  | s=setExpr { $m = $s.m; }
  | an=anchor { $m = $an.m; };

commaList returns [Type ms]:
  | (xs+=expr (',' xs+=expr)*)? { $ms = $xs.map(x -> x.m); };

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
    x=anchor '{' a=attributionList '}' { TODO; };

attributionList returns [Type attrs]:
    x=attributionPair (',' attributionPair)* { TODO; };

attributionPair returns [Type a]:
    x=expr '->' y=expr { TODO; };

binding  returns [CMathML m]:
    x=anchor '[' v=commaList '->' b=expr ']' { TODO; };

anchor  returns [CMathML m]:
    x=atom (':' ID)? { $m = $x.m; }; // Ignoring ID (can't be encoded)

atom returns [CMathML m]:
    x=paraExpr { $m = $x.m; }
  | s=ID { TODO; }
  | v=var { TODO; }
  | i=intt { TODO; }
  | f=floatt { TODO; }
  | r=ref { TODO; }
  | b=OMB { TODO; }
//  | fo=FOREIGN { TODO; }  // Not implemented
  | str=STRING { TODO; }
  | x=ifExpr { $m = $x.m; }
  | x=whileExpr { $m = $x.m; };

paraExpr  returns [CMathML m]:
    '(' x=expr ')' { $m = $x.m; };

ifExpr  returns [CMathML m]:
    'if' x=expr 'then' y=expr 'else' z=expr 'endif' { TODO; };

whileExpr  returns [CMathML m]:
    'while' x=expr 'do' y=expr 'endwhile' { TODO; };

symbol returns [CMathML m]:
    cd=ID '.' name=ID { TODO; };

var  returns [CMathML m]:
    '$' x=ID { TODO; };

ref:
    ('#' | '##') ID { throw new SyntaxError("references not supported"); };

intt  returns [CMathML m]:
    x=HEXINT { TODO; }
  | x=DECINT { TODO; };

floatt  returns [CMathML m]:
	x=HEXFLOAT { TODO; }
  | x=DECFLOAT { TODO; };




OMB: '%' [a-zA-Z0-9=]+ '%';

// Following [1] instead of the unclear Popcorn spec
ID: [a-zA-Z_][a-zA-Z0-9_]*  |  '\'' [^\']* '\'' ;

DECINT: [0-9]+;
HEXINT: '0x' [a-fA-F0-9]+;


HEXFLOAT: '0f' [a-fA-F0-9]{8,8};
DECFLOAT: [0-9]+ '.' [0-9]+ ('e' '-'? [0-9]+)? ;

// mix of [1] and spec
STRING: '"' ([^"\\] | '\\"' | '\\\\' | '\\n' | '\\r' | '\\t')* '"';

ErrorCharacter : . ;


// [1] http://java.symcomp.org/download/org.symcomp-1.5.0-src.zip /org.symcomp-1.5.0-src/openmath/src/main/antlr/org/symcomp/openmath/popcorn/
