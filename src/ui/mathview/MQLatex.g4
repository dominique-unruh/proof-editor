
grammar MQLatex;

@header {
import cmathml.*;
import static ui.mathview.MQLatex.*;
import scala.math.BigDecimal;
}

// !($y.m instanceof CN && ((CN)$y.m).isNegative())


math_eof returns [CMathML cmathml]:
    x=mathRel EOF { $cmathml = $x.m; };

math returns [CMathML m]:
    x=mathRel { $m=$x.m; };


mathRel returns [CMathML m]:
        x=mathPlus '=' y=mathPlus { $m = op("relation1","eq",$x.m,$y.m); }
    |   x=mathPlus { $m=$x.m; }
    ;

mathPlus returns [CMathML m]:
        x=mathPlus '+' y=mathTimes { $m = op("arith1","plus",$x.m,$y.m); }
    |   x=mathPlus '-' y=mathTimes { $m = op("arith1","minus",$x.m,$y.m); }
    |   z=mathTimes { $m=$z.m; }
    ;

mathTimes returns [CMathML m]:
        x=mathTimes '\\cdot' y=mathUMinus { $m = op("arith1","times",$x.m,$y.m); }
    |   z=mathUMinus { $m = $z.m; }
    ;

mathUMinus returns [CMathML m]:
        '-' x=mathInvisibleTimes { $m = $x.m.negate(); }
    |   z=mathInvisibleTimes { $m = $z.m; }
    ;

mathInvisibleTimes returns [CMathML m]:
        x=mathInvisibleTimes y=mathExp { $m = op("arith1","times",$x.m,$y.m); }
    |   z=mathExp { $m = $z.m; }
    ;

mathExp returns [CMathML m]:
        x=mathAtom '^' y=mathAtom { $m = op("arith1","power",$x.m,$y.m); }
    |   z=mathAtom { $m=$z.m; }
    ;

//math returns [CMathML m]:
//        <assoc=left> x=math '^' y=math { $m = op("arith1","power",$x.m,$y.m); }
//    |   <assoc=left> x=math y=math { $m = op("arith1","times",$x.m,$y.m); }
//    |   '-' NUM { $m = CN.apply("-"+$NUM.text); }
//    |   '-' x=math { $m = op("arith1","unary_minus",$x.m); }
//    |   <assoc=left> x=math '\\cdot' y=math { $m = op("arith1","times",$x.m,$y.m); }
//    |   z=mathAtom { $m = $z.m; }
//    ;

mathAtom returns [CMathML m]:
        NUM { $m = CN.apply($NUM.text); }
    |   VAR { $m = new CI($VAR.text); }
    |   '{' x=math '}' { $m = $x.m; }
    |   '\\left' '(' x=math '\\right' ')' { $m = $x.m; }
    |   '\\frac' '{' x=math '}' '{' y=math '}' { $m = op("arith1","divide",$x.m,$y.m); }
    ;

//math_notminus returns [CMathML m]:
//    | { false } math { $m = $math.m; }
//    ;

NUM     : [0-9]+ ('.' [0-9]+)? ;
VAR     : [a-zA-Z];
ErrorCharacter : . ;