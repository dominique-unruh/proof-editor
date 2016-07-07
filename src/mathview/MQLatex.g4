
grammar MQLatex;

@header {
import cmathml.*;
import static mathview.MQLatex.*;
import scala.math.BigDecimal;
}

math_eof returns [CMathML cmathml]:
    math EOF { $cmathml = $math.m; };

math returns [CMathML m]:
        x=math '^' y=math { $m = op("arith1","power",$x.m,$y.m); }
    |   x=math y=math { $m = op("arith1","times",$x.m,$y.m); }
    |   x=math '\\cdot' y=math { $m = op("arith1","times",$x.m,$y.m); }
    |   x=math ( '+' y=math { $m = op("arith1","plus",$x.m,$y.m); }
               | '-' y=math { $m = op("arith1","minus",$x.m,$y.m); } )
    |   x=math '=' y=math { $m = op("relation1","eq",$x.m,$y.m); }


    |   INT { $m = new CN(BigDecimal.exact($INT.text)); }
    |   VAR { $m = new CI($VAR.text); }
    |   '{' x=math '}' { $m = $x.m; }
    |   '\\left' '(' x=math '\\right' ')' { $m = $x.m; }
    |   '\\frac' '{' x=math '}' '{' y=math '}' { $m = op("arith1","divide",$x.m,$y.m); }
    ;


INT     : [0-9]+ ;
VAR     : [a-zA-Z];
ErrorCharacter : . ;