
grammar MQLatex;

math:   math '\\cdot' math # times
    |	math '+' math # plus
    |	math '-' math # minus
    |   INT # number
    |   VAR # variable
    |   '{' math '}' # braces
    |   '\\left' '(' math '\\right' ')' # parens
    |   '\\frac' '{' math '}' '{' math '}' # frac
    ;


INT     : [0-9]+ ;
VAR     : [a-zA-Z];
