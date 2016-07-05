
grammar MQLatex;

math:   math '*' math # times
    |	math '+' math # plus
    |	math '-' math # minus
    |   INT # number
    |   VAR # variable
    |   '{' math '}' # braces
    ;


INT     : [0-9]+ ;
VAR     : [a-zA-Z];