
grammar MQLatex;

math:   math '*' math # times
    |	math '+' math # plus
    |	math '-' math # minus
    |   INT # number
    |   '{' math '}' # braces
    ;


INT     : [0-9]+ ;
