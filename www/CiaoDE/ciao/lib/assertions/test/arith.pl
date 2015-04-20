:- module(arith, [
         foo/2,
         aexp/1],
        [assertions, isomodes]).

:- comment(foo(Exp1,Exp2), "The numeric values of @var{Exp1} and
   @var{Exp2} are not equal when both are evaluated as arithmetic
   expressions.").

:- true pred foo(+aexp,+aexp) + iso.

foo(X,Y) :- foo(X,Y).

:- prop aexp(E) + regtype # "@var{E} is an arithmetic expression.".

aexp(X) :- num(X).
aexp(-X) :- aexp(X).
aexp(+X) :- aexp(X).
aexp(--X) :- aexp(X).
aexp(++X) :- aexp(X).
aexp(X+Y) :- aexp(X), aexp(Y).
aexp(X-Y) :- aexp(X), aexp(Y).
aexp(X*Y) :- aexp(X), aexp(Y).
aexp(X//Y) :- aexp(X), aexp(Y).
aexp(X/Y) :- aexp(X), aexp(Y).
aexp(X rem Y) :- aexp(X), aexp(Y).
aexp(X mod Y) :- aexp(X), aexp(Y).
aexp(abs(X)) :- aexp(X).
aexp(sign(X)) :- aexp(X).
aexp(float_integer_part(X)) :- aexp(X).
aexp(float_fractional_part(X)) :- aexp(X).
aexp(integer(X)) :- aexp(X).
aexp(truncate(X)) :- aexp(X).
aexp(float(X)) :- aexp(X).
aexp(floor(X)) :- aexp(X).
aexp(round(X)) :- aexp(X).
aexp(ceiling(X)) :- aexp(X).
aexp(X**Y) :- aexp(X), aexp(Y).
aexp(X>>Y) :- aexp(X), aexp(Y).
aexp(X<<Y) :- aexp(X), aexp(Y).
aexp(X/\Y) :- aexp(X), aexp(Y).
aexp(X\/Y) :- aexp(X), aexp(Y).
aexp(\(X)) :- aexp(X).
aexp(X#Y ) :- aexp(X), aexp(Y).
aexp(exp(X)) :- aexp(X).
aexp(log(X)) :- aexp(X).
aexp(sqrt(X)) :- aexp(X).
aexp(sin(X)) :- aexp(X).
aexp(cos(X)) :- aexp(X).
aexp(atan(X)) :- aexp(X).
aexp(gcd(X,Y)) :- aexp(X), aexp(Y).
aexp([X]) :- num(X).
