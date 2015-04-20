:- module(arithmetic, [
        (is)/2, (<)/2, (=<)/2, (>)/2, (>=)/2, (=:=)/2, (=\=)/2,
         arithexpression/1],
        [assertions, isomodes]).

:- comment(title, "Arithmetic").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(usage, "These predicates are builtin in CIAO, so nothing special
   has to be done to use them.").

:- comment(module, "Arithmetic is performed by built-in predicates which
   take as arguments arithmetic expressions (see
   @pred{arithexpression/1}) and evaluate them.  Terms representing
   arithmetic expressions can be created dynamically, but at the time of
   evaluation, each variable in an arithmetic expression must be bound
   to a non-variable expression (the term must be ground).  For example,
   given the code in the first line a possible shell interaction follows:
@begin{verbatim}
evaluate(Expression, Answer) :- Answer is Expression.

?- _X=24*9, evaluate(_X+6, Ans).

Ans = 222 ?

yes
@end{verbatim}
").

% Compiled inline -- these are hooks for the interpreter.

:- comment(is(Val,Exp), "The arithmetic expression @var{Exp} is
   evaluated and the result is unified with @var{Val}").
:- trust pred is(?term,+arithexpression) + iso.

X is Y :- X is +Y.

:- comment((Exp1 < Exp2), "The numeric value of @var{Exp1} is less than
   the numeric value of @var{Exp2} when both are evaluated as arithmetic
   expressions.").
:- trust pred <(+arithexpression,+arithexpression) + iso.

X<Y :- X<Y.

:- comment((Exp1 =< Exp2), "The numeric value of @var{Exp1} is less than
   or equal to the numeric value of @var{Exp2} when both are evaluated as
   arithmetic expressions.").
:- trust pred =<(+arithexpression,+arithexpression) + iso.

X=<Y :- X=<Y.

:- comment((Exp1 > Exp2), "The numeric value of @var{Exp1} is greater than
   the numeric value of @var{Exp2} when both are evaluated as arithmetic
   expressions.").
:- trust pred >(+arithexpression,+arithexpression) + iso.

X>Y :- X>Y.

:- comment((Exp1 >= Exp2), "The numeric value of @var{Exp1} is greater than
   or equal to the numeric value of @var{Exp2} when both are evaluated as
   arithmetic expressions.").
:- trust pred >=(+arithexpression,+arithexpression) + iso.

X>=Y :- X>=Y.

:- comment((Exp1 =:= Exp2), "The numeric values of @var{Exp1} and
   @var{Exp2} are equal when both are evaluated as arithmetic expressions.").
:- trust pred =:=(+arithexpression,+arithexpression) + iso.

X=:=Y :- X=:=Y.

:- comment((Exp1 =\= Exp2), "The numeric values of @var{Exp1} and
   @var{Exp2} are not equal when both are evaluated as arithmetic
   expressions.").
:- trust pred =\=(+arithexpression,+arithexpression) + iso.

X=\=Y :- X=\=Y.

% :- comment(doinclude, arithexpression/1).

:- prop arithexpression(E) # "@var{E} is an arithmetic expression.".

:- comment(arithexpression/1, "An arithmetic expression is a term built
   from numbers and @concept{evaluable functors} that represent
   arithmetic functions.  An arithmetic expression evaluates to a
   number, which may be an integer (@pred{int/1}) or a float
   (@pred{flt/1}).  The evaluable functors allowed in an arithmetic
   expression are listed below, together with an indication of the
   functions they represent.  All evaluable functors defined in
   @concept{ISO-Prolog} are implemented, as well as some other useful or
   traditional.  Unless stated otherwise, an expression evaluates to a
   float if any of its arguments is a float, otherwise to an integer.

   @begin{itemize}

   @item @pred{- /1}: sign reversal. @bf{(ISO)}

   @item @pred{+ /1}: identity.

   @item @pred{-- /1}: decrement by one.

   @item @pred{++ /1}: increment by one.

   @item @pred{+ /2}: addition. @bf{(ISO)}

   @item @pred{- /2}: subtraction. @bf{(ISO)}

   @item @pred{* /2}: multiplication. @bf{(ISO)}

   @item @pred{// /2}: integer division.  Float arguments are truncated
   to integers, result always integer. @bf{(ISO)}

   @item @pred{/ /2}: division.  Result always float. @bf{(ISO)}

   @item @pred{rem/2}: integer remainder.  The result is always an
   integer, its sign is the sign of the first argument. @bf{(ISO)}

   @item @pred{mod/2}: modulo. The result is always a positive integer.
   @bf{(ISO)}

   @item @pred{abs/1}: absolute value. @bf{(ISO)}

   @item @pred{sign/1}: sign of. @bf{(ISO)}

   @item @pred{float_integer_part/1}: float integer part. Result always
   float. @bf{(ISO)}

   @item @pred{float_fractional_part/1}: float fractional part. Result always
   float. @bf{(ISO)}

   @item @pred{truncate/1}: The result is the integer equal to the
   integer part of the argument. @bf{(ISO)}

   @item @pred{integer/1}: same as @pred{truncate/1}.

   @item @pred{float/1}: conversion to float. @bf{(ISO)}

   @item @pred{floor/1}: largest integer not greater than. @bf{(ISO)}

   @item @pred{round/1}: integer nearest to. @bf{(ISO)}

   @item @pred{ceiling/1}: smallest integer not smaller than. @bf{(ISO)}

   @item @pred{** /2}: exponentiation.  Result always float. @bf{(ISO)}

   @item @pred{>> /2}: integer bitwise right shift. @bf{(ISO)}

   @item @pred{<< /2}: integer bitwise left shift. @bf{(ISO)}

   @item @pred{/\\ /2}: integer bitwise and. @bf{(ISO)}

   @item @pred{\\/ /2}: integer bitwise or. @bf{(ISO)}

   @item @pred{\\ /1}: integer bitwise complement. @bf{(ISO)}

   @item @pred{# /2}: integer bitwise exclusive or (xor).

   @item @pred{exp/1}: exponential (@em{e} to the power of). Result always
   float. @bf{(ISO)}

   @item @pred{log/1}: natural logarithm (base @em{e}). Result always
   float. @bf{(ISO)}

   @item @pred{sqrt/1}: square root. Result always float. @bf{(ISO)}

   @item @pred{sin/1}: sine. Result always float. @bf{(ISO)}

   @item @pred{cos/1}: cosine. Result always float. @bf{(ISO)}

   @item @pred{atan/1}: arc tangent. Result always float. @bf{(ISO)}

   @item @pred{gcd/2}: Greatest common divisor.  Arguments must evaluate
   to integers, result always integer.

   @end{itemize}

   In addition to these functors, a list of just a number evaluates to
   this number.  Since a @concept{quoted string} is just a list of
   integers, this allows a quoted character to be used in place of its
   @concept{ASCII code}; e.g. @tt{""A""} behaves within arithmetic
   expressions as the integer 65.  Note that this is not
   @cindex{ISO-Prolog}ISO-compliant, and that can be achieved by using
   the notation @tt{0'A} (which is ISO).

   Arithmetic expressions, as described above, are just data structures.
   If you want one evaluated you must pass it as an argument to one of the
   arithmetic predicates defined here.
  ").

arithexpression(X) :- num(X).
arithexpression(-X) :- arithexpression(X).
arithexpression(+X) :- arithexpression(X).
arithexpression(--X) :- arithexpression(X).
arithexpression(++X) :- arithexpression(X).
arithexpression(X+Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X-Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X*Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X//Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X/Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X rem Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X mod Y) :- arithexpression(X), arithexpression(Y).
arithexpression(abs(X)) :- arithexpression(X).
arithexpression(sign(X)) :- arithexpression(X).
arithexpression(float_integer_part(X)) :- arithexpression(X).
arithexpression(float_fractional_part(X)) :- arithexpression(X).
arithexpression(integer(X)) :- arithexpression(X).
arithexpression(truncate(X)) :- arithexpression(X).
arithexpression(float(X)) :- arithexpression(X).
arithexpression(floor(X)) :- arithexpression(X).
arithexpression(round(X)) :- arithexpression(X).
arithexpression(ceiling(X)) :- arithexpression(X).
arithexpression(X**Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X>>Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X<<Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X/\Y) :- arithexpression(X), arithexpression(Y).
arithexpression(X\/Y) :- arithexpression(X), arithexpression(Y).
arithexpression(\(X)) :- arithexpression(X).
arithexpression(X#Y ) :- arithexpression(X), arithexpression(Y).
arithexpression(exp(X)) :- arithexpression(X).
arithexpression(log(X)) :- arithexpression(X).
arithexpression(sqrt(X)) :- arithexpression(X).
arithexpression(sin(X)) :- arithexpression(X).
arithexpression(cos(X)) :- arithexpression(X).
arithexpression(atan(X)) :- arithexpression(X).
arithexpression(gcd(X,Y)) :- arithexpression(X), arithexpression(Y).
arithexpression([X]) :- num(X).
