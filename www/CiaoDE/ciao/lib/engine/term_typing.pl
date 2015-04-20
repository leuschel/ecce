:- module(term_typing, [
        var/1, nonvar/1, atom/1, integer/1, float/1, number/1, atomic/1,
        ground/1, type/2],
        [assertions, isomodes]).

:- comment(title,"Extra-logical properties for typing").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"This library contains traditional Prolog predicates
        for testing types.  They depend on the state of instantiation of
        their arguments, thus being of extra-logical nature.").

:- true prop ground(X) + native
	# "@var{X} is currently ground (it contains no variables).".
:- true success ground(X) => gnd(X).
:- true comp ground(@X) + ( sideff(free), native ).
:- true comp ground(X) : ground(X) + eval.
:- true comp ground(X) : var(X) + equiv(fail).
:- true comp ground(X) : ground(X) + equiv(true).

/* % jfran: Now implemented in C 
ground(Term):-
	nonvar(Term),
	functor(Term,_,N),
	ground_(N,Term).

ground_(0,_) :- !.
ground_(N,Term):-
	N > 0,
	arg(N,Term,Arg),
	ground(Arg),
	N1 is N-1,
	ground_(N1,Term).
*/
:- impl_defined([ground/1]).

% Compiled inline -- these are hooks for the interpreter.

:- true prop atom(X) + native
	# "@var{X} is currently instantiated to an atom.".
:- true success atom(X) => atm(X).
:- true comp atom(@X) + ( sideff(free), native ).
:- true comp atom(X) : nonvar(X) + eval.
:- true comp atom(T) : var(T) + equiv(fail).

atom(X) :- atom(X).

 :- prop atom_or_number/1 + regtype.
 
 atom_or_number(X):-
 	atm(X).
 atom_or_number(X):-
 	num(X).
 
:- true prop atomic(X) + native
	# "@var{X} is currently instantiated to an atom or a number.".
:- true comp atomic(@X) + ( sideff(free), native ).
:- true comp atomic(X) : nonvar(X) + eval.
:- true comp atomic(T) : var(T) + equiv(fail).
:- trust success atomic(T) => atom_or_number(T).

atomic(X) :- atomic(X).

:- true prop float(X) + native
	# "@var{X} is currently instantiated to a float.".
:- true success float(X) => flt(X).
:- true comp float(@X) + ( sideff(free), native ).
:- true comp float(X) : nonvar(X) + eval.
:- true comp float(T) : var(T) + equiv(fail).

float(X) :- float(X).

:- true prop integer(X) + native
	# "@var{X} is currently instantiated to an integer.".
:- true success integer(X) => int(X).
:- true comp integer(@X) + ( sideff(free), native ).
:- true comp integer(X) : nonvar(X) + eval.
:- true comp integer(T) : var(T) + equiv(fail).

integer(X) :- integer(X).

:- true prop nonvar(X) + native(not_free(X))
   # "@var{X} is currently a term which is not a free variable.".
:- true comp nonvar(@X) + ( sideff(free), native ).
:- true comp nonvar(X) : nonvar(X) + eval.
:- true comp nonvar(T) : var(T) + equiv(fail).
:- true comp nonvar(T) : nonvar(T) + equiv(true).

nonvar(X) :- nonvar(X).

:- true prop number(X) + native
	# "@var{X} is currently instantiated to a number.".
:- true success number(X) => num(X).
:- true comp number(@X) + ( sideff(free), native ).
:- true comp number(X) : nonvar(X) + eval.
:- true comp number(T) : var(T) + equiv(fail).

number(X) :- number(X).

:- true prop var(X) + native(free(X))
   # "@var{X} is a free variable.".
:- true comp var(@X) + ( native, sideff(free) ).
:- true comp var(X) : nonvar(X) + eval.
:- true comp var(X) : nonvar(X) + equiv(fail).
:- true comp var(X) : var(X) + equiv(true).

var(X) :- var(X).

:- true prop type(X,Y) + native
   # "@var{X} is internally of type @var{Y} (@tt{var}, @tt{attv}, @tt{float},
      @tt{integer}, @tt{structure}, @tt{atom} or @tt{list}).".
:- true success type(X,Y) => atm(Y).
:- true comp type/2 + ( sideff(free), native ).
:- true comp type(X,Y) : nonvar(X) + eval.

type(X, Y) :- type(X, Y). 
