:- module(basictypes,
	[ any/1,
	  atm/1,
	  flt/1,
	  gnd/1,
	  int/1,
	  num/1,
	  numexpression/1,
	  regtype/2,
	  struct/1,
	  term/1,
	  callable/1,
	  defines_type/2
	],
	[ assertions,
	  isomodes,
	  'types/types_basic'
	]).

%% --------------------------------------------------------------------------

:- comment(title,"Some basic types").

:- comment(module,"@cindex{types, basic}
   This library contains a basic set of type definitions. They are
   intended for use both as type testing builtins within programs and
   as properties in assertions.").

%% --------------------------------------------------------------------------
%% Some very special "types" ;-)

:- comment(any/1, "This is equivalent to @pred{term/1}. Included
   because this name is frequently used.").

:- prop any(X) ; "@var{X} is any term.".

any(_). %% Not needed???

:- prop term(X); "@var{X} is any term.".

:- comment(term/1, "The most general type (includes all possible
   terms).").

term(_).

:- comment(regtype/2,"The term @var{Term} is of type @var{TypeExpre}.
   Note: it is assumed that @var{TypeExpre} is a valid regular type
   expression which does not contain any variables, otherwise, the
   behavior of the predicate is undefined.").

:- prop regtype(?Term, +TypeExpre)

; "@var{Term} is of type @var{TypeExpre}.".

regtype(Term, ^(TypeTerm)):- 
    % An escaped term.
    % Ckecks that main functors are the same, and the arguments
    % of Term are of the type represented by the corresponding
    % arguments (which are regular type expressions) of TypeTerm.
    !,
    TypeTerm =.. [Name|TypeArgs],
    Term =.. [Name|TermArgs],
    map_types(TermArgs, TypeArgs).
regtype([H|R], [TH|TR]):- % The list constructor is not escaped
    !,
    regtype(H, TH),
    regtype(R, TR).      
regtype([], []):- % The empty list is not escaped
    !.
regtype(X, X):- % Numbers are not escaped
    number(X),
    !.
regtype(Term, Type):- 
    % Type represents a call to a (possibly parametric) type with 
    % Term as first argument.
    Type =.. [Name|Args],
    Call =.. [Name, Term|Args],
    call(Call).

map_types([], []).
map_types([Term|TermArgs], [Type|TypeArgs]):-
    regtype(Term, Type),
    map_types(TermArgs, TypeArgs).  

%% --------------------------------------------------------------------------
%% Very basic "types" (which typeslib understands natively):

:- prop atm(T) : term => atom ; "@var{T} is an atom (a non-numeric constant).".

atm(a).
atm(T) :- atom(T).

:- comment(flt/1, "The range of floats is the one provided by the C
        @tt{double} type, typically @tt{[4.9e-324, 1.8e+308]} (plus
        or minus).").

:- prop flt(T) : term => float ; "@var{T} is a float.".

flt(T) :- float(T), !.
flt(T) :- int(N), T is N/10.

%:- pred struct/1: term => struct.
:- prop struct(T): term => compound ; "@var{T} is a compound term.".

struct([_|_]):- !.
struct(T) :- functor(T, _, A), A>0. % compound(T).

%:- pred gnd/1: term => gnd.
:- prop gnd(T) : term => ground ; "@var{T} is ground.".

gnd([]) :- !.
gnd(T) :- functor(T, _, A), grnd_args(A, T).

grnd_args(0, _).
grnd_args(N, T) :-
        arg(N, T, A),
        gnd(A),
        N1 is N-1,
        grnd_args(N1, T).

:- comment(int/1, "The range of integers is @tt{[-2^2147483616,
        2^2147483616)}.  Thus for all practical purposes, the range of
        integers can be considered infinite.").

%:- pred int/1: term => int.
:- prop int(T): term => integer ; "@var{T} is an integer.".

int(X) :- integer(X), !.
int(0).
int(N) :- posint(I), give_sign(I, N).

posint(1).
posint(N) :- posint(N1), N is N1+1.

give_sign(P, P).
give_sign(P, N) :- N is -P.

:- prop num(T) : term => number ; "@var{T} is a number.".

num(T) :- number(T), !.
num(T) :- int(T).
num(T) :- flt(T). % never reached!

%% In builtin:
%% :- prop var(T) ; "@var{T} is a variable.".

%% --------------------------------------------------------------------------
%% Very common types:

:- type numexpression(E) ; "@var{E} is a numeric expression.".

numexpression(X) :- num(X).
numexpression(X0+X1) :- numexpression(X0), numexpression(X1).
numexpression(X0-X1) :- numexpression(X0), numexpression(X1).
numexpression(X0/X1) :- numexpression(X0), numexpression(X1).
numexpression(X0*X1) :- numexpression(X0), numexpression(X1).
numexpression(X0 mod X1) :- numexpression(X0), numexpression(X1).
numexpression(-X0) :- numexpression(X0).

%% --------------------------------------------------------------------------

:- comment(doinclude,callable/1).

:- prop callable(Term) ; "@var{Term} is a term which represents a goal, i.e.,
        an atom or a structure.".

callable(Term) :- atm(Term).
callable(Term) :- struct(Term).

%% --------------------------------------------------------------------------

:- prop defines_type(A,T) ; "Defines the regular type: @var{T}.".

defines_type(_,_). 
