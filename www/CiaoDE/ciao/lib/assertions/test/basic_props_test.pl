:- module(basic_props_test,
        [term/1, int/1, flt/1, num/1, atm/1, struct/1, gnd/1, regtype/2,
         constant/1, callable/1, stream/1, list/1, list/2, character_code/1,
         string/1, predname/1, atm_or_atm_list/1,
         compat/2, iso/1, not_further_inst/2, regtype/1
        ],
        [assertions]).

:- comment(title,"Basic data types and properties").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"@cindex{properties, basic} This library contains
   the set of basic properties used by the builtin predicates, and
   which constitute the basic data types and properties of the
   language.  They can be used both as type testing builtins within
   programs (by calling them explicitly) and as properties in
   assertions.").

:- comment(term/1, "The most general type (includes all possible
   terms).").

:- prop term(X) # "@var{X} is any term.".

term(_).

:- comment(int/1, "The type of integers. The range of integers is
        @tt{[-2^2147483616, 2^2147483616)}.  Thus for all practical
        purposes, the range of integers can be considered infinite.").

:- prop int(T) # "@var{T} is an integer.".

int(X) :- integer(X), !.
int(0).
int(N) :- posint(I), give_sign(I, N).

posint(1).
posint(N) :- posint(N1), N is N1+1.

give_sign(P, P).
give_sign(P, N) :- N is -P.

:- comment(flt/1, "The type of floating-point numbers. The range of
        floats is the one provided by the C @tt{double} type, typically
        @tt{[4.9e-324, 1.8e+308]} (plus or minus).").

:- prop flt(T) # "@var{T} is a float.".

flt(T) :- float(T), !.
flt(T) :- int(N), T is N/10.

:- comment(num/1, "The type of numbers, that is, integer or floating-point.").

:- prop num(T) # "@var{T} is a number.".

num(T) :- number(T), !.
num(T) :- int(T).
% num(T) :- flt(T). % never reached!

:- comment(atm/1, "The type atoms, or non-numeric constants.").

:- prop atm(T) # "@var{T} is an atom.".

% Should be current_atom/1
atm(a).
atm(T) :- atom(T).

:- prop struct(T) # "@var{T} is a compound term.".

struct([_|_]):- !.
struct(T) :- functor(T, _, A), A>0. % compound(T).

:- comment(gnd/1, "The type of all terms without variables.").

:- prop gnd(T) # "@var{T} is ground.".

gnd([]) :- !.
gnd(T) :- functor(T, _, A), grnd_args(A, T).

grnd_args(0, _).
grnd_args(N, T) :-
        arg(N, T, A),
        gnd(A),
        N1 is N-1,
        grnd_args(N1, T).

:- prop regtype(Term, Type) # "@var{Term} is of type @var{Type}.".
:- meta_predicate regtype(?, pred(1)).

regtype(Term, Type) :- Type(Term).

:- prop constant(T) # "@var{T} is an atomic term (an atom or a number).".

constant(T) :- atm(T) ; num(T).

:- prop callable(T) # "@var{T} is a term which represents a goal, i.e.,
        an atom or a structure.".

callable(T) :- atm(T) ; struct(T).

:- prop stream(S) => struct # "@var{S} is a (not closed) stream.".

stream(_).

% stream('$stream'(X)) :- 
% 	int(X).

:- prop list(T) # "@var{T} is a list.".

list([]).
list([_|L]) :- list(L).

:- prop list(L,T) # "@var{L} is a list of @var{T}s.".
:- meta_predicate list(?, pred(1)).

list([],_).
list([X|Xs], T) :-
        T(X),
        list(Xs, T).

:- prop character_code(T) => int
   # "@var{T} is an integer which is a character code.".

character_code(I) :- int(I).

:- prop string(T) => list(character_code)
   # "@var{T} is a string (a list of character codes).".

string(T) :- list(T, character_code).

:- prop predname(P)
   # "@var{P} is a Name/Arity structure denoting a predicate name.".

predname(P/A) :-
	atm(P),
	int(A).

:- prop atm_or_atm_list(T) # "@var{T} is an atom or a list of atoms.".

atm_or_atm_list(T) :- atm(T).
atm_or_atm_list(T) :- list(T, atm).


:- comment(compat/2,"This property captures the notion of type or
   @concept{property compatibility}. The instantiation or constraint
   state of the term is compatible with the given property, in the
   sense that assuming that imposing that property on the term does
   not render the store inconsistent. For example, terms @tt{X} (i.e.,
   a free variable), @tt{[Y|Z]}, and @tt{[Y,Z]} are all compatible
   with the regular type @pred{list/1}, whereas the terms @tt{f(a)} and
   @tt{[1|2]} are not.").

:- prop compat(Term,Prop) 
   # "@var{Term} is @em{compatible} with @var{Prop}".
:- meta_predicate compat(?, pred(1)).

compat(T, P) :- \+ \+ P(T).

% No comment necessary: it is taken care of specially anyway in the
% automatic documenter.

:- prop iso(G) # "@em{Complies with the ISO-Prolog standard.}".

:- impl_defined([iso/1]).

:- prop not_further_inst(G,V) # "@var{V} is not further instantiated.".

not_further_inst(_,_).

:- prop regtype(G) # "Defines a regular type.".

:- impl_defined([regtype/1]).
