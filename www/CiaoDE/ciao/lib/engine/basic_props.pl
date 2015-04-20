:- module(basic_props,
        [term/1, int/1, nnegint/1, flt/1, num/1, atm/1, struct/1, gnd/1,
	 constant/1,
         callable/1, operator_specifier/1, list/1, list/2, member/2,
         sequence/2, sequence_or_list/2, character_code/1, string/1,
         num_code/1, predname/1, atm_or_atm_list/1, compat/2, inst/2,
         iso/1, not_further_inst/2, sideff/2,
	 regtype/1, native/1, native/2,
	 eval/1, equiv/2,
	 bind_ins/1, error_free/1,memo/1,filter/2
        ],
        [assertions]).

%% Commented out to avoid including hiord_rt in all executables, 
%% put declarations instead:
%% :- use_package(hiord).
:- set_prolog_flag(read_hiord, on).
:- import(hiord_rt, [call/2]).

:- use_module( library( terms_check ) , [instance/2] ).


:- comment(title,"Basic data types and properties").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"@cindex{properties, basic} This library contains
   the set of basic properties used by the builtin predicates, and
   which constitute the basic data types and properties of the
   language.  They can be used both as type testing builtins within
   programs (by calling them explicitly) and as properties in
   assertions.").

:- comment(term/1, "The most general type (includes all possible
   terms).").

:- true prop term(X) + ( regtype, native ) # "@var{X} is any term.".
:- true comp term(X) + sideff(free).
:- true comp term(X) + eval.
:- true comp term(X) + equiv(true).
:- trust success term(_) => true.


term(_).

:- comment(int/1, "The type of integers. The range of integers is
        @tt{[-2^2147483616, 2^2147483616)}.  Thus for all practical
        purposes, the range of integers can be considered infinite.").

:- true prop int(T) + ( regtype, native ) # "@var{T} is an integer.".
:- true comp int(T) + sideff(free).
:- true comp int(T) : nonvar(T) + eval.
%:- trust success int(T) => integer(T).


int(X) :-
        nonvar(X), !,
        integer(X).
int(0).
int(N) :- posint(I), give_sign(I, N).

posint(1).
posint(N) :- posint(N1), N is N1+1.

give_sign(P, P).
give_sign(P, N) :- N is -P.

:- comment(nnegint/1, "The type of non-negative integers, i.e.,
	natural numbers.").

:- true prop nnegint(T) + ( regtype, native )
	# "@var{T} is a non-negative integer.".
:- true comp nnegint(T) + sideff(free).
:- true comp nnegint(T) : nonvar(T) + eval.
:- trust success nnegint(T) => ground(T).


nnegint(X) :-
        nonvar(X), !,
        integer(X),
	X >= 0.
nnegint(0).
nnegint(N) :- posint(N).


:- comment(flt/1, "The type of floating-point numbers. The range of
        floats is the one provided by the C @tt{double} type, typically
        @tt{[4.9e-324, 1.8e+308]} (plus or minus).  There are also three
        special values: Infinity, either positive or negative,
        represented as @tt{1.0e1000} and @tt{-1.0e1000}; and
        Not-a-number, which arises as the result of indeterminate
        operations, represented as @tt{0.Nan}").

:- true prop flt(T) + ( regtype, native ) # "@var{T} is a float.".
:- true comp flt(T) + sideff(free).
:- true comp flt(T) : nonvar(T) + eval.
:- trust success flt(T) => ground(T).

flt(T) :- nonvar(T), !, float(T).
flt(T) :- int(N), T is N/10.

:- comment(num/1, "The type of numbers, that is, integer or floating-point.").

:- true prop num(T) + ( regtype, native ) # "@var{T} is a number.".
:- true comp num(T) + (sideff(free),bind_ins).
:- true comp num(T) : nonvar(T) + eval.
%:- trust success num(T) => ground(T).
:- trust success num(T) => num(T).

num(T) :- number(T), !.
num(T) :- int(T).
% num(T) :- flt(T). % never reached!

:- comment(atm/1, "The type of atoms, or non-numeric constants.  The
        size of atoms is unbound.").

:- true prop atm(T) + ( regtype, native ) # "@var{T} is an atom.".
:- true comp atm(T) + sideff(free).
:- true comp atm(T) : nonvar(T) + eval.
:- trust success atm(T) => ground(T).

% Should be current_atom/1
atm(a).
atm(T) :- atom(T).

:- comment(struct/1, "The type of compound terms, or terms with
non-zeroary functors. By now there is a limit of 255 arguments.").

:- true prop struct(T) + ( regtype, native ) # "@var{T} is a compound term.".
:- true comp struct(T) + sideff(free).
:- true comp struct(T) : nonvar(T) + eval.
:- trust success struct(T) => nonvar(T).

struct([_|_]):- !.
struct(T) :- functor(T, _, A), A>0. % compound(T).

:- comment(gnd/1, "The type of all terms without variables.").

:- true prop gnd(T) + ( regtype, native ) # "@var{T} is ground.".
:- true comp gnd(T) + sideff(free).
:- true comp gnd(T) : ground(T) + eval.
:- trust success gnd(T) => ground(T).

gnd([]) :- !.
gnd(T) :- functor(T, _, A), grnd_args(A, T).

grnd_args(0, _).
grnd_args(N, T) :-
        arg(N, T, A),
        gnd(A),
        N1 is N-1,
        grnd_args(N1, T).

:- true prop constant(T) + regtype
   # "@var{T} is an atomic term (an atom or a number).".
:- true comp constant(T) + sideff(free).
:- true comp constant(T) : nonvar(T) + eval.
:- trust success constant(T) => ground(T).

constant(T) :- atm(T).
constant(T) :- num(T).

:- true prop callable(T) + regtype
   # "@var{T} is a term which represents a goal, i.e.,
        an atom or a structure.".
:- true comp callable(T) + sideff(free).
:- true comp callable(T) : nonvar(T) + eval.
:- trust success callable(T) => nonvar(T).

callable(T) :- atm(T).
callable(T) :- struct(T).

:- comment(operator_specifier/1, "The type and associativity of an
operator is described by the following mnemonic atoms:

@begin{description}

@item{@tt{xfx}} Infix, non-associative: it is a requirement that both of
the two subexpressions which are the arguments of the operator must be
of @em{lower} precedence than the operator itself.

@item{@tt{xfy}} Infix, right-associative: only the first (left-hand)
subexpression must be of lower precedence; the right-hand subexpression
can be of the @em{same} precedence as the main operator.

@item{@tt{yfx}} Infix, left-associative: same as above, but the other
way around.

@item{@tt{fx}} Prefix, non-associative: the subexpression must be of
@em{lower} precedence than the operator.

@item{@tt{fy}} Prefix, associative: the subexpression can be of the
@em{same} precedence as the operator.

@item{@tt{xf}} Postfix, non-associative: the subexpression must be of
@em{lower} precedence than the operator.

@item{@tt{yf}} Postfix, associative: the subexpression can be of the
@em{same} precedence as the operator.

@end{description}
").

:- true prop operator_specifier(X) + regtype # "@var{X} specifies the type and
        associativity of an operator.".
:- true comp operator_specifier(X) + sideff(free).
:- true comp operator_specifier(X) : nonvar(X) + eval.
:- trust success operator_specifier(T) => ground(T).

operator_specifier(fy).
operator_specifier(fx).
operator_specifier(yfx).
operator_specifier(xfy).
operator_specifier(xfx).
operator_specifier(yf).
operator_specifier(xf).

:- comment(list/1, "A list is formed with successive applications of the
   functor @tt{'.'/2}, and its end is the atom @tt{[]}.  Defined as
   @includedef{list/1}").

:- true prop list(L) + regtype # "@var{L} is a list.".
:- true comp list(L) + sideff(free).
:- true comp list(L) : ground(L) + eval.
:- trust success list(T) => nonvar(T).

list([]).
list([_|L]) :- list(L).

:- comment(list(L,T), "@var{L} is a list, and for all its elements,
   @var{T} holds.").

:- true prop list(L,T) + regtype # "@var{L} is a list of @var{T}s.".
:- true comp list(L,T) + sideff(free).
:- meta_predicate list(?, pred(1)).
:- true comp list(L,T) : (ground(L),ground(T)) + eval.
:- trust success list(X,T) => (nonvar(X), ground(T)).

list([],_).
list([X|Xs], T) :-
        T(X),
        list(Xs, T).

:- true prop member(X,L) # "@var{X} is an element of @var{L}.".
:- true comp member(X,L) + (sideff(free), bind_ins).
:- true comp member(X,L) : list(L) + eval.
:- trust success member(_X,L) => nonvar(L).

member(X, [X|_]).
member(X, [_Y|Xs]):- member(X, Xs).

:- comment(sequence/2, "A sequence is formed with zero, one or more
   occurrences of the operator @op{','/2}.  For example, @tt{a, b, c} is
   a sequence of three atoms, @tt{a} is a sequence of one atom.").

:- true prop sequence(S,T) + regtype # "@var{S} is a sequence of @var{T}s.".
:- true comp sequence(S,T) + sideff(free).

:- meta_predicate sequence(?, pred(1)).
:- true comp sequence(S,T) : (ground(S), ground(T)) + eval.
:- trust success sequence(E,T) => (nonvar(E),ground(T)).

sequence(E, T) :- T(E).
sequence((E,S), T) :-
        T(E),
        sequence(S,T).

:- true prop sequence_or_list(S,T) + regtype
   # "@var{S} is a sequence or list of @var{T}s.".
:- true comp sequence_or_list(S,T) + sideff(free).
:- meta_predicate sequence_or_list(?, pred(1)).
:- true comp sequence_or_list(S,T) : (ground(S),ground(T)) + eval.
:- trust success sequence_or_list(E,T) => (nonvar(E),ground(T)).

sequence_or_list(E, T) :- list(E,T).
sequence_or_list(E, T) :- sequence(E, T).

:- true prop character_code(T) + regtype
   # "@var{T} is an integer which is a character code.".
:- true success character_code(T) => int.
:- true comp character_code(T) + sideff(free).
:- true comp character_code(T) : nonvar(T) + eval.
:- trust success character_code(I) => ground(I).

character_code(I) :- int(I).

:- comment(string/1, "A string is a list of character codes.  The usual
        syntax for strings @tt{\"string\"} is allowed, which is
        equivalent to @tt{[0's,0't,0'r,0'i,0'n,0'g]} or
        @tt{[115,116,114,105,110,103]}.  There is also a special Ciao
        syntax when the list is not complete: @tt{\"st\"||R} is
        equivalent to @tt{[0's,0't|R]}.").

:- true prop string(T) + regtype
   # "@var{T} is a string (a list of character codes).".
:- true success string(T) => list(character_code).
:- true comp string(T) + sideff(free).
:- true comp string(T) : ground(T) + eval.
:- trust success string(T) => nonvar(T).

string(T) :- list(T, character_code).

:- comment(num_code/1, "These are the ASCII codes which can appear in 
	decimal representation of floating point and integer numbers, 
	including scientific notation and fractionary part.").

:-  true prop num_code/1 + regtype.

num_code(0'0).
num_code(0'1).
num_code(0'2).
num_code(0'3).
num_code(0'4).
num_code(0'5).
num_code(0'6).
num_code(0'7).
num_code(0'8).
num_code(0'9).
num_code(0'.).
num_code(0'e).
num_code(0'E).
num_code(0'+).
num_code(0'-).

/*
:- comment(predname(P),"@var{P} is a Name/Arity structure denoting
	a predicate name: @includedef{predname/1}").
:- true prop predname(P) + regtype
   # "@var{P} is a predicate name spec @tt{atm}/@tt{int}.".
*/
:- true prop predname(P) + regtype
   # "@var{P} is a Name/Arity structure denoting
	a predicate name: @includedef{predname/1}".
:- true comp predname(P) + sideff(free).
:- true comp predname(P) : ground(P) + eval.
:- trust success predname(P) => ground(P).

predname(P/A) :-
	atm(P),
	nnegint(A).

:- true prop atm_or_atm_list(T) + regtype
   # "@var{T} is an atom or a list of atoms.".
:- true comp atm_or_atm_list(T) + sideff(free).
:- true comp atm_or_atm_list(T) : ground(T) + eval.
:- trust success atm_or_atm_list(T) => ground(T).

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

:- true prop compat(Term,Prop)
   # "@var{Term} is @em{compatible} with @var{Prop}".
:- meta_predicate compat(?, pred(1)).
% not complety sure that assertiong below is completely correct,
% unless side effects analysis understand pred(1) (metacalls).
%:- true comp compat(Term,Prop) + sideff(free).
:- true comp compat(Term,Prop) : (ground(Term),ground(Prop)) + eval.

compat(T, P) :- \+ \+ P(T).

% No comment necessary: it is taken care of specially anyway in the
% automatic documenter. (PBC: I guess this comment refers to compat/2)

:- true prop inst(Term,Prop)
	# "@var{Term} is instantiated enough to satisfy @var{Prop}.".
:- true comp inst(Term,Prop) + sideff(free).
:- true comp inst(Term,Prop) : (ground(Term),ground(Prop)) + eval.

:- meta_predicate inst(?,pred(1)).

inst( X , Prop ) :-
	A = Prop( X ),
	copy_term( A , AC ),
	AC,
	instance( A , AC ).

:- true prop iso(G) # "@em{Complies with the ISO-Prolog standard.}".
:- true comp iso(G) + sideff(free).

:- impl_defined([iso/1]).

:- true prop not_further_inst(G,V)
        # "@var{V} is not further instantiated.". % by the predicate
:- true comp not_further_inst(G,V) + sideff(free).

:- impl_defined(not_further_inst/2).

:- true comp sideff(G,X) + ( native, sideff(free) ).
:- prop sideff(G,X) : member(X,[free,soft,hard])
# "@var{G} is side-effect @var{X}.".
:- comment(sideff(G,X),"Declares that @var{G} is side-effect free
   (if its execution has no observable result other than its success,
   its failure, or its abortion), soft (if its execution may have other
   observable results which, however, do not affect subsequent execution,
   e.g., input/output), or hard (e.g., assert/retract).").
:- meta_predicate sideff(goal,?).

:- impl_defined(sideff/2).

% Built-in in CiaoPP
:- true prop regtype(G) # "Defines a regular type.".
:- meta_predicate regtype(goal).
:- true comp regtype(G) + sideff(free).

:- impl_defined(regtype/1).

% Built-in in CiaoPP
:- true prop native(Pred,Key)
   # "This predicate is understood natively by CiaoPP as @var{Key}.".
%%   # "Predicate @var{Pred} is understood natively by CiaoPP as @var{Key}.".
:- true comp native(P,K) + sideff(free).
:- meta_predicate native(goal,?).

:- impl_defined(native/2).

% Built-in in CiaoPP
:- true prop native(Pred)
   # "This predicate is understood natively by CiaoPP.".
%%   # "Predicate @var{Pred} is understood natively by CiaoPP.".
:- true comp native(P) + sideff(free).
:- meta_predicate native(goal).

native(X):- native(X,X).

:- true prop eval(Goal) # "@var{Goal} is evaluable at compile-time.".

:- impl_defined(eval/1).

:- true prop equiv(Goal1,Goal2)
	# "@var{Goal1} is equivalent to @var{Goal2}.".

:- meta_predicate equiv(?,goal).

:- impl_defined(equiv/2).

:- true prop bind_ins(Goal) # "@var{Goal} is binding insensitive.".

:- impl_defined(bind_ins/1).

:- true prop error_free(Goal) # "@var{Goal} is error free.".

:- impl_defined(error_free/1).

:- true prop memo(Goal) # "@var{Goal} should be memoized (not unfolded).".

:- impl_defined(memo/1).

:- true prop filter(Vars,Goal) # "@var{Vars} should be filtered during 
	global control).".

:- impl_defined(filter/2).
