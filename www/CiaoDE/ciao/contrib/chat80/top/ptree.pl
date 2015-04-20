
:- module(ptree,[ print_tree/1 ],[ ]).
/* Print term as a tree */

:- use_module(library(write)).

:- include('../chatops').

/*
 :- mode print_tree(+).
 :- mode pt(+,+).
 :- mode pl(+,+).
 :- mode as_is(+).
*/

print_tree(T) :-
   numbervars(T,1,_),
   pt(T,0), nl, fail.
print_tree(_).

pt(A,I) :-
   as_is(A), !,
   tab(I), write(A), nl.
pt([T|Ts],I) :- !,
   pt(T,I),
   pl(Ts,I).
pt(T,I) :- !,
   T=..[F|As],
   tab(I), write(F), nl,
   I0 is I+3,
   pl(As,I0).

pl([],_) :- !.
pl([A|As],I) :- !,
   pt(A,I),
   pl(As,I).

as_is(A) :- atomic(A), !.
as_is('$VAR'(_)) :- !.
as_is(X) :-
   quote(X).

quote(A&R) :-
   atom(A), !,
   quote_amp(R).
quote(_-_).
quote(_--_).
quote(_+_).
quote(verb(_,_,_,_,_)).
quote(wh(_)).
quote(name(_)).
quote(prep(_)).
quote(det(_)).
quote(quant(_,_)).
quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote(R).
