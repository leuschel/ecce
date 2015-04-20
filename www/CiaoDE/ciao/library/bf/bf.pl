
:- use_module(library(aggregates), [findall/4]).

'$bf'(X, Y, _):- X == Y, !, fail. % No (more) solutions.
'$bf'([u([], Goal)|_], _, Goal).
'$bf'([u(Resolvent, U_Goal)|Us], Urest, Goal):-
	'$expand_resolvent'(Resolvent, U_Goal, Urest, NewUrest),
	'$bf'(Us, NewUrest, Goal).

'$expand_resolvent'([], _, X, X).
'$expand_resolvent'([A|Rest], Goal, Us, Us_):-
        ( '$bfpred'(A) ->
            aggregates:findall(u(Body,Goal), '$bfcl'(A, Body, Rest), Us, Us_)
        ; aggregates:findall(u(Rest,Goal), A, Us, Us_)
        ).

:- discontiguous '$bfcl'/3, '$bfpred'/1.

:- include(library('bf/ops')).

:- load_compilation_module(library('bf/bftr')).
:- add_sentence_trans(bftr/3).
