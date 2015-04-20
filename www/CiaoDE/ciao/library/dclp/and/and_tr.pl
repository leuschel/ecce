:- module(and_tr, [and_tr/2], []).

:- include(library('fd/fd_syntax')).
:- include(syntax).

and_tr((parse(A .=. B)), R) :-
	replace('.=.', A, B, R).
and_tr((parse(A .>. B)), R) :-
	replace('.>.', A, B, R).
and_tr((parse(A .<. B)), R) :-
	replace('.<.', A, B, R).
and_tr((parse(A .=<. B)), R) :-
	replace('.=<.', A, B, R).
and_tr((parse(A .>=. B)), R) :-
	replace('.>=.', A, B, R).
and_tr((parse(A .<>. B)), R) :-
	replace('.<>.', A, B, R).
and_tr((X in A ..B), R) :-
	replace_in('..', X, A, B, R).

replace(Op, A, B, R) :-
	relational(Op), 
	do_replace(Op, A, B, R1),
	((integer(A), integer(B)) ->
	  R = R1
	;
	  R = R1 @ 0
	).

do_replace('.=.', A, B, ('$parse'(A .=. B))) .
do_replace('.>.', A, B, ('$parse'(A .>. B))) .
do_replace('.<.', A, B, ('$parse'(A .<. B))) .
do_replace('.=<.', A, B, ('$parse'(A .=<. B))) .
do_replace('.>=.', A, B, ('$parse'(A .>=. B))) .
do_replace('.<>.', A, B, ('$parse'(A .<>. B))) .

replace_in(Op, X, A, B, R) :-
	domain(Op),
	do_replace_in(Op, X, A, B, R1), 
	(integer(X) ->
	 R = R1
	;
	 R = R1 @ 0
	).	

do_replace_in('..', X, A, B, (X in A .. B)).

relational('.=.').
relational('.>.').
relational('.<.').
relational('.=<.').
relational('.>=.').
relational('.<>.').

domain('..').
