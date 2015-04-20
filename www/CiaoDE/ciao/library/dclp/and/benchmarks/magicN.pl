:- use_package(library('dclp/and')).
:- use_module(library(prolog_sys), [statistics/2]).

% solve the magic square puzzle for a N*N board
:- use_module(library(lists)).

main(N, VSq) :-
       Last is N*N,
       length(Vars, Last),
       Sum is integer(N*(N**2 + 1)/2), 
       all_prefix(Vars, N, VSq), 
       do_diags(VSq, N, Sum, 1, Last),
       all_different(Vars) @ 1,
	do_rows(VSq, Sum, 2, Last),
	all_different(Vars) @ 2,
       do_cols(VSq, Sum, 3, Last),
       all_different(Vars) @ 3,
       d_labeling(Vars).

do_diags(VSq, N, Sum, Ag, Last) :-
        do_diag1(VSq, 1, Sum, Ag, Last),
        do_diag2(VSq, N, Sum, Ag, Last).

do_diag1([],_,0,_,_).
do_diag1([H|T], N, Sum, Ag, Last) :-
	nth(N, H, HElt),
	store([HElt], Ag),
       N1 is N + 1,
       (HElt in 1 .. Last) @ Ag,
       do_diag1(T, N1, TSum, Ag, Last),
       (Sum .=. HElt + TSum) @ Ag.

do_diag2([],_,0,_,_).
do_diag2([H|T], N, Sum, Ag, Last) :-
	nth(N, H, HElt),
	store([HElt], Ag),
	N1 is N - 1,
	(HElt in 1 .. Last) @ Ag,
	do_diag2(T, N1, TSum, Ag, Last),
	(Sum .=. HElt + TSum) @ Ag.

do_rows([],_,_,_). 
do_rows([L|T1], Sum, Ag, Last) :-
	store(L, Ag),
	do_row(L, Sum, Ag, Last),
	do_rows(T1, Sum, Ag, Last).

do_row([], 0,_,_).
do_row([H|T], Sum, Ag, Last) :-
	(H in 1..Last) @ Ag,
	do_row(T, Sum1, Ag, Last),
	(Sum .=. H + Sum1) @ Ag.

do_cols([[]|_],_,_,_).
do_cols(List, Sum, Ag, Last) :-
        do_col(List, Sum, Tails, Ag, Last),
        do_cols(Tails, Sum, Ag, Last).

do_col([],0, [],_,_).
do_col([[H|T]|T1], Sum, [T|T2], Ag, Last) :-
	store([H], Ag),
       (H in 1..Last) @ Ag,
       do_col(T1, TSum, T2, Ag, Last),
	(Sum .=. H + TSum) @ Ag.

%split up big list into a list of lists
all_prefix([],_,[]).
all_prefix(List, N, [List1|Rest]) :-
        prefix(List, N, List1, Residue),
        all_prefix(Residue, N, Rest).

%strip the first N elements of list and return what's left
prefix(List,0,[],List).
prefix([H|T],N,[H|T2],Out) :-
        N > 0,
        N1 is N - 1,
        prefix(T, N1, T2, Out).
