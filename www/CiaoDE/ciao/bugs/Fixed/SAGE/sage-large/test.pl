
:- use_module(library(lists),[length/2]).
%% :- use_module(concept_db).
:- use_module(concept_db_1).
%% :- use_module(concept_db_2).
%% :- use_module(concept_db_4).
%% :- use_module(concept_db_8).

main :-
	write('Starting processing...\n'),
	findall(N,concepto(_,_,_,N,_,_,_,_,_),L),
	write('Created list...\n'),
	length(L,LN),
	write('Length is: '),
	write(LN).
