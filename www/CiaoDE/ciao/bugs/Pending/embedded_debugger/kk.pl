:- module(kk,_,[assertions]).

:- use_module(mm).

main(Xs) :-
	k(Xs),
	j(Xs). % Embedded debugger skips the first call!! (perhaps because we are not debugging this module?)
