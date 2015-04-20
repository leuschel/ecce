% Test trail stack overflow

:- module(_, _, []).

f(0, []) :- !.
f(N, [_|Xs]) :-
	N1 is N - 1,
	f(N1, Xs).

s(0, []) :- !.
s(N, [a|Xs]) :- 
	N1 is N - 1,
	s(N1, Xs).

main :-
	f(8400, Xs),
	q, % this choice point forces trailing of variables
	s(8400, Xs),
	!.

q.
q.
