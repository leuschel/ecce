% not really a bug... but a system limitation
% jfran - 20040322

% ?- main.
% {ERROR: term too large in assert or record}

:- module(_, _, []).

:- data a/1.

repeat(0, []) :- !.
repeat(N, [X,X|Xs]) :- N1 is N - 1, repeat(N1, Xs).

main :-
	repeat(10000, Xs),
	asserta_fact(a(Xs)).
