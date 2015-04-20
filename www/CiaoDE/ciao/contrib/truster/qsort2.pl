:- module(qsort2, [qsort/2]).

% :- costmodel qsort(list(number), term) :: [list(uniform(-1,1),
%    uniform(1,2000)), none] => model(100, [X,_Y], [N], (length(X,N)),
%    linearmodel([1, N, N * log(N)])).

qsort(A, B) :-
	qsort_(A, B, []).

qsort_([], R, R).
qsort_([X|L], R, R0) :-	
	split(L, X, L1, L2),
	qsort_(L2, R1, R0),
	qsort_(L1, R, [X|R1]).

split([],_,[],[]).
split([X|L],Y,[X|L1],L2) :-
	X =< Y, !,
	split(L,Y,L1,L2).
split([X|L],Y,L1,[X|L2]) :-
	split(L,Y,L1,L2).
