max_length(Ls,M,Len) :- max(Ls,M), my_length(Ls,Len).

my_length([],0).
my_length([X|T],L) :-
	my_length(T,LT),
	L is LT + 1.

max(X,M) :- max1(X,0,M).

max1([],M,M).
max1([H|T],N,M) :-
	'=<'(H,N),
	max1(T,N,M).
max1([H|T],N,M) :-
	'>'(H,N),
	max1(T,H,M).




