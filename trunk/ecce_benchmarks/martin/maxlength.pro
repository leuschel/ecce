maxlength:max_length(Ls,M,Len) :- maxlength:max(Ls,M), maxlength:my_length(Ls,Len).

maxlength:my_length([],0).
maxlength:my_length([X|T],L) :-
	maxlength:my_length(T,LT),
	L is LT + 1.

maxlength:max(X,M) :- maxlength:max1(X,0,M).

maxlength:max1([],M,M).
maxlength:max1([H|T],N,M) :-
	'=<'(H,N),
	maxlength:max1(T,N,M).
maxlength:max1([H|T],N,M) :-
	'>'(H,N),
	maxlength:max1(T,H,M).




