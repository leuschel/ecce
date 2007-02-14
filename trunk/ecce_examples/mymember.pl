main(T):-
	mymember(a,[a,b,c,d|T]),
	mymember(b,T).

mymember(X,[X|_]).
mymember(X,[_|T]):- mymember(X,T).
