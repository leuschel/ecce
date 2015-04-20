
regexp1:generate(empty,T,T).
regexp1:generate(char(X),[X|T],T).
regexp1:generate(or(X,Y),H,T) :-
	regexp1:generate(X,H,T).
regexp1:generate(or(X,Y),H,T) :-
	regexp1:generate(Y,H,T).
regexp1:generate(cat(X,Y),H,T) :-
	regexp1:generate(X,H,T1),
	regexp1:generate(Y,T1,T).
regexp1:generate(star(X),T,T).
regexp1:generate(star(X),H,T) :-
	regexp1:generate(X,H,T1),
	regexp1:generate(star(X),T1,T).

