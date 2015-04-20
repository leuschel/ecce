%% try, e.g., 14 -> 7 (also 15 or 16)

:- ensure_loaded(ho_ciao).

static_tak(X,Y,Z,A) :-
   X =< Y,!,
   Z = A.

static_tak(X,Y,Z,A) :- 
   X1 is X - 1,
   Y1 is Y - 1,
   Z1 is Z - 1,
   static_tak(Z1,X,Y,A3),
   static_tak(Y1,Z,X,A2),
   static_tak(X1,Y,Z,A1),
   static_tak(A1,A2,A3,A).

dynamic_tak(OX,OY,OZ,OA) :-
	defpred( [ ( Tak(X,Y,Z,A) :- X =< Y, !, Z=A ),
                   ( Tak(X,Y,Z,A) :- 
		            X1 is X - 1,
			    Y1 is Y - 1,
			    Z1 is Z - 1,
			      Tak(Z1,X,Y,A3),
			      Tak(Y1,Z,X,A2),
			      Tak(X1,Y,Z,A1),
			      Tak(A1,A2,A3,A)   ) ]),
       Tak(OX,OY,OZ,OA).

%% System = static; dynamic
do(System,N) :- 
	atom_concat(System,'_tak',P),
	C =.. [P,N,12,6,X],
	time(_),
	% tak(N,12,6,X)
	call(C),
	time(T),
	write('Executed in '), write(T), write(' mS.'), nl,
	write('Solution = '), write(X), nl.

time(T) :- statistics(runtime,[_,T]).
