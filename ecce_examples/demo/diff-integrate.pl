
plus(0,X,X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

diffs(N,X,0) :- num(N).
diffs(var(C),X,0) :- C\==X.
diffs(var(X),X,s(0)).
diffs('+'(E1,E2),X,'+'(DE1,DE2)) :-
	diffs(E1,X,DE1),diffs(E2,X,DE2).

num(0).
num(s(X)).



dx(E,R) :- diff(E,x,R1), simplify(R1,R).

diff(num(C),X,num(0)).
diff(var(C),X,num(0)) :- C\==X.
diff(var(X),X,num(1)).
diff('+'(E1,E2),X,'+'(DE1,DE2)) :-
	diff(E1,X,DE1),diff(E2,X,DE2).
diff('*'(E1,E2),X,'+'('*'(DE1,E1),'*'(E1,DE2))) :-
	diff(E1,X,DE1),diff(E2,X,DE2).
diff('**'(var(X),num(C)),X,'*'(num(C),'**'(var(X),num(C1)))) :-
	(var(C1) -> (C1 is C - 1) ; (C is C1 + 1)).

simplify('+'(E1,E2),SE) :-
	simplify(E1,SE1),
	simplify(E2,SE2),!,
	(SE1 = num(N1)
	 -> ((N1=0) -> (SE = SE2)
	 	    ;  ((SE2 = num(N2)) -> (N3 is N1+N2,SE = num(N3))
	 	    			 ; (SE = '+'(SE1,SE2))
	 	    	)
	    )
	 ;  ((SE2 = num(N2))
	 	-> ((N2=0) -> (SE = SE1) ; (SE = '+'(SE1,SE2)))
	 	;  (SE = '+'(SE1,SE2))
	    )
	).
simplify('*'(E1,E2),SE) :-
	simplify(E1,SE1),
	simplify(E2,SE2),!,
	(SE1 = num(N1)
	 -> ((N1=1) -> (SE = SE2)
	 	; (N1 = 0 -> SE = num(0)
	 	    ;  ((SE2 = num(N2)) -> (N3 is N1*N2,SE = num(N3))
	 	    			 ; (SE = '*'(SE1,SE2))
	 	    	)
	 	  )
	    )
	 ;  ((SE2 = num(N2))
	 	-> ((N2=1) -> (SE = SE1) ; (N2 = 0 -> SE = num(0)
	 					   ; SE = '*'(SE1,SE2)))
	 	;  (SE = '*'(SE1,SE2))
	    )
	).
simplify('**'(E1,E2),SE) :-
	simplify(E1,SE1),
	simplify(E2,SE2),!,
	(SE1 = num(N1)
	 -> ((N1=1) -> (SE = num(1))
	 	; (N1 = 0 -> SE = num(0)
	 	    ;  ((SE2 = num(N2)) -> (N3 is N1**N2,SE = num(N3))
	 	    			 ; (SE = '**'(SE1,SE2))
	 	    	)
	 	  )
	    )
	 ;  ((SE2 = num(N2))
	 	-> ((N2=1) -> (SE = SE1) ; (N2 = 0 -> SE = num(1)
	 					   ; SE = '**'(SE1,SE2)))
	 	;  (SE = '**'(SE1,SE2))
	    )
	).
simplify(E,E).
