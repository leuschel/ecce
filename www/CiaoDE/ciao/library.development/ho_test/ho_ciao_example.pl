:- include(ho).

main1 :-
	%% Define a predicate on the fly
	( X(A,B) :- write(A), write(B), nl ),
	%% Pass it to someone else.
	p(X),
	fail.

p(X) :-
	%% Use it!
        X(a,b),
        X(c,d).

main2 :-
	%% Define a predicate on the fly with several clauses!
	( X(A,B) :- write(A), write(B), nl ),
	( X(A,B) :- write(B), write(A), nl ),
	%% Pass it to someone else.
	p(X),
	fail.

main3 :- 
	%% Global var (is a predicate, really)
	Y := 1, 
	( Y := 2, 
	  Y(C),
	  write(C), nl
	; Y(C),
	  write(C), nl ),
	fail.
