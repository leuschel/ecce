:- module(
	backtracking,
	[
	    main1/2,   % with &/2
	    main2/2,   % with &>/2 and <&/1
	    main3/1,   % with &/2, and dependency
	    main4/1    % with &>/2 and <&/1, and dependency
	],
	[andprolog]
	 ).

main1(X, Y) :-
	p(X) & q(Y),
	display(X), display(' = '), display(Y), nl,
	X = Y.

main2(X, Y) :-
	q(Y) &&> L,
	p(X),
	L <&&,
	display(X), display(' = '), display(Y), nl,
	X = Y.

main3(X) :-
	p(X) & q(X).

main4(X) :-
	q(X) &> L,
	p(X),
	L <& .

p(a).
p(b).
q(c).
q(b).


