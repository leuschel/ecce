:- module(_,_,[]).

main :-
	test1,
	test2,
	test3,
	test4,
	test5.

% {ERROR: Memory allocated out of addressable bounds!}
test1 :-
	X is 1/0 mod 1,
	display(X),nl.

% {ERROR: Memory allocated out of addressable bounds!}
test2 :-
	X is 1 / 0 # 1,
	display(X),nl.

% {ERROR: Memory allocated out of addressable bounds!}
test3 :-
	X is 0 / 0 /\ 1,
	display(X),nl.

% {ERROR: miscalculated size of bignum}
test4 :-
	X is floor(0 / 0),
	display(X),nl.

% {ERROR: miscalculated size of bignum}
test5 :-
	X is truncate( 0 / 0 ),
	display(X),nl.
