
:- module(hash_,[ hash_term/2 ], []).

%% :- comment(author,"Daniel Diaz").

%% :- pred hash_term(T,N) : ground * var => int(N)
%% 	# "@var{N} is a hashing index for @var{T}.".
%% :- pred hash_term(T,N) : nonground * any + throws_exception.

hash_term(T, H) :-
	term_hash1(T, H0),
	H is H0 /\ 268435455 .

term_hash1(X, _) :-
	var(X), !,
	throw(error(instantiation_error, term_hash / 2)).

term_hash1(X, H) :-
	atom(X), !,
	atom_hash(X, H).

term_hash1(X, H) :-
	integer(X), !,
	H = X.

term_hash1(X, H) :-
	float(X), !,
	H is round(float_fractional_part(X) * 100000) \/ round(float_integer_part(X)).

term_hash1(T, H) :-
	functor(T, F, N),
	atom_hash(F, H0),
	term_hash2(0, N, T, 0, H1),
	H is H1 * 129 + H0.

term_hash2(I, N, T, H0, H) :-
	(   I = N ->
	    H = H0
	;   I1 is I + 1,
	    arg(I1, T, A),
	    term_hash1(A, H1),
	    H2 is H0 * 129 + H1,
	    term_hash2(I1, N, T, H2, H)
	).
