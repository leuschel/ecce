
:- module(hash,[ hash_term/2 ], []).

%% :- pred hash_term(T,N) : ground * var => int(N)
%% 	# "@var{N} is a hashing index for @var{T}.".
%% :- pred hash_term(T,N) : nonground * var => var(N).

hash_term(T,N):-
	ground(T), !,
	hash_term_(T,N).
hash_term(_,_).

% Quintus: hash_term(foo(name,2,module), 1391).

/*         hash_term(foo(name,2,module), 1389). */
hash_term_(T,N):-
	T =.. List,
	hash_list(List,0,N).

hash_list([],N,N).
hash_list([X|Xs],N0,N):-
	hash_one_term(X,N1),
	N2 is N0 + N1,
	hash_list(Xs,N2,N).

hash_one_term(X,N):-
	atom(X), !,
	name(X,L),
	sum_list(L,0,N).
hash_one_term(X,N):-
	number(X), !,
	N = X.
hash_one_term(X,N):-
	hash_term_(X,N).

sum_list([],N,N).
sum_list([X|Xs],N0,N):-
	N1 is N0 + X,
	sum_list(Xs,N1,N).
