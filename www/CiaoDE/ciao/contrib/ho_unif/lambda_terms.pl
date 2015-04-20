:- module(lambda_terms,
	[
	    deref/2,
	    add_dummies/4,
	    make_lambda/3,
	    bind/2,
	    freshvar/2,
	    new_ref/2,
	    get_ref/2,
	    assign/2,
	    display_ref/1
	],
	[]).


% Dereference a variable
deref(A, X) :-
	get_ref(A, ptr(Y)),
	!,
	deref(Y, X).
deref(X, X).

% 
add_dummies(0, _, E, E) :-
	!.
add_dummies(N, NL, E, [X|T]) :-
	N1 is N - 1,
	X is NL + N1,
	add_dummies(N1, NL, E, T).

% Construct an abstraction from the number of abstractions N and the body T
make_lambda(0, Term, Term):-
	!.
make_lambda(N, Term, Term2) :-
	get_ref(Term, lam(N2, T)),
	!,
	N1 is N + N2,
	new_ref(lam(N1, T), Term2).
make_lambda(N, Term, Term2) :-
	new_ref(lam(N, Term), Term2).

% Binding a variable to a term
bind(V, T) :-
	(
	    (V == T) -> true
	;
	    assign(V, ptr(T))
	).

% Generating a fresh variable with a given time stamp
freshvar(X, Term) :-
	new_ref(var(_,X),Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creates a new ref by assigning an attribute to the variable
new_ref(Y, X) :-
	attach_attribute(X, Y).

% Returns the ref (attribute) of the variable
get_ref(X, Y) :-
	get_attribute(X, Y).

% Assigns a new value to the variable (detach the previous attribute and
% attach a new one
assign(X, Y) :-
	detach_attribute(X),
	attach_attribute(Var, Y),
	X = Var.

% Displays a ref
display_ref(X) :-
	flat_ref(X, V),
	!,
	display(V),display('.').
flat_ref(X, V) :-
	deref(X, Y),
	get_ref(Y, R),
	flat_rawterm(R, V).
flat_ref([], []).
flat_ref([H1|T1], [H2|T2]) :-
	!,
	flat_ref(H1, H2),
	flat_ref(T1, T2).
flat_rawterm(const(X,Y), const(X,Y)) :-
	!.
flat_rawterm(var(X,Y), var(X,Y)) :-
	!.
flat_rawterm(dB(X), dB(X)) :-
	!.
flat_rawterm(lam(X,Y), lam(X,W)) :-
	!,
	flat_ref(Y, W).
flat_rawterm(app(X,Y), app(V,W)) :-
	!,
	flat_ref(X, V),
	flat_ref(Y, W).
flat_rawterm(susp(X,Y,Z,T), susp(A,Y,Z,D)) :-
	!,
	flat_ref(X, A),
	flat_ref(T, D).
flat_rawterm(ptr(X), ptr(V)) :-
	!,
	flat_ref(X, V).
flat_rawterm(dum(X), dum(X)) :-
	!.
flat_rawterm(bndg(X,Y), bndg(V,Y)) :-
	!,
	flat_ref(X, V).
flat_rawterm(X, X).


