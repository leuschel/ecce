
append([], X, X).
append([X|Xs], Y, [X|Zs]) :-
	append(Xs,Y,Zs).

string_append([], []).
string_append([L|Ls], R) :-
        string_append(Ls, R0),
        to_string(L, S),
        append(S, R0, R).

atom_append(L, A) :-
        string_append(L, S),
        atom_codes(A,S).

to_string([],"") :- !. % empty string
to_string(A, S) :-
        atomic(A), !,
        name(A, S).
to_string(S, S).

worker(1).
