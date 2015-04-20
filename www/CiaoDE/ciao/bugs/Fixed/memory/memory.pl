% Status: FIXED - not a bug, allocates more than 100000*100000/2 cells!!

:- module(_, _, [functions]).

% Do:
% memory 100000 
% or any other big number

:- data dato/2.

main([Arg]) :-
        atom_number(Arg, N),
        assert_many(N).

assert_many(0) :- !.
assert_many(N) :-
        make_list(N, L),
        asserta_fact(dato(N, L)),
        assert_many(--N).

make_list(0, []) :- !.
make_list(N, [N|L]) :- make_list(--N, L).
