:- module(queens_with_delays, [queens/2]).

queens(N, Qs):-
        queens_constrain(N, Qs), !, %% Constraint diagonals
        queens_search(N, Qs).       %% Nondet. put a queen in each column

queens_search(0, []).               %% Finished
queens_search(ThisQueen, ToPlace):-
        ThisQueen > 0,
        select(ThisQueen, ToPlace, NewToPlace),
        OtherQueen is ThisQueen - 1,
        queens_search(OtherQueen, NewToPlace).

queens_constrain(0, []).
queens_constrain(N, [Q|Qs]):-
        N > 0,
        N1 is N - 1,
        no_attack(N1, Qs, Q, 1),
        queens_constrain(N1, Qs).

no_attack(0, [],    _Queen, _Nb).
no_attack(N, [Y|Ys], Queen,  Nb):-
        N > 0,
        when((ground(Queen), ground(Nb), ground(Y)), 
              test_no_attack(Queen, Y, Nb)),
        Nb1 is Nb + 1,
        N1 is N - 1,
        no_attack(N1, Ys, Queen, Nb1).

test_no_attack(Queen, Y, Nb):-
        Queen + Nb =\= Y,
        Queen - Nb =\= Y.
