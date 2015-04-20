:- module(queens, [queens/2]).

:- set_prolog_flag(multi_arity_warnings, off).


queens(N, Qs):-
        queens_list(N, Ns),
        queens(Ns, [], Qs).    % To place, placed, result

queens([], Qs, Qs).
queens(Unplaced, Placed, Qs):-
        select(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens(NewUnplaced, [Q|Placed], Qs).

no_attack(Q, Safe):- no_attack(Safe, Q, 1).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
        Queen =\= Y + Nb,
        Queen =\= Y - Nb,
        Nb1 is Nb + 1,
        no_attack(Ys, Queen, Nb1).

select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]):-
        select(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]):-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).
