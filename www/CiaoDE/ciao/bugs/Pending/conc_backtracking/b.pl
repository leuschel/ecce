:- use_module(library(lists), [append/3]).

apb:- 
        append(X, Y, [1, 2, 3]),
        display(X), nl,
        display(Y), nl.

do:-
        launch_goal(apb, Id),
        join_goal(Id),
        backtrack_goal(Id),
        backtrack_goal(Id),
        backtrack_goal(Id),
        backtrack_goal(Id),
        backtrack_goal(Id).

do1 :-
        launch_goal(q, Id),
        join_goal(Id),
        backtrack_goal(Id),
        backtrack_goal(Id),
        backtrack_goal(Id),
        backtrack_goal(Id).

q:-
        queens(4, Q),
        display(Q), nl.

p(_):- display('First clause finished'), nl.
        
p(N):-
        queens(N, Q),
        display(Q), 
        nl.

queens(N, Qs):-
        queens_list(N, Ns),
        queens3(Ns, [], Qs).    % To place, placed, result

queens_list(0, []).
queens_list(N, [N|Ns]):-
        N > 0,
        N1 is N - 1,
        queens_list(N1, Ns).


queens3([], Qs, Qs).
queens3(Unplaced, Placed, Qs):-
        sel(Q, Unplaced, NewUnplaced),
        no_attack_(Q, Placed),
        queens3(NewUnplaced, [Q|Placed], Qs).

no_attack_(Q, Safe):- no_attack(Safe, Q, 1).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
        Queen =\= Y + Nb,
        Queen =\= Y - Nb,
        Nb1 is Nb + 1,
        no_attack(Ys, Queen, Nb1).

sel(X, [X|Ys], Ys).
sel(X, [Y|Ys], [Y|Zs]):-
        sel(X, Ys, Zs).
