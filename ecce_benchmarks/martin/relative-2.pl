:- module(relative, [relative/3]).
:- ensure_loaded(local).


relative(X, Y, trace(1, Anc1, Anc2)):-
    ancestor(Z, X, Anc1),
    ancestor(Z, Y, Anc2).
 
ancestor(X, Y, Trace):-
    non_rigid_norm(X, Level),
    ancestor(X, Y, Level, unbounded, Trace).

ancestor(X, Y, D, Mode, Trace):-
    (D >= 0 ->
          ancestor1(X, Y, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, Level),
          freeze(Level, ancestor(X, Y, Level, bounded, Trace))
    ).

ancestor1(X, Y, _, _, trace(2, Par)):-
    parent(X, Y, Par).
ancestor1(X, Y, D, Mode, trace(3, Par, Anc)):-
    D1 is D - 1,
    parent(X, Z, Par),
    ancestor(Z, Y, D1, Mode, Anc).
%    ancestor(Z, Y, D1, Mode, Par),
%    parent(X, Z, Anc).
 
parent(X, Y, trace(4, Fat)):-
    father(X, Y, Fat).
parent(X, Y, trace(5, Mot)):-
    mother(X, Y, Mot).
 

father(jap,carol, 6).
father(jap,jonas, 7).
father(jonas,maria, 8).
mother(carol,paulina, 9).
mother(carol,albertina, 10).
mother(albertina,peter, 11).
mother(maria,mary, 12).
mother(maria,jose, 13).
mother(mary,anna, 14).
mother(mary,john, 15).


non_rigid_norm(V, 0):-
    var(V), !.
non_rigid_norm(anna, 0).
non_rigid_norm(john, 0).
non_rigid_norm(mary, 1).
non_rigid_norm(jose, 1).
non_rigid_norm(peter, 1).
non_rigid_norm(maria, 2).
non_rigid_norm(paulina, 2).
non_rigid_norm(albertina, 2).
non_rigid_norm(carol, 3).
non_rigid_norm(jonas, 3).
non_rigid_norm(jap, 4).

:- block  rigid_norm(-,?).

rigid_norm(X, Y):-
    non_rigid_norm(X, Y).
