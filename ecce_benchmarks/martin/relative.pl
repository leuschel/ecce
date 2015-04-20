:- module(relative, [relative/3]).
:- ensure_loaded(local).


relative(X, Y, trace(1, Anc1, Anc2)):-
    ancestor(Z, X, Anc1),
    ancestor(Z, Y, Anc2).
 
ancestor(X, Y, trace(2, Par)):-
    parent(X, Y, Par).
ancestor(X, Y, trace(3, Par, Anc)):-
    parent(X, Z, Par),
    ancestor(Z, Y, Anc).
 
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


%       carol > albertina > peter
% jap > carol > paulina
% jap > jonas > maria > mary > anna = john
%               maria > jose
%
% anna = john = 0
% mary = jose = peter = 1
% maria = paulina = albertina = 2
% carol = jonas = 3
% jap = 4