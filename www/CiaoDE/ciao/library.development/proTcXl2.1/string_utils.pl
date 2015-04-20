 %% string_utils.pl -- Several definitions to help in porting from ECLIPSE
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro
 %% Created On      : Tue May 14 19:15:13 1996
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Thu May 16 16:21:44 1996
 %% Update Count    : 11
 %% Status          : Unknown, Use with caution!

atom_string(A, S):- name(A, S).

number_string(A, S):- name(A, S).

concat_string([], []).
concat_string([Atom|Atoms], Result):-
        concat_string(Atoms, R),
        name(Atom, String),
        append(String, R, Result).

concat_atoms(A1, A2, Concat) :-
        name(A1, LA1),
        name(A2, LA2),
        append(LA1, LA2, LA),
        name(Concat, LA).

append([], L, L).
append([X|L1], L2, [X|L3]) :-
    append(L1, L2, L3).
