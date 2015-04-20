
:- use_module(library(lists)).
:- use_module(library(system)).

a1(What, Thread):-
        display(calling), nl,
        eng_call(What, create, Thread, Id),
        pause(1),
        display('asking 1'), nl,
        eng_backtrack(Id, self),
        pause(1),
        display('asking 2'), nl,
        eng_backtrack(Id, self),
        pause(1),
        display('asking 3'), nl,
        eng_backtrack(Id, self),
        pause(1),
        display('asking 4'), nl,
        eng_backtrack(Id, self),
        pause(1),
        display('asking 5'), nl,
        eng_backtrack(Id, self),
        pause(1),
        display('asking 6'), nl,
        eng_backtrack(Id, self),
        pause(1),
        eng_backtrack(Id, self).



back1:- display(back1_1_last), nl.

back2:- display(back2_1), nl.
back2:- display(back2_last), nl.

back3:- display(back3_1), nl.
back3:- display(back3_2), nl.
back3:- display(back3_last), nl.

back4:- display(back4_1), nl.
back4:- display(back4_2), nl.
back4:- display(back4_3), nl.
back4:- display(back4_last), nl.

back5:- back4(_).

back4(_):- display(back4_1), nl.
back4(_):- display(back4_2), nl.
back4(_):- display(back4_3), nl.
back4(_):- display(back4_last), nl.


backapp:-
        append(A, B, [1,2,3]),
        display(A), nl,
        display(B), nl.
