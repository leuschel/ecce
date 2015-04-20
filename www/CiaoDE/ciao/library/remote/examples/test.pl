
:- include(library(hlc)).  
:- use_package(library(clpq)).  
:- include(library('remote/ciao_client')).  


p(a).
constrain(_X, _Y, Z):- Z .=. 9.

s(X):- 
        p(X) &&> H, H <&& .

test(X, Y, Z, Host):-
        d(1),
        d(2),
        5*Y+7*X .=. 3,
        constrain(X, Y, Z) @ Host &&> H,
        d(3),
        X+Y+Z .=. 0,
        d(4),
        H <&&,
        d(5).

t(X, Y, Z):-
        X+Y+Z .=. 0,
        constrain(A, B, C),
        asserta(c(A, B, C)),
        5*Y+7*X .=. 3,
        retract(c(X, Y, Z)).

d(X):-
        display(X),
        nl.
