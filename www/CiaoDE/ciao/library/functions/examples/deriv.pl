:- module(deriv, [der/2, dere/2, derf/2], [functions]).

:- use_module(library(arithpreds)). % For dere/2 & derf/2

% Using Predefined settings

der(x)       :=  1.
der(C)       :=  0                    :- number(C).
der(^(A+B))  := ^(~der(A) + ~der(B)).
der(^(C*A))  := ^(C * ~der(A))        :- number(C).
der(^(x**N)) := ^(N * ^(x**(N-1)))    :- integer(N), N>0.

% Disabling arithmetic functor evaluation

:- function(arith(false)).

dere(x)    :=  1.
dere(C)    :=  0                  :- number(C).
dere(A+B)  := ~dere(A) + ~dere(B).
dere(C*A)  := C * ~dere(A)        :- number(C).
dere(x**N) := N * x ** ~(N-1)     :- integer(N), N>0.

% Declaring function derf

:- function(derf/1).

derf(x)    :=  1.
derf(C)    :=  0                 :- number(C).
derf(A+B)  := derf(A) + derf(B).
derf(C*A)  := C * derf(A)        :- number(C).
derf(x**N) := N * x ** ~(N-1)    :- integer(N), N>0.
