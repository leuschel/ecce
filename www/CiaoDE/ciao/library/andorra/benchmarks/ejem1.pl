:- module(ejem1,_,[andorra]).


:- use_module(defineq).
:- use_module(library(write)).


:- determinate(p(X),nonvar(X)).

p(1):- display(p(1)),nl.
p(2):- display(p(2)),nl.



%r(1):- display(r(1)),nl.
r(X):- display(r(1)),nl, X = 1.

s(1):- display(s(1)),nl.
s(2):- display(s(2)),nl.
 

:- determinate(main(_,_),true).

main(X,Y):- p(X),q(1,Y),Y = f(M),s(X),r(M).

main2(X,Y) :- pred1(X,Y), Y = c.

main3(A,F) :-  inst(A), atom_concat(A,hola,F),display(F).

inst(atom1).
inst(atom2).

pred1(X,Y) :- pred2(X),pred3(Y).
pred1(_X,c).

pred2(a):- display(pred2(a)).
pred2(b):- display(pred2(b)).  

pred3(c):- display(pred3(c)).
pred3(d):- display(pred3(d)).


rec(a).
rec(f(X)):- rec(X).


:- determinate(fib(X,Y),(nonvar(X);(nonvar(Y),Y \== 1))).

fib(0,1).
fib(1,1).
fib(A,B):-
 	A > 1,
 	C is A - 1,
 	D is A - 2,
 	fib(C,E),
 	fib(D,F),
 	B is F + E.
