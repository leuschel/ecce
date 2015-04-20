:- module(mm,_,[assertions]).
:- use_package(debug).

:- spy k_2/1.

:- spy j/1.

k(Xs) :- k_2(Xs).

k_2([]).
k_2([X|Xs]) :- 
         display(X),
	 k_2(Xs),
         nl.

j([]).
j([X|Xs]) :- 
         display(X),
	 j(Xs),
         nl.
