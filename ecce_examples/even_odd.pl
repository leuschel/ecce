even(0).
even(s(X)) :- odd(X).
odd(s(X)) :- even(X).

eo(X) :- even(X),odd(X).

gen([1]).
gen([0|X]) :- gen(X).

trans([0,1|T],[1,0|T]).
trans([H|T],[H|T1]) :- trans(T,T1).


not_live([]).
not_live([0|T]) :- trans([0|T],Y), not_live(Y).

not_thm(X) :- gen(X), not_live(X).

