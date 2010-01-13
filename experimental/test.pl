/* test.pl */

app([],X,X).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).

len([],0).
len([H|T],L) :- len(T,LT), L is LT+1.
