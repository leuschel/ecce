app([],L,L).
app([H|X],Y,[H|Z]) :- when((nonvar(X);nonvar(Z)), app(X,Y,Z)).


app2([],L,L).
app2([H|X],Y,[H|Z]) :- app2(X,Y,Z).