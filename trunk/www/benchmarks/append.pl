
append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).

