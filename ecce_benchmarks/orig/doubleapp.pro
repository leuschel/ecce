double_app(X,Y,Z,Res) :- 
	append(X,Y,I),append(I,Z,Res).

append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).
