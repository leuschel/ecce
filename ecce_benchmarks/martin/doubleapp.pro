doubleapp:double_app(X,Y,Z,Res) :- 
	doubleapp:append(X,Y,I),doubleapp:append(I,Z,Res).

doubleapp:append([],L,L).
doubleapp:append([H|X],Y,[H|Z]) :- doubleapp:append(X,Y,Z).