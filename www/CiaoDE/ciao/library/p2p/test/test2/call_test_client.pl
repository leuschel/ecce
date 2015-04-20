:- use_package(remote).
:- include(port).

main(X) :- 
	port(Port),
	r(Port, X).

r(Port, X):-
	p(q(X)) @ a(localhost, Port).
