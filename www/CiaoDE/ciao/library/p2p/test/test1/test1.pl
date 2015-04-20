:- use_package(remote).
:- include(port1).
:- include(port2).

main :- 
	port1(Port),
	set_connection(Port, Connection),
	serve_connection(Connection).

r(X):-
	port2(Port),
	p(X) @ a(localhost, Port).

q(a).
