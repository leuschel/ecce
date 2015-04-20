:- use_package(remote).
:- include(port).

main:- 
	port(Port),
	get_connection(Port, Connection),
	serve_connection(Connection).

p(X) :- 
	call(X).
q(a).
