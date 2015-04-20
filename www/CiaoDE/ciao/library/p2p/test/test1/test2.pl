:- use_package(remote).
:- include(port1).
:- include(port2).

main:- 
	port2(Port),
	get_connection(Port, Connection),
	serve_connection(Connection).

p(a).

% p(X) :- 
% 	port1(Port),
% 	q(X) @ a(localhost, Port).
