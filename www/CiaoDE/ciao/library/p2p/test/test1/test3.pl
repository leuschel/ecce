:- use_package(remote).
:- include(port1).

main(X) :- 
	port1(Port),
	r(X) @ a(localhost, Port).
