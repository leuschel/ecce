:- use_module(library('p2p/chord')).
:- use_package(remote).
:- use_module(library(concurrency)).
:- use_module(library(arrays)).


main :-
	joinNet(a(localhost, 16547), a(localhost,18995), Id, C),
%	createNet(netConfig(chord, 200, 10), a(localhost,18995), Id, C),
	display(node(Id, C)), nl,
	eng_call(serve_connection(C), create, create).
