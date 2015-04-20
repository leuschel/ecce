:- use_module(library('p2p/chord')).
:- use_package(remote).
:- use_module(library(concurrency)).


main :-
	main1.
%	main2.

main1 :-
	createNet(netConfig(chord, 200, 10), a(localhost,16547), Id, C),
	display(node(Id, C)), nl,
%	serve_connection(C).
	eng_call(serve_connection(C), create, create).

p(a).
