
:- module(client,_,[]).

:- use_module(library('pillow/http')).
:- use_module(library('pillow/http_server'),[http_request/4]).

main([Host,S,D]):-
	atom_codes(S,Codes),
	number_codes(Port,Codes),
	atom_codes(D,Doc),
	http_request(Request,Host,Port,Doc),
	fetch_url(Request,[],Response),
	display(received(Response)), nl.
