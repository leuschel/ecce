:- module(_,[main/1]).
:- use_module(library('pillow/http_post')).
:- use_module(library(system),[current_host/1]).
:- use_module(library(format),[format/2]).

main([]):-
	display('usage: fetch Host Port Doc'),nl.

main([Host,Port,Doc]):-
	atom_codes(Port,Codes),
	number_codes(N,Codes),
	atom_codes(Doc,String),
%	current_host(Host),
%	fetch_url(http(Host,N,String),[timeout(50)],Response),
	fetch_url(http(Host,N,String),[post,content("hola"),content_length('4')],Response),
	print_list(Response).

print_list([]).
print_list([X|Xs]):-
	format("~w~n",[X]),
	print_list(Xs).
