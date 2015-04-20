
:- use_module(library(lists),[append/3]).
:- use_module(library(sockets)).
:- use_module(library('sockets/sockets_io'),[serve_socket/3]).
:- use_module(library(strings),[write_string/2]).
:- use_module(library(write)).

main([S]):-
	atom_codes(S,Codes),
	number_codes(N,Codes),
	set_prolog_flag(write_strings,on),
	bind_socket(N,5,Socket),
	serve_socket(Socket,serve,catcher).

serve(Stream):-
	socket_recv(Stream,Str),
%	socket_recv(Stream,Str2),
%	atom_codes(Atm,Str),
	Atm=Str,
%	append(Str1,Str2,Atm),
	writeq(received_message(Stream,Atm)), nl,
	answer(Answer),
        write_string(Stream,Answer),
        flush_output(Stream),
	close(Stream).

answer("HTTP/1.0 200 OK"||[13,10,13,10]).

catcher(Error):- 
	writeq(user,Error), nl(user).
