
:- use_module(library(soap)).
:- use_module(library(file_utils),[file_to_string/2]).
:- use_module(library('pillow/html'),[xml2terms/2]).
:- use_module(library('sockets/sockets_io'),[safe_write/2]).

main([File]):-
	% open both sides on the same connection
	% (this process is thus connected to itself)
	server(ConnId,ServerId),
	client(ConnId,ClientId),
	catch(send_ok(File,ClientId),_,send_wrong(File,ClientId)),
	% receive
	current_fact(soap_receive(_Sender,OtherTerms)),
	set_prolog_flag(write_strings,on),
	write(OtherTerms), nl,
	% close both sides
	soap:close(ServerId),
	soap:close(ClientId).

send_ok(File,ClientId):-
	file_to_string(File,String),
	xml2terms(String,Terms),
	assertz_fact(soap_send(ClientId,Terms)).

send_wrong(Atom,Stream):-
	atom_codes(Atom,String),
	safe_write(Stream,String).
