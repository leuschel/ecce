
:- module(soap_server,[serve_socket/3],[assertions,hiord]).

:- use_module(library(read)).
:- use_module(library(soap)).
:- use_module(library('sockets/sockets_io')).

:- meta_predicate serve_socket(?,pred(1),pred(1)).

serve_socket(Socket,Server,Catcher):-
	sockets_io:serve_socket(Socket,serve(Server,Catcher),Catcher).

serve(Stream,Server,Catcher):-
	read(Stream,String),
	serve_(String,Server,Catcher,Stream,Answer),
	soap_message(Return,Answer),
	safe_write(Stream,Return).

serve_(String,Server,_Catcher,_Stream,Answer):-
	soap_message(String,Request), !,
	Answer = ok,
	Server(Request).
serve_(String,_Server,Catcher,Stream,not_a_soap_message):-
	Catcher(not_a_soap_message(Stream,String)).
