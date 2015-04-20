
:- module(soap_client,[soap_send/3],[]).

:- use_module(library(read)).
:- use_module(library(soap)).
:- use_module(library('sockets/sockets_io')).

soap_send(Stream,Message,Answer):-
	soap_message(String,Message),
	safe_write(Stream,String),
	read(Stream,Response),
	soap_message(Response,Answer).
