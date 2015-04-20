
/* Server
   Upon start-up it binds a socket and listens for messages on it.
   For each soap message received server/1 is called with its contents.
   For any error, included if a message not in soap format is received,
   catcher/1 is called.
*/

% imports required to be a server
:- use_module(soap_server).
:- use_module(library(sockets)).

main:-
	bind_socket(Port,5,Socket),
	display('Server connected at port: '), display(Port), nl,
	serve_socket(Socket,server,catcher).

server(Request):-
	display('SOAP message:'), nl,
	show(Request).

catcher(not_a_soap_message(_Stream,String)):- !,
	display('Not a SOAP message:'), nl,
	show(String).
catcher(Error):- 
	displayq(Error), nl.

show(Message):-
	prolog_flag(write_strings,X,on),
	writeq(Message), nl,
	prolog_flag(write_strings,_,X).
