
:- use_package(persdb).
:- use_module(library('pillow/http_server')).
:- use_module(library(sockets)).
:- use_module(library('sockets/sockets_io'),[serve_socket/3]).
:- use_module(library(strings),[write_string/2]).
:- use_module(library(soap),[soap_message/2]).
:- use_module(library(write)).

% Predicados Persistentes
% En estos predicados se asertan los mensajes recibidos y los errores 
% detectados. Cada hecho que se aserta va a un fichero:
% ./user/cached_1.pl y ./user/received_message_2.pl
% Muy util para "espiar" que es lo que esta pasando...
persistent_dir(db,'./ser').

:- persistent(received_message/2,db).
:- persistent(cached/1,db).

% Punto de entrada principal
% Invocando el ejecutable como server NUMERO, arranca el proceso servidor
% escuchando en el port NUMERO. Por cada nueva peticion de un cliente se llama
% al predicado serve/1, por cada error que ocurra se llama a catcher/1.
main(N):-
	%atom_codes(S,Codes),
	%number_codes(N,Codes),
	set_prolog_flag(write_strings,on),
	bind_socket(N,5,Socket),
	serve_socket(Socket,server,catcher).

% Manejador de Peticiones
% Lee una peticion, la procesa, devuelve la respuesta y cierra la conexion.
% Para http esto es suficiente. Para otros protocolos podria escribirse y 
% volver a leer, volver a escribir, etc. sobre el mismo Stream.
server(Stream):-
	http_serve_fetch(Stream,serve(Stream)).

serve(Message,Stream,Answer):-
	!,
	display(Message),nl,
	display(Stream),nl,
	http_answer(good,Answer).

serve(Message,Stream,Answer):-
	writeq(received_message(Stream,Message)), nl,
	assertz_fact(received_message(Stream,Message)),
	process(Message,Answer).

% Manejador de Errores
catcher(Error):-
	!,
	display('Error: '),
	display(Error),nl.

catcher(Error):- 
	assertz_fact(cached(Error)),
	writeq(Error), nl.

% Proceso de la Aplicacion
% Parsea el mensaje HTTP extrayendo el mensaje SOAP, luego parsea este
% obteniendo el contenido: un termino XML.
process(Message,[Answer]):-
	http_message(Message,SoapStr),
	soap_message(SoapStr,XmlTerm), !,
	http_answer(good,Answer),
	writeq(xmlterm(XmlTerm)), nl.
process(_Message,[Answer]):-
	http_answer(bad,Answer).

http_answer(bad,status(request_error,400,"ERROR")).
http_answer(good,status(success,200,"OK")).

http_message(Message,SoapStr):-
	member(post,Message), !,
	member(content(SoapStr),Message).
http_message(Message,SoapStr):-
	member(document(SoapStr),Message).

% Si el protocolo de la aplicacion es sincrono, el mensaje SOAP de respuesta
% deberia ir en Answer. Aqui se usa Answer como confirmacion de recepcion
% correcta; la respuesta SOAP se enviaria usando la parte cliente: 
% ver client.pl
