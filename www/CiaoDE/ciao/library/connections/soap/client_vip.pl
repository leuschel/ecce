
:- use_module(library(file_utils),[file_to_string/2]).
:- use_module(library('pillow/html'),[xml2terms/2]).
:- use_module(library('pillow/http'),[fetch_url/3]).
:- use_module(library('pillow/http_server'),[http_request/4]).
:- use_module(library(soap),[soap_message/2]).
:- use_module(library(write)).
:- use_module(library(strings)).

% Identificacion del servidor
conexion('www.clip.dia.fi.upm.es',31202).

% Punto de entrada principal
% Invocando el ejecutable como client FICHERO, se conecta al servidor
% especificado y manda un mensaje SOAP bajo HTTP con el XML de FICHERO.
main([File]):-
	process(File,Message),
	conexion(Host,Port),
	http_request(Request,Host,Port,Message),
	fetch_url(Request,[],Response),
	set_prolog_flag(write_strings,on),
	writeq(response(Response)), nl,
	member(content(X),Response),
	soap_message(X,XmlTerm),
	xml2terms(String,XmlTerm),
	open('output.txt',write,Stream),
	write_string(Stream,String),
	close(Stream).

% Proceso de la Aplicacion
% Convierte el XML en un mensaje SOAP.
process(File,SoapStr):-
	file_to_string(File,String),
	xml2terms(String,XmlTerm),
	soap_message(SoapStr,XmlTerm).

% Si el protocolo de la aplicacion es sincrono, el mensaje SOAP de respuesta
% vendria en la Response como content(SoapTerm). Aqui Response se usa solo
% para confirmacion, por lo que deberia ser de este tipo: 
% [status(success,200,[]),content([])] indicando OK y ningun contenido. 
% La respuesta SOAP vendria en otro mensaje y para recibirlo se deberia usar
% la parte servidor: ver server.pl
