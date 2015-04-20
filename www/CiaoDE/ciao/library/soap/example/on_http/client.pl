
/* Client
   It should be invoked as "client File"
   It will read File (which should contain XML) and send its contents
   in a SOAP message via HTTP to the server identified in conexion/2

   The application protocol is supposed to be asynchronous, so that the
   message is sent and the only answer received back is a confirmation
   of reception. In a synchronous protocol the answer could contain
   a true response.

   In the present case, a true response, if any, would have to come in 
   a different message. To receive such messages the server part should 
   be used. See server.pl
*/

% server identification
conexion('www.clip.dia.fi.upm.es',31000).

% imports required by the processing of the client
:- use_module(library('pillow/http'),[fetch_url/3]).
:- use_module(library('pillow/http_server'),[http_request/4]).
:- use_module(library(soap),[soap_message/2]).

% imports required by the processing of the application itself
:- use_module(library(file_utils),[file_to_string/2]).
:- use_module(library('pillow/html'),[xml2terms/2]).
:- use_module(library(write)).

% main entry point
main([File]):-
	% application processing
	process(File,Message),
	% send and retrieve answer
	conexion(Host,Port),
	http_request(Request,Host,Port,Message),
	fetch_url(Request,[],Response),
	% show answer
	set_prolog_flag(write_strings,on),
	writeq(response(Response)), nl.

% Note that the answer in Response is simply an OK or an error.
% It is used only for confirmation of reception and has no content. 
% It should look like: [status(success,200,[]),content([])] 
% In a response with real content in the form of a SoapTerm, it would 
% instead look like: [status(success,200,[]),content(SoapTerm)] 

% application process
% it converts XML read from File into a SOAP message
process(File,SoapStr):-
	file_to_string(File,String),
	xml2terms(String,XmlTerm),
	soap_message(SoapStr,XmlTerm).
