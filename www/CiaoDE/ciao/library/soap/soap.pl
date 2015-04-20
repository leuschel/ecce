
:- module(soap,[soap_message/2],[]).

:- use_module(library('pillow/html'),[xml2terms/2]).
:- use_module(library('pillow/xmlterm'),[canonic_xml_terms_filter/2]).

soap_message(XMLSoap,Request):-
	nonvar(XMLSoap), !,
	soap_message_1(XMLSoap,Request).
soap_message(XMLSoap,Request):-
	nonvar(Request), !,
	soap_message_2(Request,XMLSoap).

soap_message_1(String,Request):-
	% It is XML code
	xml2terms(String,Terms),
	canonic_xml_terms_filter(Terms,[env(Envelope,Attr,Elements)]),
	% It is a SOAP envelope
	member(XmlNS="http://schemas.xmlsoap.org/soap/envelope/",Attr),
	atom_concat('xmlns:',NameSpace,XmlNS),
	atom_concat(NameSpace,':Envelope',Envelope),
	% It has a SOAP body
	parse_envelope(Elements,NameSpace,[Request|_Body]).
	% First body element is the request
	% xmlterm(First,Request).

parse_envelope([env(Name,_Attr,_SubEls)|Els],NameSpace,Body):-
	atom_concat(NameSpace,':Header',Name), !,
	parse_envelope(Els,NameSpace,Body).
parse_envelope([env(Name,_Attr,SubEls)|_Els],NameSpace,Body):-
	atom_concat(NameSpace,':Body',Name), !,
	Body=SubEls.

soap_message_2(Request,String):-
	% xmlterm(Request,Term),
	soap_envelope(Envelope,Request),
	xml2terms(String,Envelope).

soap_envelope(env('SOAP-ENV:Envelope',
	 ['xmlns:SOAP-ENV'="http://schemas.xmlsoap.org/soap/envelope/",
	  'SOAP-ENV:encodingStyle'="http://schemas.xmlsoap.org/soap/encoding/"],         [env('SOAP-ENV:Body',
	  [],
	  [Request])
	 ]),	
	 Request).
