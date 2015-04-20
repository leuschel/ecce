:- module(_,[netscape_message/1,netscape_append_message/1],[]).

:- use_module(library('pillow/html'),[html2terms/2]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(system),[mktemp/2,shell/1]).
:- use_module(library(streams),[open_output/2,close_output/1]).
:- use_module(library(strings),[write_string/1]).
:- use_module(library(aggregates),[findall/3]).

:- data message_buffer/1.

netscape_message(HtmlTerms) :-
	retractall_fact(message_buffer(_)),
	netscape_append_message(HtmlTerms).

netscape_append_message(HtmlTerms) :-
	assertz_fact(message_buffer(HtmlTerms)),
	findall(Terms,message_buffer(Terms),AllTerms),
	netscape_show_message(AllTerms).

netscape_show_message(HtmlTerms) :-
	temporary_file(TmpFile),
	open_output(TmpFile,O),
	html2terms(Html,HtmlTerms),
	write_string(Html),
	close_output(O),
	nsc_visit_file(TmpFile).

:- data temp_file/1.

temporary_file(TmpFile) :-
	temp_file(TmpFile),
	!.
temporary_file(TmpFile) :-
	mktemp('/tmp/nscoutXXXXXX',TmpFile),
	asserta_fact(temp_file(TmpFile)).

nsc_visit_file(File) :-
	atom_concat(['netscape -remote openURL\\(file:',File,'\\)'],
	            NSCommand),
	shell(NSCommand).
