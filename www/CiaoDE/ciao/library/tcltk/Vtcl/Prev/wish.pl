%%------------------------------------------------------------------------
%% WISH
%%
%% EXAMPLE OF INSTANCIABLE MODULE IN CIAO/Prolog
%%
%%------------------------------------------------------------------------

:- module(wish,
	[
	    wish/0,
	    wish/1,
	    destructor/0,
	    send_code/1,
	    send_code_string/1,
	    receive_term/2,
	    receive_term/1
	]).

:- data code_strm/1.
:- data term_strm/1.

:- use_module(library(sockets)).
:- use_module(library(system)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(strings)).

%%------------------------------------------------------------------------
%% CLASS CONSTRUCTOR / DESTRUCTOR
%%------------------------------------------------------------------------

wish :-
	current_host(Host),
	bind_socket(Port,1,Socket),
	number_codes(Port,PortCode),
	popen('wishx',write,Strm),
	write_string(Strm,"set prolog_host "),
	write(Strm,Host),nl(Strm),flush_output(Strm),
	write_string(Strm,"set prolog_port "),
	write_string(Strm,PortCode),nl(Strm),
	flush_output(Strm),
	set_fact(code_strm(Strm)),
	send_initial_code,
	socket_accept(Socket,TermStream),
	set_fact(term_strm(TermStream)),
	true.

wish(SourceFile) :-
	string(SourceFile,File,""), % Type check
	wish,
	send_code(["source ",File]).



destructor :-
	code_strm(Strm),
	term_strm(TermStrm),
	write_string(Strm,"uplevel 0 exit"),
	nl(Strm),
	flush_output(Strm),
	close(TermStrm),
	close(Strm),
	retractall_fact(code_strm(_)),
	retractall_fact(term_strm(_)).

%%------------------------------------------------------------------------

send_initial_code :-
	core(String),
	send_code_string(String),
	fail.

send_initial_code.

%%------------------------------------------------------------------------
%% BASIC PRED FOR SENDING A SINGLE LINE OF TCLTK CODE TO WISH
%%------------------------------------------------------------------------

send_code(StringList) :-
	code_strm(Strm),
	send_code_list(Strm,StringList).

send_code_list(Strm,[]) :-
	nl(Strm),
	flush_output(Strm).

send_code_list(Strm,[String|Ns]) :-
	write_string(Strm,String),
	send_code_list(Strm,Ns).

send_code_string(Str) :-
	string(Str,String,""),          % Type check
	code_strm(Strm),
	write_string(Strm,String),
	nl(Strm),
	flush_output(Strm).

%%------------------------------------------------------------------------
%% READ A PROLOG TERM FROM TCL/TK
%%------------------------------------------------------------------------

receive_term(Term) :-
	term_strm(Stream),
        read_term(Stream,Term,[]).
	

receive_term(Term,VarNames) :-
	term_strm(Stream),
        read_term(Stream,Term,[variable_names(VarNames)]).
	
	
%%------------------------------------------------------------------------
%% INITIAL CODE
%%------------------------------------------------------------------------

core("wm withdraw .").

core("set term_socket [socket $prolog_host $prolog_port]").

core("proc prolog_term {term} {").
core("global term_socket").
core(" puts  $term_socket $term. ").
core(" flush $term_socket ").
core("} ").
