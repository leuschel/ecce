%%------------------------------------------------------------------------
%% TCL/TK low level library
%%------------------------------------------------------------------------

:- module(bltclass,[],[assertions,regtypes,isomodes]).

:- comment(author,"Isabel Mart@'{i}n Garc@'{i}a").


:- export(new_interp/1).
%%:- export(new_interp/2).
%%:- export(tcltk/2).
:- export(tcltk_raw_code/2).
%%:- export(copy_stdin/1).
%%:- export(receive_term/3).
%%:- export(receive_term/2).
%%:- export(send_term/2). 

%%:- export(delete/1).
%%:- export(receive_event/2).
%%:- export(receive_list/2).
%%:- export(tcl_error/1).

:- export(bltwish_interp/1).
:- export(interp_file/2).

:- use_module(library(sockets)).
:- use_module(library(system)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(strings)).
:- use_module(library(format),[format/3]).
:- use_module(library(terms)).

:- comment(title,"Low level Interface between Prolog and blt").
:- comment(module,"This module exports some predicates to interact with
	Tcl-tk, particularly with the bltwish program. Bltwish is a
	windowing shell consisting of the Tcl command languaje, the Tk
	toolkit plus the additional commands that comes with the BLT
	library and a main program that reads commands. It creates a main
	window and then processes Tcl commands.

").  
%%------------------------------------------------------------------------
%% CONSTRUCTOR / DESTRUCTOR
%%------------------------------------------------------------------------

%:- export(new_interp_file/2).

%new_interp_file(_,X) :-
%	nonvar(X),
%	!,
%	fail.

%new_interp_file(File,'$wish$'(Strm,TermStream,EventStream)) :-
%	current_host(Host),
%	bind_socket(Port,1,Socket),
%	number_codes(Port,PortCode),
%	atom_concat(['bltwish <',File],Command),
%	popen(Command,write,Strm),
%	write_string(Strm,"set prolog_host "),
%	write(Strm,Host),nl(Strm),flush_output(Strm),
%	write_string(Strm,"set prolog_port "),
%	write_string(Strm,PortCode),nl(Strm),
%	flush_output(Strm),
%	bind_socket(EPort,1,ESocket),
%	number_codes(EPort,EPortCode),
%	write_string(Strm,"set event_port "),
%	write_string(Strm,EPortCode),nl(Strm),
%	flush_output(Strm),
%%	send_initial_code(Strm),
%%	socket_accept(Socket,TermStream),
%%	socket_accept(ESocket,EventStream),
%	true.





:- pred new_interp(-bltwish_interp).

:- comment(new_interp(Interp),"Creates a bltwish interprter and returns a
	socket. The socket allows the comunication between Prolog and
	Tcl-tk. Thus, bltwish receives the commands through the socket.

").

:- push_prolog_flag(multi_arity_warnings,off).

new_interp(X) :-
	nonvar(X),
	!,
	fail.

new_interp('$wish$'(Strm,TermStream,EventStream)) :-
	current_host(Host),
	bind_socket(Port,1,Socket),
	number_codes(Port,PortCode),
	popen('bltwish',write,Strm),   %%BLTWISH
	write_string(Strm,"set prolog_host "),
	write(Strm,Host),nl(Strm),flush_output(Strm),
	write_string(Strm,"set prolog_port "),
	write_string(Strm,PortCode),nl(Strm),
	flush_output(Strm),
	bind_socket(EPort,1,ESocket),
	number_codes(EPort,EPortCode),
	write_string(Strm,"set event_port "),
	write_string(Strm,EPortCode),nl(Strm),
	flush_output(Strm),
	send_initial_code(Strm),
	socket_accept(Socket,TermStream),
	socket_accept(ESocket,EventStream),
	true.

new_interp('$wish$'(Strm,TermStream,EventStream),Options) :-
	current_host(Host),
	bind_socket(Port,1,Socket),
	number_codes(Port,PortCode),
	atom_concat(bltwish,Options,V),
	popen(V,write,Strm),
	write_string(Strm,"set prolog_host "),
	write(Strm,Host),nl(Strm),flush_output(Strm),
	write_string(Strm,"set prolog_port "),
	write_string(Strm,PortCode),nl(Strm),
	flush_output(Strm),
	bind_socket(EPort,1,ESocket),
	number_codes(EPort,EPortCode),
	write_string(Strm,"set event_port "),
	write_string(Strm,EPortCode),nl(Strm),
	flush_output(Strm),
	send_initial_code(Strm),
	socket_accept(Socket,TermStream),
	socket_accept(ESocket,EventStream),
	true.

delete('$wish$'(Strm,_TermStrm,_EventStrm)) :-
	write_string(Strm,"uplevel 0 exit"),
	nl(Strm),
	flush_output(Strm),
	close(Strm).

%%------------------------------------------------------------------------

send_initial_code(Strm) :-
	core(String),
	write_string(Strm,String),
	nl(Strm),
	flush_output(Strm),
	fail.

send_initial_code(_).

%%------------------------------------------------------------------------
%% SEND BASIC TCLTK CODE ITEMS TO WISH
%%------------------------------------------------------------------------

:- pred interp_file(+sourcename,+bltwish_interp).

:- comment(interp_file(File,Interp),"Sends the script file (File) to the
	interpreter through the socket. A script file is a file that
	contains commands that Tcl-tk can execute.

").

interp_file(File,Interp) :-
	open(File,read,Strm),
	interp_file_aux(Strm,Interp),
	close(Strm).

interp_file_aux(Strm,Interp) :-
	get_line_aux(Strm,Line),
	tcltk_raw_code(Line,Interp),
	fail.
interp_file_aux(_,_). 
	
get_line_aux(Strm,Line) :-
	bltclass:get_line(Strm,Line),
%	inform_user([Line]),
	( Line = end_of_file -> (!,fail) ; true).

get_line_aux(Strm,Line) :-
	get_line_aux(Strm,Line).

%%------------------------------------------------------------------------
%% SEND BASIC TCLTK CODE ITEMS TO WISH
%%------------------------------------------------------------------------


:- pred tcltk_raw_code(+string,+bltwish_interp).

:- comment(tcltk_raw_code(Command_Line,Interp),"Sends a command line to the 
	interpreter. Tcl-tk parses and executes it.

").

tcltk_raw_code(Str,'$wish$'(Strm,_,_)) :-
	string(Str,String,""),
	!,
	write_string(Strm,String),
	nl(Strm),
	flush_output(Strm).

copy_stdin('$wish$'(Strm,_,_)) :-
	!,
	copy_stdin_aux(Strm).

copy_stdin_aux(Strm) :-
	get_code(Byte),
	Byte =\= -1,
	!,
	put_code(Strm,Byte),
	flush_output(Strm),
	copy_stdin_aux(Strm).

copy_stdin_aux(_).

%%------------------------------------------------------------------------
%% MACRO IN ORDER TO SEND TCL/TK CODE TO WISH
%%------------------------------------------------------------------------

tcltk(Code,'$wish$'(Strm,_,_)) :-
	!,
	nl(Strm),
	send_code(Code,Strm).

%%------------------------------------------------------------------------

send_code([],Strm) :-
	!,
	nl(Strm),
	flush_output(Strm).

send_code([Number|Nc],Strm) :-
	number(Number),
	!,
	number_codes(Number,NumberAsCode),
	write_string(Strm,NumberAsCode),
	write(Strm,' '),
	send_code(Nc,Strm).
	
send_code([chars(String)|Nc],Strm) :-
	!,
	send_code([String|Nc],Strm).

send_code([dq(Code)|Nc],Strm) :-
	write(Strm,'\"'),
	send_code(Code,Strm),
	write(Strm,'\" '),
	!,
	send_code(Nc,Strm).

send_code([sqb(Code)|Nc],Strm) :-
	write(Strm,'['),
	send_code(Code,Strm),
	write(Strm,'] '),
	!,
	send_code(Nc,Strm).

send_code([br(Code)|Nc],Strm) :-
	write(Strm,'{'),
	send_code(Code,Strm),
	write(Strm,'} '),
	!,
	send_code(Nc,Strm).

send_code([min(Code)|Nc],Strm) :-
	atom(Code),
	!,
	write(Strm,'-'),
	write(Strm,Code),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([format(Fmt,Args)|Nc],Strm) :-
	format(Strm,Fmt,Args),
	!,
	send_code(Nc,Strm).

send_code([write(Term)|Nc],Strm) :-
	write(Strm,Term),
	!,
	send_code(Nc,Strm).

send_code([tcl(Var)|Nc],Strm) :-
	number_codes(Var,Str),
	atom_codes(Atom,Str),
	write(Strm,Atom),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([Atom|Nc],Strm) :-
	atom(Atom),
	!,
	write(Strm,Atom),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([Str|Nc],Strm) :-
	string(Str,String,""),
	!,
	write_string(Strm,String),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([_|Nc],Strm) :-
	!,
	send_code(Nc,Strm).

send_code(Not_a_list,Strm) :-
	!,
	send_code([Not_a_list],Strm).

%%------------------------------------------------------------------------
%% SEND A PROLOG TERM TO TCL/TK
%%------------------------------------------------------------------------

send_term(Term,'$wish$'(_,Stream,_)) :-
        write_term(Stream,Term,[]),
	nl(Stream),flush_output(Stream).
	

%%------------------------------------------------------------------------
%% READ A PROLOG TERM FROM TCL/TK
%%------------------------------------------------------------------------

receive_term(Term,'$wish$'(_,Stream,_)) :-
        read_term(Stream,Term,[]),
	exceptions(Term).
	
receive_term(Term,VarNames,'$wish$'(_,Stream,_)) :-
        read_term(Stream,Term,[variable_names(VarNames)]).

%%------------------------------------------------------------------------
%% EXCEPTIONS FROM TCL/TK
%%------------------------------------------------------------------------

exceptions(Term) :- 
	catch(Term,Error,handle(Error)).

tcl_error(Text) :-
	write('Tcl exception: '),
	write_string(Text),
	nl,
	halt.

tcl_result(_).

handle(X) :-
	write(X),
	nl,
	halt.

%%________________________________________________________________________
%% READ A PROLOG LIST OF TERMS FROM TCLTK
%%________________________________________________________________________

receive_event([Term],'$wish$'(_,_,Stream)) :-
	read_term(Stream,Term,[]).


%%------------------------------------------------------------------------



receive_list([],'$wish$'(_,_,Stream)) :-
	!,
	read_term(Stream,end_of_event_list,[]).
	
receive_list([Term|Nt],'$wish$'(_,_,Stream)) :-
	read_term(Stream,Term,[]),
	!,
	receive_list(Nt,'$wish$'(_,_,Stream)).

%%------------------------------------------------------------------------
%% EVALUATE IF AN ERROR OCCURS IN THE TCLTK SCRIPT
%%------------------------------------------------------------------------




%%------------------------------------------------------------------------
%% INITIAL CODE
%%------------------------------------------------------------------------

%core("wm withdraw .").

core("set term_socket [socket $prolog_host $prolog_port]").
core("set event_socket [socket $prolog_host $event_port]").
%core("[fconfigure event_socket -blocking true]").

core("proc prolog_term {term} {").
core("global event_socket").
core(" puts  $event_socket $term. ").
core(" flush $event_socket ").
core("} ").

core("proc prolog {agoal} {").
%core(" prolog_term goal($agoal) ").
core(" prolog_term execute($agoal) ").
core("} ").

% Execute command and sendresults back to prolog
% This is internally used by tcl_eval/3.
% return 0 when the scripts runs without errors, and 1 if there is an error

%core("proc prolog_cmd {command} {").
%core(" global term_socket").
%core("   set result [catch {uplevel $command} var]").
%core("   if {$result} {").
%core("       set var [convert_variable $var ]").
%core("       puts  $term_socket tcltk_low_level:tcl_error(\\""$var\\"").").
%core("       flush $term_socket").
%core("       return $result").
%core("   } else { ").
%core("       puts  $term_socket tcltk_low_level:tcl_result(\\""$result\\"").").
%core("       flush $term_socket").
%core("       return $result").
%core("   } ").
%core("} ").

%% Execute command and send results back to prolog
%% this is internally used by tcl_event/3

%core("proc prolog_one_event {a_term} {").
%core(" global event_socket").
%core(" global term_socket").
%core("   set result 0").
%core("   set result_var 0 ").
%core("   puts  $event_socket $a_term.").
%core("   flush $event_socket").
%core("   gets  $term_socket result").
%core("   set ret [unify_term $a_term $result]").
%%core("   gets  $event_socket $result_var").
%%core("   puts $result_var").
%core("} ").



get_line(Stream, Line) :-
        current_input(OldIn),
        set_input(Stream),
        bltclass:get_line(Line),
        set_input(OldIn).

get_line(Line) :-
        get_code(C),
        get_line_after(C, Cs),
        Line = Cs.

get_line_after(-1,end_of_file) :- !. % EOF
get_line_after(10,[]) :- !. % Newline
get_line_after(13, R) :- !, % Return, delete if at end of line
        get_code(C),
        get_line_after(C, Cs),
        ( Cs = [] ->
              R = []
        ; R = [13|Cs]
        ).
get_line_after(C, [C|Cs]) :-
        get_code(C1),
        get_line_after(C1, Cs).

:- pop_prolog_flag(multi_arity_warnings).
  
:- regtype bltwish_interp/1.

bltwish_interp(Interp):-
	stream(Interp).

:- comment(bltwish_interp(Interp), "This type defines a bltwish
   interpreter.  In fact, the bltwish interpreter receives the
   commands through the socket created. @includedef{bltwish_interp/1}").
