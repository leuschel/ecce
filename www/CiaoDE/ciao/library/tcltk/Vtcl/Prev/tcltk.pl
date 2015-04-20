%%------------------------------------------------------------------------
%%
%% TCL/TK INTERFACE LIBRARY FOR CIAO/Prolog
%%
%% This is the same interface as defined in SICStus tcltk library...
%% 
%% ANGEL FERNANDEZ PINEDA
%%
%% at development...
%% Not fully implemented.
%%
%%------------------------------------------------------------------------

:- module(tcltk,
	[
	    tcl_new/1,
	    tcl_delete/1,
	    tcl_eval/3,
	    tcl_event/3,
	    goal_server/1,
	    goal_server_loop/1
	]).

%%------------------------------------------------------------------------

:- use_module(library('sockets/sockets')).
:- use_module(library(system),[popen/3,current_host/1]).
:- use_module(library(format),[format/3]).
:- use_module(library(lists),[append/3,member/2]).
:- use_module(library(strings),[write_string/2]).
:- use_module(library(write),[write/2]).
:- use_module(library(atom2term),[string2term/2]).
:- use_module(engine(internals),[module_concat/3,term_to_meta/2]).
:- use_module(library(concurrency)).

%%------------------------------------------------------------------------
%%
%% TCL interpreter creation
%%
%%------------------------------------------------------------------------

tcl_new(TCLID) :-
	var(TCLID),
	bind_socket(RsPort,1,RsSocket),
	current_host(Host),
	number_codes(RsPort,RsPortCode),
	popen(wishx,write,CmdStream),
%	write_string(CmdStream,"load /usr/lib/libtclx.so "),nl(CmdStream),
%	flush_output(CmdStream),
	write_string(CmdStream,"set prolog_host "),
	write(CmdStream,Host),nl(CmdStream),flush_output(CmdStream),
	write_string(CmdStream,"set prolog_port "),
	write_string(CmdStream,RsPortCode),nl(CmdStream),
	flush_output(CmdStream),
	bind_socket(EvPort,1,EvSocket),
	number_codes(EvPort,EvPortCode),
	write_string(CmdStream,"set event_port "),
	write_string(CmdStream,EvPortCode),nl(CmdStream),
	flush_output(CmdStream),
	bind_socket(GlPort,1,GlSocket),
	number_codes(GlPort,GlPortCode),
	write_string(CmdStream,"set goal_port "),
	write_string(CmdStream,GlPortCode),nl(CmdStream),
	flush_output(CmdStream),
	send_initial_code(CmdStream),
	socket_accept(RsSocket,RsStream),
	socket_accept(EvSocket,EvStream),
	socket_accept(GlSocket,GlStream),
	eng_call(goal_server_loop(GlStream),create,create),
	TCLID = tcl_interpreter(cmd(CmdStream),result(RsStream),
	            event(EvStream),goal(GlStream)).

send_initial_code(Stream) :-
	core(Str),
	write_string(Stream,Str),nl(Stream),
	flush_output(Stream),
	fail.

send_initial_code(_).

%%------------------------------------------------------------------------
%%
%% TCL interpreter destruction
%%
%%------------------------------------------------------------------------

tcl_delete(TCLID) :-
	nonvar(TCLID),
	TCLID = tcl_interpreter(cmd(CmdStream),result(RsStream),
	            event(EvStream),goal(GlStream)),
	nl(CmdStream),
	write_string(CmdStream,"uplevel #0 exit"),
	nl(CmdStream),
	flush_output(CmdStream),
	close(CmdStream),
	close(EvStream),
	close(GlStream),
	close(RsStream).

%%------------------------------------------------------------------------
%%
%% Send commands to TCL
%%
%%------------------------------------------------------------------------

tcl_eval(TCLID,[Cmd|Next],Result) :-
	!,
	nonvar(TCLID),
	TCLID = tcl_interpreter(cmd(CmdStream),result(RsStream),
	            event(_),goal(_)),
	write_string(CmdStream,"prolog_cmd {"),
	nl(CmdStream),
	tcl_eval_list(CmdStream,[Cmd|Next]),
	write_string(CmdStream,"}"),
	nl(CmdStream),
	flush_output(CmdStream),
	get_result(RsStream,Result).

tcl_eval(TCLID,Cmd,Result) :-
	Cmd \== [],
	tcl_eval(TCLID,[Cmd],Result).

%%------------------------------------------------------------------------

tcl_eval_list(_,[]) :- !,true.

tcl_eval_list(CmdStream,[write(Term)|Next]) :-
	write(CmdStream,Term),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[format(Fmt,Args)|Next]) :-
	format(CmdStream,Fmt,Args),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[nl|Next]) :-
	nl(CmdStream),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[sqb(CommandList)|Next]) :-
	char_code('[',Code1),
	char_code(']',Code2),
	write_string(CmdStream,[Code1]),
	tcl_eval_list(CmdStream,CommandList),
	write_string(CmdStream,[Code2]),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[dq(CommandList)|Next]) :-
	char_code('"',Code),
	write_string(CmdStream,[Code]),
	tcl_eval_list(CmdStream,CommandList),
	write_string(CmdStream,[Code]),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[br(CommandList)|Next]) :-
	char_code('{',Code1),
	char_code('}',Code2),
	write_string(CmdStream,[Code1]),
	tcl_eval_list(CmdStream,CommandList),
	write_string(CmdStream,[Code2]),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[Cmd|Next]) :-
	atom(Cmd),
	Cmd \== [],
	!,
	atom_codes(Cmd,Str),
	write_string(CmdStream,Str),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[Cmd|Next]) :-
	number(Cmd),
	!,
	number_codes(Cmd,Str),
	write_string(CmdStream,Str),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,[chars(Cmd)|Next]) :-
	!,
	write_string(CmdStream,Cmd),
	write_string(CmdStream," "),
	tcl_eval_list(CmdStream,Next).

tcl_eval_list(CmdStream,Cmd) :-
	!,
	tcl_eval_list(CmdStream,[Cmd]).

%%------------------------------------------------------------------------
%%
%% Tcl Event reading
%%
%%------------------------------------------------------------------------

tcl_event(TCLID,[],TermList) :-
	!,
	nonvar(TCLID),
	TCLID = tcl_interpreter(cmd(_),result(_),
	            event(EvStream),goal(_)),
	get_event_terms(EvStream,TermList).

tcl_event(TCLID,[Cmd|Next],TermList) :-
	!,
	nonvar(TCLID),
	TCLID = tcl_interpreter(cmd(_),result(_),
	            event(EvStream),goal(_)),
	nl(CmdStream),
	tcl_eval_list(CmdStream,[Cmd|Next]),
	nl(CmdStream),
	flush_output(CmdStream),
	get_event_terms(EvStream,TermList).

tcl_event(TCLID,Cmd,TermList) :-
	tcl_event(TCLID,[Cmd],TermList).

%%------------------------------------------------------------------------

get_event_terms(EvStream,TermList) :-
	socket_recv(EvStream,Data),
	get_event_aux(Data,TermList,[]).

get_event_aux([],[],[]).

get_event_aux([13|NextData],[Term|Next],TermString) :-
	NextData = [10|NextData2],
	!,
	string2term(TermString,Term),
	get_event_aux(NextData2,Next,[]).

get_event_aux([Code|NextData],TermList,TermString) :-
	append(TermString,[Code],NewTermString),
	get_event_aux(NextData,TermList,NewTermString).

%%------------------------------------------------------------------------
%%
%% For internal use...
%%
%%------------------------------------------------------------------------

get_result(Stream,RdStr):-
        read_term(Stream,ResultTerm,[]),
	( ResultTerm = tcl_result -> 
	  get_normal_result(Stream,RdStr) ;
	  (  
	      ResultTerm = tcl_exception(Code),
	      throw(tcl_exception(Code)),
	      fail
	  )
	).

get_normal_result(Stream,RdStr) :-
	socket_recv(Stream,RdStrAux),
	( append(RdStr,[13,10],RdStrAux) -> 
	  (!,true)
	;
	  RdStr = RdStrAux
	).

%%------------------------------------------------------------------------
%%
%% GOAL EXECUTION SERVER
%%
%% This Pred is executed as a thread...
%%------------------------------------------------------------------------

goal_server(Stream) :-
	catch(
	  read_term(Stream,prolog_goal(Goal),[variable_names(VarNames)]),
	  _,
	  fail),
	!,
	term_to_meta(Goal,MetaGoal),
	execute_goal(MetaGoal,VarNames,Stream),
	true.

%%------------------------------------------------------------------------

execute_goal(Goal,VarNames,Stream) :-
	inform_user(['GOAL ',Goal]),nl,
	( catch(Goal,Exception,tclgoal_exception(Exception,Stream)) -> 
	    tclgoal_ok(VarNames,Stream) 
	        ;
	    tclgoal_fails(Stream) 
	),
	inform_user(['GOAL SERVED']),nl.

%%------------------------------------------------------------------------

tclgoal_exception(What,Stream) :-
	inform_user(['EXCEPT ',What]),nl,
	write(Stream,3),  % 3 = return with exception code
	nl(Stream),
	flush_output(Stream),
	write_string(Stream,"{"),
	write(Stream,What),
	write_string(Stream,"}"),
	nl(Stream),
	flush_output(Stream),
	fail.

tclgoal_fails(Stream) :-
	inform_user(['FAIL ']),nl,
	write(Stream,0),  % 0 = return with failure
	nl(Stream),
	flush_output(Stream).

tclgoal_ok(VarNames,Stream) :-
	inform_user(['OK  ',VarNames]),nl,
	write(Stream,1),  % 1 = return with success
	nl(Stream),
	flush_output(Stream),
	send_variables(VarNames,Stream).

send_variables([],Stream) :-
	write(Stream,no_more_prolog_vars),
	nl(Stream),
	flush_output(Stream).

send_variables([=(VarName,VarValue)|Next],Stream) :-
	write(Stream,VarName),
	nl(Stream),
	flush_output(Stream),
	write_string(Stream,"\""),
	write(Stream,VarValue),
	write_string(Stream,"\""),
	nl(Stream),
	flush_output(Stream),
	send_variables(Next,Stream).

%%------------------------------------------------------------------------

tclgoal_to_metacall(:(Module,Goal),MetaCall) :-
	!,
	module_concat(Module,Goal,Qualified),
	term_to_meta(Qualified,MetaCall).

tclgoal_to_metacall(Goal,MetaCall) :-
	!,
	module_concat(user,Goal,Qualified),
	term_to_meta(Qualified,MetaCall).

%%------------------------------------------------------------------------

goal_server_loop(GlStream) :-
	goal_server(GlStream),
	!,
	goal_server_loop(GlStream).

goal_server_loop(_).

%%------------------------------------------------------------------------
%% WISH INITIALIZATION CODE
%%------------------------------------------------------------------------

% Connect to prolog socket in order to send results.
% Tcl global variables such as "prolog_host" were
% previously set by tcl_new/1...

core("set prolog_socket [socket $prolog_host $prolog_port]").
core("set event_socket [socket $prolog_host $event_port]").
core("set goal_socket [socket $prolog_host $goal_port]").

% Declare "prolog_variables" array

core("set prolog_variables(_) none").

% Execute command and send results back to prolog
% This is internally used by tcl_eval/3.

core("proc prolog_cmd {command} {").
core("global prolog_socket").
core("set ErrCode [catch {set result [uplevel #0 $command]}]").
core("if {$ErrCode == 0} {").
core("   puts  $prolog_socket \"tcl_result.\" ").
core("   flush $prolog_socket").
core("   puts  $prolog_socket $result").
core("   flush $prolog_socket").
core("   return $result").
core("} else {").
core("   puts  $prolog_socket \"tcl_exception($ErrCode).\"").
core("   flush $prolog_socket").
core("   error $ErrCode").
core("} ").
core("} ").

% Tcl procedure as defined in SICStus user's manual.

core("proc prolog_event {Terms} {").
core("global event_socket").
core("set nterms [llength $Terms]").
core("set x 0").
core("while {$x<$nterms} {").
core("puts  $event_socket [lindex $Terms $x]").
core("flush $event_socket").
core("incr x").
core("} ").
core("} ").

% Tcl auxiliary procedure in order to set "prolog_variables"

core("proc internal_get_prolog_vars {} {").
core(" global goal_socket").
core(" global prolog_variables").
core("   unset prolog_variables").
core("   while {1} {").
core("      set var_name [gets $goal_socket]").
core("      if {$var_name == \"no_more_prolog_vars\"} {").
core("         break").
core("      }").
core("      set var_value [gets $goal_socket]").
core("      set prolog_variables($var_name) $var_value").
core("   }").
core("   set prolog_variables(_) none").
core("}").

% Tcl procedure as defined in SICStus user's manual.

core("proc prolog {Goal} {").
core("global goal_socket").
core("global prolog_variables").
core("puts $goal_socket \"prolog_goal($Goal).\" ").
core("flush $goal_socket").
core("set result [gets $goal_socket]").
core("if {$result == 3} {").
core("   set err_msg [gets $goal_socket]").
core("   set result  [gets $goal_socket]").
core("   error \"Prolog Exception: $err_msg\" ").
core("}").
core("if {$result == 1} {").
core("   internal_get_prolog_vars").
core("}").
core("return $result").
core("}").

core(" ").
