:- module(srcbyrd_rt, [srcdbg_byrd/6, 
	               trace/0, debug/0, notrace/0, nodebug/0
		      ],
	              [dcg, assertions]).

:- use_module(engine(internals),['$predicate_property'/3,
	term_to_meta/2,'$setarg'/4]).
:- use_module(engine(debugger_support), ['$spypoint'/3,'$debugger_state'/2,
	'$debugger_mode'/0]).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1]).
:- use_module(library(format),[format/3]).
:- use_module(library(write),[writeq/1, write_term/2]).
:- use_module(library(ttyout),[ttydisplay/1,ttyflush/0,ttyget/1]).
:- use_module(library('debugger/debugger_lib')).
:- use_module(library(ttyout)).
:- use_module(library(sort)).

:- redefining(trace/0).
:- redefining(debug/0).
:- redefining(notrace/0).
:- redefining(nodebug/0).

% initialize_srcdebugger :-
% 	format(user_error,'Punto 1~n',[]),
% 	set_prolog_flag(embedded_debugger,off),
% 	format(user_error,'Punto 2~n',[]),
% 	what_is_on(off).

% Predicates to interactuate with the debugger
trace:-
 	debugger_lib:trace.
debug:-
 	debugger_lib:debug.
notrace:-
	debugger_lib:notrace.
nodebug:- 
 	debugger_lib:nodebug.

srcdbg_byrd(X, Pred, Src, L0, L1, Number):-
	get_debugger_state(State),
	arg(2, State, Debugging),
	( 
	    debuggable(X, Debugging, Pred, Src, L0, L1, Number) ->
	           srcdbg_byrd2(X, State, Pred, Src, L0, L1, Number)
	;
	    '$nodebug_call'(X)
	).

debuggable(_, trace, _, _, _, _, _):- !.
debuggable(X, debug, Pred, Src, _, Ln1, Number):-
	(
	    term_to_meta(G, X),
	    '$spypoint'(G, on, on)
	;
	    % Ln0 is free because there is no way to determine where the 
	    % clause start, but the end of the clause can be determine exactly.
	    current_fact(breakpoint(Pred, Src, _Ln0, Ln1, Number))
	    
	), !.
debuggable(_, _, _, _, _, _, _):- !, fail.

% The embedded debugger
:- meta_predicate srcdbg_byrd(goal, _, _, _, _, _).

srcdbg_byrd2(G, State, Pred, Src, L0, L1, Number):-
	term_to_meta(Goal, G),
	( 
	    call_hook(Goal, State, Pred, Src, L0, L1, Number)
	;
	    fail_hook(Goal, State, Pred, Src, L0, L1, Number), !, fail
	),
	( 
	    exit_hook(Goal, State, Pred, Src, L0, L1, Number)
	;
	    redo_hook(Goal, State, Pred, Src, L0, L1, Number), fail
	).

call_hook(Goal, State, Pred, Src, L0, L1, Number):-
	debug_port(Goal, call, State, Msg, Pred, Src, L0, L1, Number),
 	call_hook2(Msg, Goal).

call_hook1(Goal):-
	(
	    '$predicate_property'(Goal, _, _) ->
	        term_to_meta(Goal, G),
		'$nodebug_call'(G)
	;
	    functor(Goal, Name, Arity),
	    format(user_error, '{Warning: The predicate ~q is undefined}~n',
	                       [Name/Arity]),
	    fail
	).

call_hook2(answer(Goal), Goal).
call_hook2(no, Goal) :- call_hook1(Goal).

exit_hook(Goal, State, Pred, Src, L0, L1, Number):-
	debug_port(Goal, exit, State, _, Pred, Src, L0, L1, Number).

redo_hook(Goal, State, Pred, Src, L0, L1, Number):-
 	debug_port(Goal, redo, State, _, Pred, Src, L0, L1, Number).

fail_hook(Goal, State, Pred, Src, L0, L1, Number):-
 	debug_port(Goal, fail, State, _, Pred, Src, L0, L1, Number).

debug_port(Goal, Port, State, Msg, Pred, Src, L0, L1, Number):-
	( '$spypoint'(Goal, on, on)
	;
 	    % Ln0 is free because there is no way to determine where the 
	    % clause start, but the end of the clause can be determine exactly.
	    current_fact(breakpoint(Pred, Src, _Ln0, L1, Number))
	),!,
	prompt_command(0'p, Goal, [], Port, State, Msg, 
	               Pred, Src, L0, L1, Number).

debug_port(Goal, Port, State, Msg, Pred, Src, L0, L1, Number):-
 	arg(2, State, trace),
	current_fact(leashed(Port)), !,
	prompt_command(0'p, Goal, [], Port, State, Msg, 
	               Pred, Src, L0, L1, Number).

debug_port(Goal, Port, State, no, Pred, Src, L0, L1, Number):-
 	arg(2, State, trace), !,
	write_goal(0'p, Goal, [], Port, Pred, Src, L0, L1, Number),
	ttynl.

debug_port(_, _, _, no, _, _, _, _, _).

prompt_command(T, Goal, Xs, Port, State, Msg, Pred, Src, L0, L1, Number):-
	write_goal(T, Goal, Xs, Port, Pred, Src, L0, L1, Number),
	get_command(C),
	do_trace_command(C, Goal, Xs, Port, State, Msg, 
                        Pred, Src, L0, L1, Number).

write_goal(T, Goal, Xs, Port, Pred, Src, L0, L1, Number):-
	port_info(Port, Pport),
	current_output(CO),
	set_output(user),
	display_list(['         In ', Src, ' (', L0, -, L1,') ',
		     Pred,-, Number, '\n']),
	spy_info(Xs, Goal, Mark, S, []),
	display_list([Mark, '*', '  ', '*', Pport | S]),
	write_goal2(T, Goal),
	set_output(CO).

spy_info([], Goal, ' + ') --> {'$spypoint'(Goal, on, on)}, !.
spy_info([], _, '   ') --> [].
spy_info([I-X|Xs], _, Goal) --> 
	spy_info(Xs, X, Goal),
	[^, I].

write_goal2(0'd, Goal):- % display
	display(Goal).
write_goal2(0'p, Goal):- % print
	current_fact(printdepth(D)),
	get_attributed_vars(Goal, AtVars, []),
	(
	    AtVars = [_|_] ->
	    sort(AtVars, SortedAtVars),
	    write_term(Goal, [max_depth(D)]), % Skip user's portray_attribute
	    print_attributes(SortedAtVars, D)
	;
	   write_term(Goal, [portrayed(true), max_depth(D)])
	).
write_goal2(0'w, Goal):- % write
	writeq(Goal).

do_trace_command(0'a, _, _, _, _, _, _, _, _, _, _) :- !, % a(bort)
	abort.
do_trace_command(0'c, _, _, _, State, no, _, _, _, _, _) :- !, % c(reep)
 	'$setarg'(2, State, trace, true),
 	'$debugger_mode'.
do_trace_command(0'\n , _, _, _, State, no, _, _, _, _, _) :- !, % c(reep)
 	'$setarg'(2, State, trace, true),
 	'$debugger_mode'.
do_trace_command(0'd, X, Xs, Port, State, Msg, 
 	         Pred, Src, Ln0, Ln1, Number) :- !, % d(isplay)
 	prompt_command(0'd, X, Xs, Port, State, Msg, 
 	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'l, _, _, _, State, no, _, _, _, _, _) :- !, % l(eap)
 	'$setarg'(2, State, debug, true),
 	'$debugger_mode'.
do_trace_command(0'n, _, _, _, _State, no, _, _, _, _, _) :- 
  	!, % n(odebug)
	nodebug.  	%'$setarg'(3, State, 0, true).
do_trace_command(0'p, X, Xs, Port, State, Msg, 
 	         Pred, Src, Ln0, Ln1, Number) :- !, % p(rint)
 	prompt_command(0'p, X, Xs, Port, State, Msg, 
 	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'w, X, Xs, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :- 
        !, % w(rite)
	prompt_command(0'w, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'+, X, Xs, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :- 
        !, % +(spy this)
	lastof(Xs, _-X, _-Goal),
	spy1(Goal),
	prompt_command(0'p, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'-, X, Xs, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :- 
        !, % -(nospy this)
	lastof(Xs, _-X, _-Goal),
	nospy1(Goal),
	prompt_command(0'p, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'<, X, Xs, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :- 
        !, %< (reset printdepth)
	retractall_fact(printdepth(_)),
        assertz_fact(printdepth(10)),
	prompt_command(0'p, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command([0'<,I], X, Xs, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %< arg (set printdepth)
	retractall_fact(printdepth(_)),
        assertz_fact(printdepth(I)),
	prompt_command(0'p, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'^, X, Xs, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %^ (reset subterm)
	lastof(Xs, _-X, _-Goal),
	prompt_command(0'p, Goal, [], Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command([0'^,0], _, [_-X|Xs], Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %^ 0 (up subterm)
	prompt_command(0'p, X, Xs, Port, State, Msg,
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command([0'^,I], X, Xs, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- %^ arg (set subterm)
	arg(I, X, Ith), !,
	prompt_command(0'p, Ith, [I-X|Xs], Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'?, X, Xs, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :- 
        !, % ?(help)
	debugging_options(embedded),
	prompt_command(0'p, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'h, X, Xs, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :- 
        !, % h(elp)
	debugging_options(embedded),
	prompt_command(0'p, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(_, X, Xs, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :- 
	% all others
	format(user, '{Option not applicable at this port}~n', []),
	prompt_command(0'p, X, Xs, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).

lastof([], X, X).
lastof([X0|Xs], _, X) :- lastof(Xs, X0, X).


%%%%%	ttydisplay('s      skip'), ttynl,
%%%%%   ttydisplay('    r     retry            r <i>  retry i'), ttynl,
%%%%%	ttydisplay('    f     fail             f <i>  fail i'), ttynl,
%%%%%   ttydisplay('=      debugging'), ttynl,
%%%%%	ttydisplay('    @     command          u      unify'), ttynl,
%%%%%	ttydisplay('    ^     reset subterm    ^ <n>  set subterm'), ttynl,
