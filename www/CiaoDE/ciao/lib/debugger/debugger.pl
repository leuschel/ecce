:- module(debugger, [
% jf: remove these commented lines if everything is ok - 20031122
%	'$debugger_state'/2,'$debugger_mode'/0,'$spypoint'/3,
%	srcdbg_spy/6,
        adjust_debugger/0,switch_off_debugger/0,
        debug_module/1, nodebug_module/1, debug_module_source/1,
        debug/0, nodebug/0, trace/0, notrace/0, 
	spy/1, nospy/1, nospyall/0, 
	breakpt/6,nobreakpt/6,nobreakall/0,list_breakpt/0, 
 	debugging/0, leash/1, maxdepth/1, call_in_module/2,
        current_debugged/1, debugger_setting/2,
        reset_debugger/1, set_debugger/1, get_debugger_state/1,
	adjust_debugger_state/2,
        retry_hook/4,
        debug_trace/1,
        do_interrupt_command/1],
	[dcg,assertions]).

:- use_module(engine('debugger_support')).
:- use_module(library('debugger/debugger_lib'),[debugging_options/1]).
:- use_module(engine(internals)).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1, '$meta_call'/1]).
:- use_module(library(format)).
:- use_module(library(ttyout)).
:- use_module(library(read), [read/2]).
:- use_module(library(system), [cyg2win/3, current_env/2, using_windows/0]).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(user, ['$shell_call'/1]).

:- comment(title, "Predicates controlling the interactive debugger").

:- comment(module, "This library implements predicates which are
   normally used in the interactive top-level shell to debug
   programs. A subset of them are available in the embeddable debugger.").

:- comment(author,"A. Ciepielewski").
:- comment(author,"M. Carlsson").
:- comment(author,"T. Chikayama").
:- comment(author,"K. Shen").
:- comment(author,"D. Cabeza").
:- comment(author,"M. Rodriguez").

:- comment(hide, adjust_debugger/0).
:- comment(hide, switch_off_debugger/0).
:- comment(hide, current_debugged/1).
:- comment(hide, debugger_setting/2).
:- comment(hide, reset_debugger/1).
:- comment(hide, set_debugger/1).
:- comment(hide, get_debugger_state/1).
:- comment(hide, retry_hook/4).
:- comment(hide, debug_trace/1).
:- comment(hide, do_interrupt_command/1).

%------------------ Bug Comments ------------------------------

:- comment(bug,"Add an option to the emacs menu to automatically select
	all modules in a project.").
:- comment(bug,"Consider the possibility to show debugging messages 
	directly in the source code emacs buffer.").

%------------------Prolog debugger by AC------------------------------
% Minor hacks by MC.
% Some hacks by Takashi Chikayama (17 Dec 87)
%   - Making tracer to use "print" rather than "write"
%   - Temporarily switching debugging flag off while writing trace
%     message and within "break" level.
% Some hacks by Kish Shen (May 88)
%   - Subterm navigation
%   - Handle unbound arg in spy/1 and nospy/1
%   - Trap arith errors in debug mode
%------------- Built-in predicates for debugging------------------------

% :- multifile define_flag/3.
% 
% define_flag(debug, [on,debug,trace,off], off).

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off

%%:- initialization(initialize_debugger_state).
%%:- on_abort(initialize_debugger_state).

% This has to be done before any choicepoint
initialize_debugger_state :-
	'$debugger_state'(_, s(off,off,1000000,0,[])),
	'$debugger_mode'.

reset_debugger(State) :-
	'$debugger_state'(State, s(off,off,1000000,0,[])),
	'$debugger_mode'.

set_debugger(State) :-
	'$debugger_state'(_, State),
	'$debugger_mode'.

get_debugger_state(L) :-
	'$debugger_state'(L, L).

switch_off_debugger :-
        '$debugger_state'(State,State),
        '$setarg'(2, State, off, true),
	'$debugger_mode'.


:- data debug_mod/2.

:- true pred debug_module(Module) : atm(Module)
        # "The debugger will take into acount module @var{Module}
          (assuming it is loaded in interpreted mode).  When issuing this
          command at the toplevel shell, the compiler is instructed also
          to set to @em{interpret} the loading mode of files defining that
          module and also to mark it as 'modified' so that (re)loading 
	  this file or a main file that uses this module will force it 
	  to be reloaded for source-level debugging.".

debug_module(M) :- atom(M), !,
        ( current_fact(debug_mod(M,_)) ->
	    true
        ; atom_concat(M, ':', Mc),
          assertz_fact(debug_mod(M, Mc))
        ).
debug_module(M) :-
        format(user_error, '{Bad module ~q - must be an atom}~n', [M]).

:- true pred nodebug_module(Module) : atm(Module)
        # "The debugger will not take into acount module @var{Module}.
          When issuing this command at the toplevel shell, the compiler is
          instructed also to set to @em{compile} the loading mode of files
          defining that module.".

nodebug_module(M) :- % If M is a var, nodebug for all
        retractall_fact(debug_mod(M,_)).
%        what_is_debugged.

%% This entry point is only for documentation purposes.
:- true pred debug_module_source(Module) : atm(Module)
        # "The debugger will take into acount module @var{Module}
          (assuming it is is loaded in source-level debug mode).  When 
	  issuing this command at the toplevel shell, the compiler is 
	  instructed also to set to @em{interpret} the loading mode of 
	  files defining that module and also to mark it as 'modified'
	  so that (re)loading this file or a main file that uses this
	  module will force it to be reloaded for source-level debugging.".

debug_module_source(M):-
	debug_module(M).

current_debugged(Ms) :- findall(M, current_fact(debug_mod(M,_)), Ms).

:- data printdepth/1.

printdepth(10).

:- data debugdepth/1.

debugdepth(100000).

:- true pred maxdepth(MaxDepth) : int
        # "Set maximum invocation depth in debugging to
           @var{MaxDepth}. Calls to compiled predicates are not included
           in the computation of the depth.".

maxdepth(D) :-
        integer(D), !,
        retractall_fact(debugdepth(_)),
	assertz_fact(debugdepth(D)),
	what_maxdepth.
maxdepth(D) :-
        format(user_error, '{Bad maxdepth ~q - must be an integer}~n', [D]).

:- true pred debug/0 # "Switches the debugger on. The interpreter will
        stop at all ports of procedure boxes of spied predicates.".

debug :-
	debugger_setting(_, debug),
	what_is_on(debug).

:- true pred nodebug/0 # "Switches the debugger off.  If there are any
        spy-points set then they will be kept but disabled.".

nodebug :- notrace.

:- true pred trace/0 # "Start tracing, switching the debugger on if
        needed.  The interpreter will stop at all leashed ports of
        procedure boxes of predicates either belonging to debugged
        modules or called from clauses of debugged modules.  A message
        is printed at each stop point, expecting input from the user
        (write @tt{h} to see the available options).".

trace :-
	debugger_setting(_, trace),
	what_is_on(trace).

:- true pred notrace/0 # "Equivalent to @pred{nodebug/0}.".

notrace :-
	debugger_setting(_, off),
	what_is_on(off).

:- true pred spy(PredSpec) : sequence(multpredspec)
        # "Set spy-points on predicates belonging to debugged modules and
          which match @var{PredSpec}, switching the debugger on if
          needed. This predicate is defined as a prefix operator by the
          toplevel.".

spy(Preds) :-
        get_debugger_state(State),
	( arg(1, State, off) -> debug ; true ),
        parse_functor_spec(Preds, X, spy1(X)).

:- true pred nospy(PredSpec) : sequence(multpredspec)
        # "Remove spy-points on predicates belonging to debugged modules
          which match @var{PredSpec}. This predicate is defined as a prefix
          operator by the toplevel.".

nospy(Preds) :-
        parse_functor_spec(Preds, X, nospy1(X)).

spy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	install_spypoint(Pred, N, A).

nospy1(Pred) :-
	functor(Pred, N, A),
	warn_if_udp(Pred, N, A),
	remove_spypoint(Pred, N, A).

:- true pred nospyall/0 # "Remove all spy-points.".

nospyall :-
	spypoint(F),
	'$spypoint'(F, _, off),
	fail.
nospyall :-
	format(user, '{All spypoints removed}~n', []).

:- true pred breakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
	: atm * sourcename * int * int * int * int

	# "Set a @index{breakpoint} in file @var{Src} between lines
          @var{Ln0} and @var{Ln1} at the literal corresponding to the
          @var{Number}'th occurence of (predicate) name @var{Pred}.  The
          pair @var{Ln0}-@var{Ln1} uniquely identifies a program clause and
          must correspond to the
          start and end line numbers for the clause. The rest of the
          arguments provide enough information to be able to locate the
          exact literal that the @var{RealLine} line refers to. This is
          normally not issued by users but rather by the @apl{emacs} mode,
          which automatically computes the different argument after
          selecting a point in the source file.".

:- pred breakpoint(Pred, Src, Ln0, Ln1, Number) # "Breakpoint storage.".

:- data breakpoint/5.  

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),!,
	format(user, 
	    '{There is already a breakpoint on literal ~a in line ~d}~n', 
	    [Pred, RealLine]).

breakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
        get_debugger_state(State),
	( arg(1, State, off) -> debug ; true ),
        assertz_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),
	format(user, '{Breakpoint placed on literal ~a in line ~d}~n', 
	       [Pred, RealLine]).

:- true pred nobreakall/0 # "Remove all breakpoints.".

nobreakall :-
        retractall_fact(breakpoint(_, _, _, _, _)),
	format(user, '{All breakpoints removed}~n', []).

:- true pred nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine)
	: atm * sourcename * int * int * int * int
	# "Remove a breakpoint in file @var{Src} between lines
          @var{Ln0} and @var{Ln1} at the @var{Number}'th occurence of
          (predicate) name @var{Pred} (see @pred{breakpt/6}). Also 
	  normally used from de @apl{emacs} mode.".

nobreakpt(Pred, Src, Ln0, Ln1, Number, RealLine) :-
	retract_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),!,
	format(user, '{Breakpoint removed from literal ~a in line ~d}~n', 
	       [Pred, RealLine]).
nobreakpt(Pred, _, _, _, _, RealLine) :-
	format(user, '{No breakpoint on literal ~a in line ~d}~n',
	       [Pred, RealLine]).

:- true pred list_breakpt/0 # "Prints out the location of all
	breakpoints. The location of the breakpoints is showed usual by
	referring to the source file, the lines between which the predicate
	can be found, the predicate name and the number of ocurrence of the
	predicate name of the literal.".

list_breakpt:-
	current_fact(breakpoint(Pred, Src, Ln0, Ln1, Number)),
	format(user, 'Breakpoint in file ~a ~d-~d on literal ~a-~d~n',
	       [Src, Ln0, Ln1, Pred, Number]),
	fail.
list_breakpt.

debugger_setting(Old, New) :-
	get_debugger_state(State),
	arg(1, State, Old),
	'$setarg'(1, State, New, true),
	adjust_debugger_state(State, New).

:- data leashed/1.

leashed(call).
leashed(exit).
leashed(redo).
leashed(fail).

:- prop port(X) + regtype.

port(call).
port(exit).
port(redo).
port(fail).

:- true pred leash(Ports) : list(port)
        # "Leash on ports @var{Ports}, some of @tt{call}, @tt{exit},
          @tt{redo}, @tt{fail}. By default, all ports are on leash.".

leash(L) :-
	nonvar(L),
	leash1(L),
	!.
leash(L) :-
        format(user_error, '{Bad leash specification ~q}~n', [L]).

leash1(half)  :- !, leash1([call,redo]).
leash1(full)  :- !, leash1([call,exit,redo,fail]).
leash1(loose) :- !, leash1([call]).
leash1(none)  :- !, leash1([]).
leash1(tight) :- !, leash1([call,redo,fail]).
leash1(L) :-
        list(L),
        retractall_fact(leashed(_)), leashlist(L), what_is_leashed.

leashlist([]).
leashlist([Port|L]) :-
        assertz_fact(leashed(Port)),
        leashlist(L).

:- true pred debugging/0 # "Display debugger state.".

debugging :-
	get_debugger_state(State),
	arg(1, State, G),
	what_is_on(G),
        what_is_debugged,
	what_is_leashed,
	what_maxdepth,
	all_spypoints,
	ttynl.
	
%------------------------ meta-interpreters ------------------------------

% called from interpreter.pl

debug_trace(X) :-
	extract_info(X, Goal, Pred, Src, Ln0, Ln1, Number), 
        ( debuggable(Goal) ->
            debug_trace2(Goal, Pred, Src, Ln0, Ln1, Number)
        ; term_to_meta(X, G), '$nodebug_call'(G)
        ).

debuggable(Goal) :-
        in_debug_module(Goal).
debuggable(_) :-
        get_debugger_state(S),
        arg(5, S, [a(_,Ancestor,_)|_]),
        in_debug_module(Ancestor).

in_debug_module(G) :-
        functor(G, F, _),
        current_fact(debug_mod(_,Mc)),
        atom_concat(Mc, _, F).

debug_trace2(X, Pred, Src, Ln0, Ln1, Number) :-
	get_debugger_state(State),
	retry_hook_(X, B, D, NA, OA, Port, State),
	'$setarg'(4, State, B, on),
	'$setarg'(5, State, NA, on),
	(   Port=call,
	    call_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number)
	;   fail_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number), !, fail
	),
	(   exit_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number)
	;   redo_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number), fail
	),
	'$setarg'(5, State, OA, on).



call_hook(X, B, _, State, _, _, _, _, _) :-
	arg(3, State, Level),
	B>Level, !,
	call_hook1(X).
call_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number) :-
	debug_port(X, B, D, call, State, Msg, Pred, Src, Ln0, Ln1, Number),
	call_hook2(Msg, X).

call_hook1(X) :-
	(   '$predicate_property'(X, _, _) ->
            term_to_meta(X, G),
	    '$nodebug_call'(G)
        ;   get_debugger_state(State),
	    adjust_debugger_state(State, trace),
	    functor(X, Name, Ar),
	    format(user_error, '{Warning: The predicate ~q is undefined}~n',
	                       [Name/Ar]),
            fail
        ).

call_hook2(answer(X), X).
call_hook2(no, X) :- call_hook1(X).

exit_hook(_, B, _, State, _, _, _, _, _) :- 
	arg(3, State, Level), B>Level, !.
exit_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, exit, State, _, Pred, Src, Ln0, Ln1, Number).

redo_hook(_, B, _, State, _, _, _, _, _) :- 
	arg(3, State, Level), B>Level, !.
redo_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number) :- 
	debug_port(X, B, D, redo, State, _, Pred, Src, Ln0, Ln1, Number).

fail_hook(_, B, _, State, _, _, _, _, _) :- 
	arg(3, State, Level), B>Level, !.
fail_hook(X, B, D, State, Pred, Src, Ln0, Ln1, Number) :-
	'$setarg'(3, State, 1000000, true),
	debug_port(X, B, D, fail, State, _, Pred, Src, Ln0, Ln1, Number).

retry_hook(_, P, P, _).
retry_hook(Invocation, P0, P, A) :- retry_hook(Invocation, P0, P, A).

retry_hook_(X, B, D, [a(B,X,D)|A], A, Port, State) :-
	State = s(_,_,_,B0,A),
	a_length(A, D0),
	B is B0+1,
	D is D0+1,
	(   current_fact(debugdepth(M)), D=<M -> true
	;   adjust_debugger_state(State, trace),
	    format(user_error,
	           '{Warning: Interpreter maxdepth exceeded}~n', [])
	),
	retry_hook(B, call, Port, '$$retry_hook').

a_length([], 0).
a_length([a(_,_,X)|_], X).

debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :-
	(
	    '$spypoint'(X, on, on)
	;
	    % Ln0 is free because there is no way to determine where the 
	    % clause start, but the end of the clause can be determine exactly.
	    current_fact(breakpoint(Pred, Src, _Ln0, Ln1, Number))
	    
	),!,
	prompt_command(0'p, X, [], B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
debug_port(X, B, D, Port, State, Msg, Pred, Src, Ln0, Ln1, Number) :-
	arg(2, State, trace),
	current_fact(leashed(Port)), !,
	prompt_command(0'p, X, [], B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
debug_port(X, B, D, Port, State, no, Pred, Src, Ln0, Ln1, Number) :-
	arg(2, State, trace), !,
	write_goal(0'p, X, [], B, D, Port, Pred, Src, Ln0, Ln1, Number),
	ttynl.
debug_port(_, _, _, _, _, no, _, _, _, _, _).

prompt_command(T, X, Xs, B, D, Port, State, Msg, 
	       Pred, Src, Ln0, Ln1, Number) :-
	write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Number),
	get_command(C),
	do_trace_command(C, X, Xs, B, D, Port, State, Msg, 
	                 Pred, Src, Ln0, Ln1, Number).

do_trace_command(0'a, _, _, _, _, _, _, _, _, _, _, _, _) :- !, % a(bort)
	abort.
do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _) :- !, % c(reep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'\n , _, _, _, _, _, State, no, _, _, _, _, _) :- 
	!, % CR (creep)
	'$setarg'(2, State, trace, true),
	'$debugger_mode'.
do_trace_command(0'd, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- !, % d(isplay)
	prompt_command(0'd, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'g, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- !, % g(ancestors)
	arg(5, State, CA),
	show_ancestors(CA, -1),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command([0'g,Arg], X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number):- !, % g(ancestors) arg
	arg(5, State, CA),
	show_ancestors(CA, Arg),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'l, _, _, _, _, _, State, no, _, _, _, _, _) :- !, % l(eap)
	'$setarg'(2, State, debug, true),
	'$debugger_mode'.
do_trace_command(0'n, _, _, _, _, _, State, no, _, _, _, _, _) :- 
	!, % n(odebug)
	'$setarg'(3, State, 0, true).
do_trace_command(0'p, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- !, % p(rint)
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'r, _, _, _, _, _, State, no, _, _, _, _, _) :- !, % r(etry)
	arg(5, State, [a(B,_,_)|_]),
	do_retry_fail(B, State, call).
do_trace_command([0'r,B], _, _, _, _, _, State, no, _, _, _, _, _) :- 
	!, % r(etry) arg
	do_retry_fail(B, State, call).
do_trace_command(0'f, _, _, _, _, _, State, no, _, _, _, _, _) :- !, %f(ail)
	arg(5, State, [a(B,_,_)|_]),
	do_retry_fail(B, State, fail).
do_trace_command([0'f,B], _, _, _, _, _, State, no, _, _, _, _, _) :- 
	!, % f(ail) arg
	do_retry_fail(B, State, fail).
do_trace_command(0's, _, _, B, _, Port, State, no, _, _, _, _, _) :- % s(kip)
	set_skip(Port, B, State), !.
do_trace_command(0'w, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, % w(rite)
	prompt_command(0'w, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'+, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, % +(spy this)
	lastof(Xs, _-X, _-Goal),
	spy1(Goal),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'-, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, % -(nospy this)
	lastof(Xs, _-X, _-Goal),
	nospy1(Goal),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'=, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, % =(debugging)
        reset_debugger(_),
	debugging,
        set_debugger(State),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
% do_trace_command(0'b, X, Xs, B, D, Port, State, Msg) :- !, % b(reak)
% 	break,
% 	prompt_command(0'p, X, Xs, B, D, Port, State, Msg).
do_trace_command(0'@, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %@ (command)
	do_once_command('| ?- '),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'u, _, _, _, _, call, _, answer(X1), _, _, _, _, _) :- 
	!, %u (unify)
	'$prompt'(Old, '|: '),
	read(user, X1),
	'$prompt'(_, Old).
do_trace_command(0'<, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %< (reset printdepth)
	retractall_fact(printdepth(_)),
        assertz_fact(printdepth(10)),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command([0'<,I], X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %< arg (set printdepth)
	retractall_fact(printdepth(_)),
        assertz_fact(printdepth(I)),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'^, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %^ (reset subterm)
	lastof(Xs, _-X, _-Goal),
	prompt_command(0'p, Goal, [], B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command([0'^,0], _, [_-X|Xs], B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, %^ 0 (up subterm)
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command([0'^,I], X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- %^ arg (set subterm)
	arg(I, X, Ith), !,
	prompt_command(0'p, Ith, [I-X|Xs], B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'?, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, % ?(help)
	debugging_options(interpreted),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(0'h, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- 
        !, % h(elp)
	debugging_options(interpreted),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).
do_trace_command(_, X, Xs, B, D, Port, State, Msg, 
	         Pred, Src, Ln0, Ln1, Number) :- % all others
	format(user, '{Option not applicable at this port}~n', []),
	prompt_command(0'p, X, Xs, B, D, Port, State, Msg, 
	               Pred, Src, Ln0, Ln1, Number).

lastof([], X, X).
lastof([X0|Xs], _, X) :- lastof(Xs, X0, X).

do_retry_fail(B, State, Port) :-
	'$retry_cut'(B, Port),
	'$setarg'(2, State, trace, true),	% retry implies creep!
	fail.

do_once_command(Prompt) :-
	'$prompt'(OldPrompt, Prompt),
	reset_debugger(State),
	read(user, Command),
	'$prompt'(_, '|: '),
	( '$shell_call'(Command) -> Y=yes
        ; Y=no
        ),
	'$prompt'(_, OldPrompt),
	set_debugger(State),
	Y=yes, !.
do_once_command(_) :-
        format(user_error, '{Warning: goal failed}~n', []).

%--------------------------options---------------------------------------

% debugging_options :-
% 	ttydisplay('Debugging options:'), ttynl,
% 	ttydisplay('   <cr>   creep            c      creep'), ttynl,
% 	ttydisplay('    l     leap             s      skip'), ttynl,
% 	ttydisplay('    r     retry            r <i>  retry i'), ttynl,
% 	ttydisplay('    f     fail             f <i>  fail i'), ttynl,
% 	ttydisplay('    d     display          p      print'), ttynl,
% 	ttydisplay('    w     write'), ttynl,
% 	ttydisplay('    g     ancestors        g <n>  ancestors n'), ttynl,
% 	ttydisplay('    n     nodebug          =      debugging'), ttynl,
% 	ttydisplay('    +     spy this         -      nospy this'), ttynl,
% 	ttydisplay('    a     abort'), ttynl,
% 	ttydisplay('    @     command          u      unify'), ttynl,
% 	ttydisplay('    <     reset printdepth < <n>  set printdepth'), ttynl,
% 	ttydisplay('    ^     reset subterm    ^ <n>  set subterm'), ttynl,
% 	ttydisplay('    ?     help             h      help'), ttynl,
% 	ttynl.

%-------------------------facilities-------------------------------------

% extract_info('debugger:srcdbg_spy'(Goal,Pred,Src,Ln0,Ln1,Number),
extract_info('debugger_support:srcdbg_spy'(Goal,Pred,Src,Ln0,Ln1,Number),
	       NewGoal,Pred,Src,Ln0,Ln1,Number):-
	!,
	term_to_meta(NewGoal,Goal).
extract_info(Goal,Goal,nil,nil,nil,nil,nil). 

adjust_debugger :-
        get_debugger_state(State),
	arg(1, State, G),
	adjust_debugger_state(State, G).

adjust_debugger_state(State, New) :-
	'$setarg'(2, State, New, true),
	'$setarg'(3, State, 1000000, true),
	'$debugger_mode'.

all_spypoints :-
	spypoint(_), !,
	ttydisplay('Spypoints:'), list_spypoints.
all_spypoints :-
	ttydisplay('{There are no spypoints}'), ttynl.

list_spypoints :-
	spypoint(X),
	functor(X, N, A),
	ttynl, tab(user, 4), write(user, N/A),
	fail.
list_spypoints :-
	ttynl.

warn_if_udp(F, _, _) :- '$predicate_property'(F, _, _), !.
warn_if_udp(_, N, A) :-
	format(user_error, '{Warning: No definition for ~q}~n', [N/A]).

install_spypoint(F, N, A) :-
	'$spypoint'(F, on, on), !,
	format(user, '{There is already a spypoint on ~q}~n', [N/A]).
install_spypoint(F, N, A) :-
	'$spypoint'(F, off, on), !,
	format(user, '{Spypoint placed on ~q}~n', [N/A]).
install_spypoint(_, N, A) :-
	format(user, '{Cannot spy built-in predicate ~q}~n', [N/A]).

remove_spypoint(F, N, A) :-
	'$spypoint'(F, off, off), !,
	format(user, '{There is no spypoint on ~q}~n', [N/A]).
remove_spypoint(F, N, A) :-
	'$spypoint'(F, on, off), !,
	format(user, '{Spypoint removed from ~q}~n', [N/A]).
remove_spypoint(_, N, A) :-
	ttynl,
	format(user, '{Cannot spy built-in predicate ~q}~n', [N/A]).

spypoint(X) :-
	'$current_predicate'(_, X),
	'$spypoint'(X, on, on).

write_goal(T, X, Xs, B, D, Port, Pred, Src, Ln0, Ln1, Number) :-
        reset_debugger(State),
	port_info(Port, Pport),
        current_output(CO),
        set_output(user),
	print_srcdbg_info(Pred, Src, Ln0, Ln1, Number),
	spy_info(Xs, X, Mark0, S, []),
	(
	    Mark0 == '   ' -> break_info(Pred, Src, Ln0, Ln1, Number, Mark)
	;
	    Mark=Mark0
	),
        display_list([Mark, B, '  ', D, Pport | S]),
	write_goal2(T, X),
        set_output(CO),
        set_debugger(State).

print_srcdbg_info(_, _, nil, nil, nil):- !.
print_srcdbg_info(Pred, Src, Ln0, Ln1, Number):-
	(
	    using_windows ->
            % Emacs understand slashes instead of backslashes, even on
            % Windows, and this saves problems with escaping
            % backslashes
	    atom_codes(Src, SrcCodes),
	    cyg2win(SrcCodes, ActualSrcCodes, noswap),
	    atom_codes(ActualSrc, ActualSrcCodes)	    
	;
	    Src = ActualSrc
        ),
	display_list(['         In ', ActualSrc,' (',Ln0,-,Ln1,') ',
	             Pred,-,Number,'\n']).

spy_info([], Goal, ' + ') --> {'$spypoint'(Goal, on, on)}, !.
spy_info([], _, '   ') --> [].
spy_info([I-X|Xs], _, Goal) --> 
	spy_info(Xs, X, Goal),
	[^, I].

break_info(Pred, Src, _Ln0, Ln1, Number, ' B '):-
	current_fact(breakpoint(Pred, Src, _, Ln1, Number)), 
	!.
break_info(_Pred, _Src, _Ln0, _Ln1, _Number, '   ').

port_info(block, '  Block: ').
port_info(unblock, '  Unblock: ').
port_info(call, '  Call: ').
port_info(exit, '  Exit: ').
port_info(redo, '  Redo: ').
port_info(fail, '  Fail: ').
port_info(void, '  ').

write_goal2(0'd, Goal) :- 
	display(Goal).	% display
write_goal2(0'p, Goal) :-			% print
	current_fact(printdepth(D)),
        get_attributed_vars(Goal, AtVars),
        (
            AtVars = [_|_] ->
            sort(AtVars, SortedAtVars),
            write_term(Goal, [max_depth(D)]), % Skip user's portray_attribute
            print_attributes(SortedAtVars, D)
        ;
            write_term(Goal, [portrayed(true),max_depth(D)])
        ).
write_goal2(0'w, Goal) :- 
	writeq(Goal).	% write

get_attributed_vars(Term, AtVars) :-
        get_attributed_vars_(Term, [], _, [], AtVars).

get_attributed_vars_(X, Seen, Seen, At, NewAt) :-
        var(X), !,
        ( get_attribute(X, AtX) ->
            NewAt = [attach_attribute(X,AtX)|At]
        ;
            NewAt = At
        ).
get_attributed_vars_(X, Seen, Seen, At, At) :-
        atomic(X), !.
get_attributed_vars_(X, Seen, Seen, At, At):-
        already_seen(Seen, X), !.
get_attributed_vars_(X, Seen, NewSeen, At, NewAt):-
        functor(X, _, Ar),
        get_attributed_vars_args(Ar, X, [X|Seen], NewSeen, At, NewAt).

get_attributed_vars_args(0, _, Seen, Seen, At, At):- !.
get_attributed_vars_args(N, X, Seen0, Seen2, At0, At2):- 
        N > 0,
        arg(N, X, A),
        get_attributed_vars_(A, Seen0, Seen1, At0, At1),
        N1 is N - 1,
        get_attributed_vars_args(N1, X, Seen1, Seen2, At1, At2).

already_seen([T|_Ts], Term):-
        T == Term,
        !.
already_seen([_T|Ts], Term):- already_seen(Ts, Term).

print_attributes([], _D).
print_attributes([A|As], D):-
        nl,
        tab(10),    % 10 blanks
        display('['),
        write_term(A, [max_depth(D)]),
        display(']'),
        print_attributes(As, D).

get_command(Command) :-
	ttydisplay(' ? '),
	ttyflush,
	ttyget(C1),
	get_rest_command(C1, Command).

get_rest_command(0'\n , 0'\n) :- !.
get_rest_command(C1, Command) :-
	ttyget(C2),
	get_arg(C2, C1, Command).

get_arg(0'\n , C, C):- !.
get_arg(C2, C1, [C1,Arg]) :-
	C2 >= 0'0, C2 =< 0'9, !,
	trd_digits(C2, 0, Arg).
get_arg(_, C1, C):-
	ttyget(C2),
	get_arg(C2, C1, C).

trd_digits(Ch, SoFar, I) :-
	Ch >= 0'0, Ch =< 0'9, !,
	Next is SoFar*10 + Ch - 0'0,
	ttyget(Ch1),
	trd_digits(Ch1, Next, I).
trd_digits(0'\n , I, I) :- !.
trd_digits(_, I, J) :-
	ttyget(Ch),
	trd_digits(Ch, I, J).

show_ancestors([_], _) :- !,
	ttynl, ttydisplay('No ancestors.'), ttynl.
show_ancestors([_|CA], N) :-
	ttynl, ttydisplay('Ancestors:'), ttynl,
	list_ancestors(CA, N).

list_ancestors([], _) :- !.
list_ancestors(_, 0) :- !.
list_ancestors([a(B,X,D)|As], N0) :-
	N is N0-1,
	list_ancestors(As, N),
	write_goal(0'p, X, [], B, D, void, _Pred, _Src, nil, nil, nil),
	ttynl.

mode_message(debug,
'{The debugger will first leap -- showing spypoints and breakpoints (debug)}').
mode_message(trace,
	      '{The debugger will first creep -- showing everything (trace)}').
mode_message(off,
	      '{The debugger is switched off}').

what_is_on(Mode) :-
	mode_message(Mode, Msg),
	ttydisplay(Msg),
	ttynl.

what_is_debugged :-
        current_debugged(Ms),
        ( Ms = [] ->
          format(user, '{No module is selected for debugging}~n',[])
        ; format(user, '{Modules selected for debugging: ~w}~n',[Ms])
        ).

what_is_leashed :-
	is_leashed([call,exit,redo,fail], L),
	show_leash_info(L).

is_leashed([], []).
is_leashed([X|Xs], [X|Ys]) :- current_fact(leashed(X)), !, is_leashed(Xs, Ys).
is_leashed([_|Xs], Ys) :- is_leashed(Xs, Ys).

show_leash_info([]) :- !,
	format(user, '{No leashing}~n', []).
show_leash_info(Ps) :-
	format(user, '{Using leashing stopping at ~w ports}~n', [Ps]).

what_maxdepth :-
	current_fact(debugdepth(M)),
	format(user, '{Interpreter maxdepth is ~w}~n', [M]).


set_skip(call, To, State) :- '$setarg'(3, State, To, true).
set_skip(redo, To, State) :- '$setarg'(3, State, To, true).
set_skip(_, _To, State) :- 
	format(user, '{Skip not applicable at this port, creeping ...}~n', []),
	do_trace_command(0'c, _, _, _, _, _, State, no, _, _, _, _, _).

:- meta_predicate parse_functor_spec(?,?,goal).

parse_functor_spec(V, _, _) :-
        var(V), !,
        format(user_error, '{A variable is a bad predicate indicator}~n', []).
parse_functor_spec((S,Ss), GoalArg, Goal) :-
        parse_functor_spec(S, GoalArg, Goal),
        parse_functor_spec(Ss, GoalArg, Goal).
parse_functor_spec(S, GoalArg, Goal) :-
	Flag=f(0),
	(   functor_spec(S, Name, Low, High, M),
            current_fact(debug_mod(M,Mc)),
            atom_concat(Mc, Name, PredName),
	    '$current_predicate'(PredName, GoalArg),
	    functor(GoalArg, _, N),
	    N >= Low, N =< High,
	    '$setarg'(1, Flag, 1, true),
	    '$nodebug_call'(Goal),
	    fail
	;   Flag=f(0),
	    format(user_error,
               "{Bad predicate indicator or predicate undefined in modules currently debugged:~n ~w}~n", [S]),
            fail
	;   true
	).

:- comment(doinclude, multpredspec/1).

:- true prop multpredspec/1.

multpredspec(Mod:Spec) :- atm(Mod), multpredspec(Spec).
multpredspec(Name/Low-High) :- atm(Name), int(Low), int(High).
multpredspec(Name/(Low-High)) :- atm(Name), int(Low), int(High).
multpredspec(Name/Arity) :- atm(Name), int(Arity).
multpredspec(Name) :- atm(Name).

functor_spec(Mod:Spec, Name, Low, High, Mod) :-
        functor_spec(Spec, Name, Low, High, _).
functor_spec(Name/Low-High, Name, Low, High, _) :-
	atom(Name),
	integer(Low), integer(High), !.
functor_spec(Name/(Low-High), Name, Low, High, _) :-
	atom(Name),
	integer(Low), integer(High), !.
functor_spec(Name/Arity, Name, Arity, Arity, _) :-
	atom(Name),
	integer(Arity), !.
functor_spec(Name, Name, 0, 255, _) :-		% 255 is max. arity
	atom(Name).


do_interrupt_command(0'@) :- !,			% @ (command)
	ttyskipeol, do_once_command('| ?- '),
        do_interrupt_command(0'\n).
do_interrupt_command(0'a) :- !,			% a(bort)
	ttyskipeol, abort.
% do_interrupt_command(0'b) :- !,			% b(reak)
% 	ttyskipeol, break.
do_interrupt_command(0'c) :- !,			% c(ontinue)
	ttyskipeol.
do_interrupt_command(0'd) :- !,			% d(ebug)
	ttyskipeol, debug.
do_interrupt_command(0'e) :- !,			% e(xit)
	ttyskipeol, halt.
do_interrupt_command(0't) :- !,			% t(race)
	ttyskipeol, trace.
do_interrupt_command(0'\n) :- !,		% cr
	format(user, '~nCiao interruption (h for help)? ', []),
	ttyflush,
	ttyget(C),
	do_interrupt_command(C).
do_interrupt_command(_) :-			% h(elp) or other
	ttyskipeol,
	interrupt_options,
	do_interrupt_command(0'\n).

interrupt_options :-
	ttynl,
	ttydisplay('Ciao interrupt options:'), ttynl,
	ttydisplay('    a        abort           - cause abort'), ttynl,
%	ttydisplay('    b        break           - cause break'), ttynl,
	ttydisplay('    c        continue        - do nothing'), ttynl,
	ttydisplay('    d        debug           - start debugging'), ttynl,
	ttydisplay('    t        trace           - start tracing'), ttynl,
	ttydisplay('    e        exit            - cause exit'), ttynl,
	ttydisplay('    @        command         - execute a command'), ttynl,
	ttydisplay('    h        help            - get this list'), ttynl.

% :- meta_predicate call_in_module(?, fact).

:- true pred call_in_module(Module, Predicate) : atm * callable
        # "Calls predicate @var{Predicate} belonging to module
          @var{Module}, even if that module does not export the
          predicate. This only works for modules which are in debug 
	  (interpreted) mode (i.e., they are not optimized).".

call_in_module(Module, Goal) :-
        module_concat(Module, Goal, MGoal),
        '$meta_call'(MGoal).
