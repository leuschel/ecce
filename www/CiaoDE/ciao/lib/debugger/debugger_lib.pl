:- module(debugger_lib,[get_debugger_state/1,
	                debug_module/1, nodebug_module/1, 
			debug_module_source/1,
			debug/0, nodebug/0, trace/0, notrace/0,
	                breakpoint/5,
			nobreakpt/6, nobreakall/0, list_breakpt/0,
			spy/1, nospy/1, nospyall/0, spy1/1, nospy1/1,
			leashed/1,
			get_command/1,
			port_info/2,
			what_is_on/1, 
			printdepth/1,
			get_attributed_vars/3,
			print_attributes/2,
			debugging_options/1,
			functor_spec/5
		       ],
		       [assertions]).

:- use_module(library(ttyout)).
:- use_module(engine(internals)).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1]).
:- use_module(engine(debugger_support), ['$debugger_state'/2,'$debugger_mode'/0,'$spypoint'/3]).
:- use_module(library(format)).
:- use_module(library(write)).
:- use_module(library(sort)).

:- comment(hide, get_debugger_state/1).
:- comment(hide, what_is_on/1).
:- comment(hide, debugging_options/1).
:- comment(hide, spy1/1).
:- comment(hide, nospy1/1).
:- comment(hide, functor_spec/5).

% :- multifile define_flag/3.
% 
% define_flag(debug, [on,debug,trace,off], off).

% Debugger_state = s(DebugFlag, OptDebugFlag, int, int, AncestorList)
% DebugFlag = trace|debug|off
% OptDebugFlag = trace|debug|off


reset_debugger(State) :-
	'$debugger_state'(State, s(off,off,1000000,0,[])),
	'$debugger_mode'.
get_debugger_state(L) :-
	'$debugger_state'(L, L).

debugger_setting(Old, New) :-
	get_debugger_state(State),
	arg(1, State, Old),
	'$setarg'(1, State, New, true),
	adjust_debugger_state(State, New).

adjust_debugger :-
        get_debugger_state(State),
	arg(1, State, G),
	adjust_debugger_state(State, G).

adjust_debugger_state(State, New) :-
	'$setarg'(2, State, New, true),
	'$setarg'(3, State, 1000000, true),
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

:- pred breakpoint(Pred, Src, Ln0, Ln1, Number) # "Breakpoint storage.".

:- data breakpoint/5.  

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

:- true pred nobreakall/0 # "Remove all breakpoints.".

nobreakall :-
        retractall_fact(breakpoint(_, _, _, _, _)),
	format(user, '{All breakpoints removed}~n', []).

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
	    '$current_predicate'(PredName, GoalArg), % don't work
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

:- data leashed/1.

leashed(call).
leashed(exit).
leashed(redo).
leashed(fail).

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

port_info(block, '  Block: ').
port_info(unblock, '  Unblock: ').
port_info(call, '  Call: ').
port_info(exit, '  Exit: ').
port_info(redo, '  Redo: ').
port_info(fail, '  Fail: ').
port_info(void, '  ').

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

:- data printdepth/1.

printdepth(10).


get_attributed_vars(X, At, At):- atomic(X), !.
get_attributed_vars(X, [attach_attribute(X,AtX)|At], At):- 
        var(X), 
        get_attribute(X, AtX), !.
get_attributed_vars(X, At, At):- var(X), !.     % No attributes
get_attributed_vars([X|Xs], At0, At2):- !,
        get_attributed_vars(X, At0, At1),
        get_attributed_vars(Xs, At1, At2).
get_attributed_vars(X, At0, At1):-
        functor(X, _, Ar),
        get_attributed_vars_args(Ar, X, At0, At1).

get_attributed_vars_args(0, _, At, At):- !.
get_attributed_vars_args(N, X, At0, At2):- 
        N > 0,
        arg(N, X, A),
        get_attributed_vars(A, At0, At1),
        N1 is N - 1,
        get_attributed_vars_args(N1, X, At1, At2).

print_attributes([], _D).
print_attributes([A|As], D):-
        nl,
        tab(10),    % 10 blanks
        display('['),
        write_term(A, [max_depth(D)]),
        display(']'),
        print_attributes(As, D).


% Command options

debugging_options(embedded) :-
	ttydisplay('Debugging options:'), ttynl,
	ttydisplay('   <cr>   creep            c      creep'), ttynl,
	ttydisplay('    l     leap'), ttynl,
%	ttydisplay('    l     leap             s      skip'), ttynl,
%	ttydisplay('    r     retry            r <i>  retry i'), ttynl,
%	ttydisplay('    f     fail             f <i>  fail i'), ttynl,
	ttydisplay('    d     display          p      print'), ttynl,
	ttydisplay('    w     write'), ttynl,
%	ttydisplay('    g     ancestors        g <n>  ancestors n'), ttynl,
%	ttydisplay('    n     nodebug          =      debugging'), ttynl,
	ttydisplay('    +     spy this         -      nospy this'), ttynl,
	ttydisplay('    a     abort'), ttynl,
%	ttydisplay('    @     command          u      unify'), ttynl,
	ttydisplay('    <     reset printdepth < <n>  set printdepth'), ttynl,
	ttydisplay('    ^     reset subterm    ^ <n>  set subterm'), ttynl,
	ttydisplay('    ?     help             h      help'), ttynl,
	ttynl.

debugging_options(interpreted) :-
	ttydisplay('Debugging options:'), ttynl,
	ttydisplay('   <cr>   creep            c      creep'), ttynl,
	ttydisplay('    l     leap             s      skip'), ttynl,
	ttydisplay('    r     retry            r <i>  retry i'), ttynl,
	ttydisplay('    f     fail             f <i>  fail i'), ttynl,
	ttydisplay('    d     display          p      print'), ttynl,
	ttydisplay('    w     write'), ttynl,
	ttydisplay('    g     ancestors        g <n>  ancestors n'), ttynl,
	ttydisplay('    n     nodebug          =      debugging'), ttynl,
	ttydisplay('    +     spy this         -      nospy this'), ttynl,
	ttydisplay('    a     abort'), ttynl,
	ttydisplay('    @     command          u      unify'), ttynl,
	ttydisplay('    <     reset printdepth < <n>  set printdepth'), ttynl,
	ttydisplay('    ^     reset subterm    ^ <n>  set subterm'), ttynl,
	ttydisplay('    ?     help             h      help'), ttynl,
	ttynl.
