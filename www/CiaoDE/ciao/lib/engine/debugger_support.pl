:- module(debugger_support, [], [assertions]).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(internals), [term_to_meta/2]).

:- export('$retry_cut'/2).
:- impl_defined(['$retry_cut'/2]).

:- export('$debugger_state'/2).
:- impl_defined(['$debugger_state'/2]).

:- export('$spypoint'/3).
:- impl_defined(['$spypoint'/3]).

:- export('$debugger_mode'/0).
:- impl_defined(['$debugger_mode'/0]).


:- export(srcdbg_spy/6).
:- pred srcdbg_spy/6 # "Performing source level debugging, all goals
   are expanded to this. This is currenlty done for all interpreted
   code.".
:- comment(hide,srcdbg_spy/6).
:- meta_predicate srcdbg_spy(goal,?,?,?,?,?).

srcdbg_spy(Goal, _, _, _, _, _) :-
        term_to_meta(G, Goal),
        '$meta_call'(G).

/*
srcdbg_spy(Goal, _, _, _, _, _) :-
	'$debugger_state'(State,State),
	arg(1, State, X),
	( X = off ->
	     term_to_meta(G, Goal),
	     '$meta_call'(G)
	;
	    true
	).
*/
% srcdbg_spy(_,_,_,_,_,_):-
%  	'$debugger_state'(State,State),
%  	(  
%  	    arg(1,State,trace)
%  	;
%  	    arg(1,State,debug)
%  	),!.

% srcdbg_spy(Goal,_,_,_,_,_):-
%  	'$debugger_state'(State,State),
%  	arg(1,State,off),!,
%  	term_to_meta(G,Goal),
% 	'$meta_call'(G).
