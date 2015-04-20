:- op(1050,fx,'on').
:- op(1050,fx,'when').
:- op(1050,fx,'before').
:- op(1050,fx,'after').
:- op(1100,xfx,'do').
:- op(150, xfx, [$]).
:- op(950, xfx, (=>)).
:- op(1150, fx, [message_def,alarm_def,message,alarm,argnames]).

:- new_declaration(use_messages/1).
:- new_declaration(use_protocol/2).
:- new_declaration(task/1).
:- new_declaration(activity/1).

:- load_compilation_module(library('agents/agents_tr')).
:- add_sentence_trans(translate/3).
:- add_term_trans(argnames_use/3).

:- use_package(class).

:- inherit_class(library('agents/agents_class')).

:- export([main/0]).


:- data message_defined/1.
:- data message_implemented/1.
:- data alarm_defined/1.
:- data alarm_implemented/1.

:- concurrent running_tasks/2.
:- concurrent finished_tasks/2.
:- concurrent ready_tasks/2.

:- data def_before_task/2.
:- data def_after_task/2.


'$before_task'(Task):-
	( def_before_task(Task,Before),
	  self(S), Y = (S:Before), call(Y) ->
	  true
	; 
	  true
	).
	  

'$do_task'(Task):-
	( self(S), Y = (S:Task), call(Y) ->
	  ( continue_task(Task) ->
	    display('Redo' + Y),nl,
	    '$do_task'(Task)
	  ;
	    true
	  )
	;
	  show(['Error doing task',Task])
	).

'$after_task'(Task):-
	( def_after_task(Task,After),
	  self(S), Y = (S:After), call(Y) ->
	  true
	;
	  true
	).

'$check_protocol'(_,_,_).

'$check_message'(Messg,implemented):-
	message_implemented(Messg),!.

'$check_message'(Messg,defined) :-
	functor(Messg,M,A), 
	message_defined('/'(M,A)),!.

'$check_message'(_,not_defined).

'$process_message'(Term):-
 (self(X), Y = (X:Term), call(Y)).


:- init_trans.

:- use_package(expander).