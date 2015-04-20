%%----------------------------------------------%%
%% A Simple asker Agent.                        %%
%%----------------------------------------------%%

%% Agent declaration: the current source defines a class of agents.
:- class(asker_tr).

%%----------------------------------------------------------------
%% Added for correct work of the agent, do not change
%%----------------------------------------------------------------
:- inherit_class(library(agents)).
:- use_module(library(platforms)).
%:- use_package(argnames).

:- export([main/0,check/1]).

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

%%----------------------------------------------------------------
%% Added for correct work of the agent, do not change
%%----------------------------------------------------------------

asker_tr(Name):-
	agents(Name,asker_tr).


check(P):-
	current_fact_nb(P).

:- implements(library('agents/protocols/ask_inform')).

% State declaration: 

:- data question/1.

question(capital(francia)).
question(capital(alemania)).
question(capital(españa)).
question(capital(italia)).
:- data no_more_question/0.

:- data knowledge/2.

knowledge(capital(portugal),lisboa).

% Interface declaration: the following messages will
% be acepted at run-time.
%% :- message inform.
%% :- export inform/3.


%% Tasks:
%% :- task asker_ask.

	
% Initial Task

main :-
	show('Doing main'),
	register_service([asking]),
	add_task(look_for_answer,true).

look_for_answer :-
	( look_for_service(Answer,[answering]) ->
	  add_task(asker_ask(Answer),true),
	  end_task(look_for_answer)
	;
	  wait(3)).

%% Task Answer_ask
asker_ask(Answer) :-
	  ( current_fact(question(Q)) ->
	    Msg = message(ask,_,_,Q,_),
	    send(Answer,Msg),
	    recieve(Answer,_Msg2)
	  ;
	    send(Answer,message(no_more_ask,_,_,_,_)), 
	    end_task(asker_ask(Answer))
	  ).
	
inform(_X, Answer, _Mess_Id):-
	current_fact(question(Question)),
	asserta_fact(knowledge(Question,Answer)),
	current_fact_nb(knowledge(Question1,Answer1)),
	show(knowledge(Question1,Answer1)),
	retract_fact(question(Question)).
