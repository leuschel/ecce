%%----------------------------------------------%%
%% A Simple answer Agent.                        %%
%%----------------------------------------------%%
 
%% Agent declaration: the current source defines a class of agents.
:- class(answer_tr).

%%----------------------------------------------------------------
%% Added for correct work of the agent, do not change
%%----------------------------------------------------------------
:- inherit_class(library(agents)).

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

message_defined(ask/3).
message_implemented(ask(_,_,_)).

answer_tr(Name):-
	agents(Name,answer_tr).


check(P):-
	current_fact_nb(P).


:- implements(library('agents/protocols/inform_ask')).

% State declaration: 
:- data knowledge/2.

knowledge(capital(españa),madrid).
knowledge(capital(francia),paris).

% Interface declaration: the following messages will
% be acepted at run-time.
%% :- message ask.
%% :- export ask/3.

%% Activities:
% :- activity answering.
answer_activity(answering).

%% Tasks:
%% :- task answer_ask.


% Initial Task

main :-
	show('Doing main'),
	register_service([answering]),
	add_task(answer_ask,true).
	

%% Task Answer_ask
answer_ask :-
    recieve(_Sender,_Msg).
   
ask(X,Question,_Mess_Id):-	
	( current_fact_nb(knowledge(Question,Answer)) ->
	   send(X,message(inform,_,_,Answer,_))
	;
	  % input(X,Question,User_Answer), 
	   send(X,message(inform,_,_,dont_know,_))
	).

no_more_ask(_X,_,_Mes):-
	true.

