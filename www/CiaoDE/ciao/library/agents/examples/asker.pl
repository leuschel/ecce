%%----------------------------------------------%%
%% A Simple asker Agent.                        %%
%%----------------------------------------------%%

%% Agent declaration: the current source defines a class of agents.
:- agents(asker).

:- include(library('agents/protocols/ask/messages')).

:- alarm_def [type,sender,reciever,content].
:- alarm problem.

:- use_module(library(platforms),[look_for_service/2,wait/1]).

:- use_protocol(library('agents/protocols/ask'),asker).

:- activities([asking]).

:- tasks([look_for_answer/0,asker_ask/1]).

:- export(prueba/0). 

% State declaration: 

:- data question/1.

question(capital(francia)).
question(capital(alemania)).
question(capital(españa)).
question(capital(italia)).

:- data knowledge/2.

knowledge(capital(portugal),lisboa).

%% Tasks:
:- tasks([look_for_answer,asker_ask]).

	
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

before asker_ask(Answer) do
	show(['Before asker_ask',Answer]).

%% Task Answer_ask
asker_ask(Answer) :- 
	  ( current_fact(question(Q)) ->
	    send(Answer,message${type => ask, content => Q}),
	    receive(Answer,_Msg2)
	  ;
	    send(Answer,message${type => no_more_ask}), 
	    end_task(asker_ask(Answer))
	  ).

after asker_ask(Answer) do
	show(['After asker_ask',Answer]).

when inform${content => Answer} do
	current_fact(question(Question)),
	asserta_fact(knowledge(Question,Answer)),
	current_fact_nb(knowledge(Question1,Answer1)),
	show(knowledge(Question1,Answer1)),
	retract_fact(question(Question)).

on problem${content => C} do
	show(['Problem', C]).
 
prueba :-
	message_defined('/'(Name,A)),
	message_implemented(Y),
	Y =.. [Name|Arg],
	show([messages,(Name/A),Y]),
	fail.
