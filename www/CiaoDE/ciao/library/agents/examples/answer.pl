 
%%----------------------------------------------%%
%% A Simple answer Agent.                        %%
%%----------------------------------------------%%
  
:- agents(answer).  

:- include(library('agents/protocols/ask/messages')).

:- alarm_def [type,sender,reciever,content].
:- alarm problem.

  
:- use_protocol(library('agents/protocols/ask'),answer).

:- activities([answering]).

:- tasks([answer_ask/0]).
 
:- data knowledge/2.

:- export(prueba/0).

knowledge(capital(francia),paris).
knowledge(capital(españa),madrid).
knowledge(capital(alemania),berlin).

main :-	show('Doing Main'),
	register_service([answering]),
	add_task(answer_ask,true).	

before answer_ask do
	show('Before answer_ask').

answer_ask :-
    receive(_Sender,_Msg).

after answer_ask do
	show('After answer_ask').

when ask${sender => X, content => Question} do
	( current_fact_nb(knowledge(Question,Answer)) ->
	   send(X,message${type => inform,content => Answer})
	;
	  % input(X,Question,User_Answer), 
	   send(X,message${type => inform, content => dont_know})
	).

when no_more_ask${} do true.

on problem${sender => S, content => T} do	
    show([problem,S,T]).

prueba :-
	message_defined('/'(Name,A)),
	message_implemented(Y),
	Y =.. [Name|Arg],
	show([messages,(Name/A),Y]),
	fail.
