%%----------------------------------------------%%
%% A Simple prueba Agent.                        %%
%%----------------------------------------------%%
 
%% Agent declaration: the current source defines a class of agents.
:- class(hprueba).

:- inherit_class(library('agents/examples/prueba')).

:- export([init/0,hmain/0,hmio/1,hwakeup/0]).
 
:- use_module(library(platforms)).


:- data knowledge/2.

knowledge(capital(españa),madrid).
knowledge(capital(francia),paris).


hprueba(Name):-
	prueba(Name).

hcheck(P):-
	current_fact_nb(P).


% Initial Task

init:- main.

hmain :-
	agent_name(Name),
	show(Name),nl,
	register_service(Name,[answering]),
	add_task(hmio(pepe)),
	wait(5),
	add_task(hwakeup).
	
hwakeup :-
	wait(5),
	add_task(hmio(luis)),
	goal_id(Id),
	show(doing(hwakeup(Id))),
	remove_task(hwakeup).

hmio(X):-
	goal_id(Id),
	show(doing(hmio(X,Id))),
	remove_task(hmio(X)).