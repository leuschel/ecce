:- class(prueba).

:- inheritable([add_task/1,remove_task/1]).
:- inheritable([agent_name/1,show/1,goal_id/1]).


:- use_module(library(concurrency)).
:- use_module(plat,[ouput/2,wait/1]).

:- export(main/0).
:- export(check/1).
:- export(assert/1).
:- export(state/0).

:- data name/1.
:- concurrent ready_tasks/2.
:- concurrent running_tasks/2.
:- concurrent finished_tasks/2.

%:- inheritable([ready_tasks/2,running_tasks/2,finished_tasks/2]).
:- inheritable([name/1]).

prueba(Name) :-
	set_fact(name(Name)),
	eng_goal_id(Id),self(S),
	show([S,Id]),
	start(taskmanager),
	start(finishmanager),
	eng_status.

state :-
	eng_status.

check(X):-
	current_fact_nb(X).

assert(X):- 
	asserta_fact(X).

agent_name(X):-
	var(X),
	current_fact(name(X)).

goal_id(X):-
	var(X),
	eng_goal_id(X).

main:-  add_task(mio(pepe)),
	wait(3),
	add_task(wakeup).
%	start(mio(pepe)),
%	start(wakeup).

add_task(T):-
	asserta_fact(ready_tasks(T,_)),
	show(task_added(T)),nl.

remove_task(T):-
	retract_fact(running_tasks(T,Id)),
	show(run(T,Id)),
	asserta_fact(finished_tasks(T,Id)),
	show(end(T,Id)).

show(P):-
	name(Name),
	ouput(Name,P).

start(X) :-
	self(S), Y = (S:X), show(call(Y)),
	eng_call(Y,create,create,Id),
	asserta_fact(running_tasks(X,Id)),
	show([start,Y,Id]).

taskmanager:-
	retract_fact(ready_tasks(X,N)),
	show([taskmanager,X,N]),
	start(X),
	taskmanager.

finishmanager:-
	retract_fact(finished_tasks(T,Id)),
	eng_status,
	eng_wait(Id),
	show([finishmanager,T,Id]),
	eng_release(Id),
	eng_status,
	finishmanager.

wakeup :-
	wait(5),
	add_task(mio(luis)),
	eng_goal_id(Id),
	show(doing(wakeup(Id))),
	remove_task(wakeup).

mio(X):-
	eng_goal_id(Id),
	show(doing(mio(X,Id))),
	remove_task(mio(X)).
