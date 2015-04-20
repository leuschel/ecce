:- class(bug).
:- use_module(library(concurrency)).

:- concurrent ready_tasks/2.
:- concurrent running_tasks/2.
:- concurrent finished_tasks/2.

:- export([main/0,check/1,tasks/0]).

check(X):-
	current_fact_nb(X).

main:-
     eng_call(taskmanager,create,create,Id),
     display([taskmanager,Id]),nl,
%     eng_call(finishmanager,create,create,Id2),
     asserta_fact(ready_tasks(mio(pepe),2)).
%     eng_call(tasks,create,create,Id3),
%     display([tasks,Id3]),nl.

taskmanager:-
	retract_fact(ready_tasks(X,_)),
	display(X),nl,
	self(N), Y = (N:X), 
	eng_call(Y,create,create),
	taskmanager.

finishmanager:-
	retract_fact(finished_tasks(X,Id)),
	eng_status,
	eng_wait(Id),
	eng_release(Id),
	finishmanager.
	
mio(X):-
	eng_goal_id(Id),
	display([X,Id]),nl,
	asserta_fact(finished_tasks(mio(X),Id)),
	display(done),nl.
	

tasks :-
	asserta_fact(ready_tasks(mio(luis),_)).