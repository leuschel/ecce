:- module(
        _,
        [(&&>)/2, (<&&)/1, (&>)/2, (<&)/1],
        []).


:- use_module(library(concurrency)).
:- use_module(library(prolog_sys), [new_atom/1]).

:- concurrent ident_and_goal/4.     %% Put requests
:- concurrent ident_and_answer/3.   %% Get answers

:- concurrent sleeping_threads/1.   %% Number of threads waiting for a goal.

sleeping_threads(0).


:- meta_predicate(&&(:)).
:- meta_predicate(&&>(:, ?)).
:- meta_predicate(&(:)).
:- meta_predicate(&>(:, ?)).

&&(Goal):-
        ensure_agent,
        &(Goal).

&(Goal):-
        assertz_fact(ident_and_goal(_Ident, Goal, call, _WamId)).

&>(Goal, Handle):-
        new_atom(Ident),
        Handle = '$handle'(Ident, Goal),
        assertz_fact(ident_and_goal(Ident, Goal, call, _WamId)),
        display(asserted(ident_and_goal(Ident, Goal))),
        nl.

&&>(Goal, Handle):-
        ensure_agent,
        &>(Goal, Handle).

<&&(Handle):-
        Handle = '$handle'(Ident, Goal, WamId),
        retrieve_answer(Ident, AnswerGoal, WamId),   %% Binds WamId
        first_and_backtracking(Ident, WamId, AnswerGoal, Goal).

first_and_backtracking(_Ident, _WamId, Goal, Goal).    %% Current sol.
first_and_backtracking(Ident, WamId, _AnswerGoal, _Goal):-
 %% Leave a signal to perform backtracking
        assertz_fact(ident_and_goal(Ident, __Goal, backtrack, WamId)),
        fail.

 %%         display(retracted(ident_and_answer(Ident, Goal, WamId))),
 %%         nl.


<&(Handle):- <&&(Handle).

retrieve_answer(Ident, Goal, WamId):-
        retract_fact(ident_and_answer(Ident, Goal, WamId)), !.

ensure_agent:-
        (
            current_fact_nb(sleeping_threads(0)) ->
            eng_call(agent, create, create)
        ;
            true
        ).


agent:-
%% A new agent is born: add it to the agent count
        increase_num_of_agents,
%% Wait for a new goal to execute
        retract_fact(ident_and_goal(Ident, Goal, Action, WamId)),
        display(picked_up(Ident, Goal)), nl,
%% Once we have it, execute it --- but there is an agent less to execute
        decrease_num_of_agents,
        do_action(Action, Ident, Goal, WamId),
        display(put(Ident, Goal)), nl,
%% And now there is again a new agent to run goals
        increase_num_of_agents,
%% Back to get a new goal
        fail.

do_action(call, Ident, Goal, _WamId):-
        eng_call(wrapper(Goal, Ident), create, self, _WamId).
do_action(backtrack, _Ident, _Goal, WamId):-
        eng_backtrack(WamId, self).
do_action(cut, _Ident, _Goal, WamId):-
        eng_cut(WamId).
do_action(release, _Ident, _Goal, WamId):- 
        eng_release(WamId).

wrapper(Goal, Ident):-
        eng_self(WamId),
        call(Goal),
        assertz_fact(ident_and_answer(Ident, Goal, WamId)).

increase_num_of_agents:-
        retract_fact(sleeping_threads(NumAg)), !,
        NewNumAg is NumAg + 1,
        asserta_fact(sleeping_threads(NewNumAg)).

decrease_num_of_agents:-
        retract_fact(sleeping_threads(NumAg)), !,
        NewNumAg is NumAg - 1,
        asserta_fact(sleeping_threads(NewNumAg)).
