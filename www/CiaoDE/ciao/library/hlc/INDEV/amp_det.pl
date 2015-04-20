:- module(_, 
        [(&&)/1, (&&>)/2, (<&&)/1, kill/1], 
        [
%% I cant make it work!
%%           'amp/ampersands' 
        ]).

%% Skeletal implementation, deterministic goals which do not fail,
%% no error checking.

:- use_module(library(concurrency)).

:- concurrent wrapped_goal/2.

:- meta_predicate(&&(:)).
:- meta_predicate(&&>(:, ?)).


%% Just start the goal, do not mind what happens to it

&&(Goal):- eng_call(Goal, create, create).


&&>(Goal, '$handle'(GoalId, Goal)):-     %% "Goal" carries logical vars.
        eng_call(wrapper(Goal), create, create, GoalId).


<&&('$handle'(GoalId, Goal)):-   %% Fails if no such handle.
        eng_wait(GoalId),       %% Optional
        retract_fact(wrapped_goal(GoalId, WhatHasHappened)),   %% Wait for it!
        eng_release(GoalId),
        WhatHasHappened = Goal, !.   %% Bind vars. back or fail.


wrapper(Goal):-                  %% Assumes Goal does not backtrack
        eng_self(GoalId),
        call(Goal),
        asserta_fact(wrapped_goal(GoalId, Goal)).


kill(Handle):-
        Handle = '$handle'(GoalId, _Goal),
        integer(GoalId),
        current_fact_nb(currently_running(GoalId)),   %% Fail if no such goal
        eng_kill(GoalId).
