

:- module(_, 
        [(&&)/1, (&&>)/2, (<&&)/1, kill/1], 
        [
%%            'amp/ampersands' 
        ]).

:- use_module(library(concurrency)).

:- concurrent wrapped_goal/2.
:- concurrent currently_running/1.

:- meta_predicate(&&(:)).
:- meta_predicate(&&>(:, ?)).


&&(Goal):- eng_call(Goal, create, create).


&&>(Goal, Handle):-
        Handle = '$handle'(Goal, GoalId),     %% "Goal" carries logical vars.
        eng_call(wrapper(Goal), create, create, GoalId).


%% the call to join_goal/1 is optional in the code below: since we will
%% anyway wait for the fact which has the "executed" goal  wrapped inside,
%% we may as well dispose of this call.  But it performs a wait on a part
%% diferent from the shared database, and therefore it favours a faster
%% update of facts.  Also, since it can call POSIX threads primitives,
%% waiting on a goal to finish can be esaily made non-active.


%% Still lacking: backtracking.  Should it be done in &&> or in <&& ?

<&&(Handle):-   %% Fails if no such handle.
        Handle = '$handle'(Goal, GoalId),
        integer(GoalId),
        current_fact_nb(currently_running(GoalId)),   %% Fails if no such goal
        eng_wait(GoalId), %% Optional
        retract_fact(wrapped_goal(WhatHasHappened, GoalId)),   %% Wait for it!
        retract_fact(currently_running(GoalId)), !,
        eng_release(GoalId),
        WhatHasHappened = Goal.   %% Bind vars. back or fail.


wrapper(Goal):-  %% Assumes Goal does not backtrack
        eng_self(GoalId),
        asserta_fact(currently_running(GoalId)),
        call(Goal),
        asserta_fact(wrapped_goal(Goal, GoalId)).
wrapper(Goal):-   %% In case of failure, leave a trace.
        eng_self(GoalId),
        asserta_fact(wrapped_goal(Goal-failed, GoalId)).


kill(Handle):-
        Handle = '$handle'(_Goal, GoalId),
        integer(GoalId),
        current_fact_nb(currently_running(GoalId)),   %% Fail if no such goal
        eng_kill(GoalId).

cut(Handle):-
        Handle = '$handle'(_Goal, GoalId),
        eng_wait(GoalId),
        eng_release(GoalId).
