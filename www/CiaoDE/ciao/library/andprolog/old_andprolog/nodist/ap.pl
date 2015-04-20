:- module(ap, [
        active_agents/1, 
        in_parallel/2],
        []).

%% :- use_module(library(lists), [member/2]).
:- use_module(library(conc_aggregates)).
:- use_module(library(concurrency)).

:- meta_predicate in_parallel(:, :).


%% Those below are used for keeping track of the number of agents.

%% Argument is just the number of active agents.

:- concurrent number_of_active_agents/1.

number_of_active_agents(1).

%% Argument is the GoalId of every active agent.

:- concurrent active_agent/1.

%% Strategy: to execute a & b in parallel, leave "a" as a goal to be 
%% executed and find all solutions to "b".  If "a" has not been 
%% started when "b" is finished, execute it locally.  Otherwise, check 
%% for solutions to "a".  If no solutions are yet available, look for some
%% work waiting to be done.

in_parallel(GoalA, GoalB):-
        leave_goal(GoalA, IdA),
        findall(GoalB, GoalB, SolsB),
        (
            get_goal_nb(GoalA, IdA) ->    %% Fail if GoalA not started
            findall(GoalA, GoalA, SolsA)
        ;
            repeat,
            perform_some_other_work(IdA, SolsA),
            !                             %% Delete "repeat"'s chpt
        ),
        member(GoalA, SolsA),             %% Join solutions
        member(GoalB, SolsB).


perform_some_other_work(Id, Sols):-        %% Obtain Sols for Id
        get_solutions_nb(Id, Sols), !.
perform_some_other_work(_Id, _Sols):-      %% Do something else in the meantime
        get_goal_nb(OtherGoal, GoalId),
        findall(OtherGoal, OtherGoal, Solutions),
        put_solutions(GoalId, Solutions),
        fail.
            


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start an agent, register it as started and increment the number of 
%% active agents.

agent:-
        eng_self(MyId),
        asserta_fact(active_agent(MyId)),
        get_goal(Goal, Id),
        findall(Goal, Goal, Solutions),
        put_solutions(Id, Solutions),
        fail.


%% Adjust the number of agents.

active_agents(ReqNumber):-
        var(ReqNumber),
        current_fact(number_of_active_agents(ReqNumber)), !.
active_agents(ReqNumber):-
        integer(ReqNumber),
        ReqNumber >= 1,
        retract_fact(number_of_active_agents(NumAct)), !,
        (
            ReqNumber >= NumAct ->                 %% (Maybe) Increase it
            MoreAgents is ReqNumber - NumAct,
            create_agents(MoreAgents)
        ;
            LessAgents is NumAct - ReqNumber,
            kill_agents(LessAgents)            
        ),
        asserta_fact(number_of_active_agents(ReqNumber)).

create_agents(0):- !.
create_agents(N):-
        N > 0,
        eng_call(agent, create, create),
        N1 is N - 1,
        create_agents(N1).

kill_agents(0):- !.
kill_agents(N):-
        N > 0,
        retract_fact_nb(active_agent(Active)), !,
        eng_kill(Active),
        N1 is N - 1,
        kill_agents(N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Concurrent server of ids for goals.  Note the cut after retract_fact/1.
%% this is the standard way to implement a concurrent ticket server; 
%% it as to wait for the next ticket to be available, and retract it
%% atomically.  Then, no choicepoint should be left.

:- concurrent goal_id/1.

goal_id(0).

next_id(N):-
        retract_fact(goal_id(N)), !,
        N1 is N + 1,
        asserta_fact(goal_id(N1)).
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Assorted utilities to leave and take goals

:- concurrent goal_to_execute/2.   %% (Id, Goal)
:- concurrent solutions/2.         %% (Id, Sols)

leave_goal(Goal, Id):-
        next_id(Id),
        assertz_fact(goal_to_execute(Id, Goal)).

get_goal(Goal, Id):-
        retract_fact(goal_to_execute(Id, Goal)).

get_goal_nb(Goal, Id):-
        retract_fact_nb(goal_to_execute(Id, Goal)).


%% There should be one set of solutions per goal, so backtracking must
%% not wait for more solutions.  However, retract_fact_nb/1 cannot be
%% used, since we have to wait for the first tuple.  We just cut the
%% (dynamic) choicepoint left by the retract_fact/1 primitive.

get_solutions(Id, Sols):-
        retract_fact(solutions(Id, Sols)), !.

get_solutions_nb(Id, Sols):-
        retract_fact_nb(solutions(Id, Sols)), !.

put_solutions(Id, Sols):-
        assertz_fact(solutions(Id, Sols)).
