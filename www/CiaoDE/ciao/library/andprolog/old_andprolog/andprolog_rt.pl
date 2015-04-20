
:- module(
        andprolog_rt,
        ['&'/2,
         % '=>'/2,
         indep/1,indep/2,active_agents/1],
        [assertions, isomodes]
        ).


:- include(library('andprolog/andprolog_ops')).

:- set_prolog_flag(multi_arity_warnings, off).

%% FOR TEMPORARILY PARTIALLY DOCUMENTING:
:- use_module(library('assertions/doc_props')).
:- use_module(library(concurrency)).
:- use_module(library(prolog_sys)).

%%***************************************************************************

:- concurrent number_of_active_agents/1.

number_of_active_agents(1).

:- concurrent active_agent/1.

 %% :- comment(GoalA & GoalB,"@var{GoalA} and @var{GoalB} are run in
 %% independent and-parallel fashion.  This is just a first sketch, and
 %% valid only for deterministic independent goals.").

:- meta_predicate((goal&goal)).
:- true comp (A & B) + native(call((A,B))).

GoalA & GoalB:-
        leave_goal(GoalA, IdA),
        call_with_result(GoalB, ResultB),   %% Possibility of int. back. here!
        (
            get_goal_nb(GoalA, IdA) ->    %% Fails if GoalA not started
            call_with_result(GoalA, ResultA)
        ;
            repeat,            %% Backtrack while checking Goal A
            perform_some_other_work(IdA, GoalA, ResultA),
            !                             %% Delete "repeat"'s chpt
        ),
        ResultA = success,
        ResultB = success.

perform_some_other_work(Id, Solution, Result):-        %% Obtain Sols for Id
        get_solution_nb(Id, Solution, Result), !.
perform_some_other_work(_Id, _Sols, _Result):- %% Do something else in the meantime
        get_goal_nb(OtherGoal, GoalId),
        call_with_result(OtherGoal, Result),
        put_solution(GoalId, OtherGoal, Result),
        fail.


call_with_result(Goal, WhatHappened):-
        call(Goal), !,             %% Deterministic, anyway!
        WhatHappened = success.
call_with_result(_Goal, failure).

agent:-
        eng_self(MyId),
        asserta_fact(active_agent(MyId)),
        get_goal(Goal, Id),
        call_with_result(Goal, Result),
        put_solution(Id, Goal, Result),
        fail.


%% Adjust the number of agents.

 %% :- pred active_agents(?NumberOfAgents): int # "Tests/sets the number of
 %% active agents looking for goals to execute.  As for now, those agents
 %% are resource-consuming, even when they are just looking for work, and
 %% not executing any user goals.".

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

 %% :- concurrent goal_id/1.
 %% 
 %% goal_id(0).
 %% 
 %% next_id(N):-
 %%         retract_fact(goal_id(N)), !,
 %%         N1 is N + 1,
 %%         asserta_fact(goal_id(N1)).
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Assorted utilities to leave and take goals

:- concurrent goal_to_execute/2.   %% (Id, Goal)
:- concurrent solution/3.         %% (Id, Sols, Result)

leave_goal(Goal, Id):-
        new_atom(Id),
        assertz_fact(goal_to_execute(Id, Goal)).

get_goal(Goal, Id):-
        retract_fact(goal_to_execute(Id, Goal)).

get_goal_nb(Goal, Id):-
        retract_fact_nb(goal_to_execute(Id, Goal)).


%% There should be one set of solution per goal, so backtracking must
%% not wait for more solution.  However, retract_fact_nb/1 cannot be
%% used, since we have to wait for the first tuple.  We just cut the
%% (dynamic) choicepoint left by the retract_fact/1 primitive.

get_solution(Id, Sols, Result):-
        retract_fact(solution(Id, Sols, Result)), !.

get_solution_nb(Id, Sols, Result):-
        retract_fact_nb(solution(Id, Sols, Result)), !.

put_solution(Id, Sols, Result):-
        assertz_fact(solution(Id, Sols, Result)).



%%***************************************************************************

 %% :- meta_predicate((goal=>goal)).
 %% 
 %% :- comment(A=>B,"If @var{A} is true @var{B} is run in parallel,
 %% 	otherwise sequentially.").
 %% :- trust pred '=>'(A,B) : andcallable(B) + doc_incomplete.
 %% 
 %% % :- impl_defined((=>)/2).
 %% 

:- prop andcallable(Exp) # "@var{Exp} is of the form @tt{goal&goal}.".

andcallable(A&B):- callable(A), callable(B).

 %% :- comment(indep(X,Y), "@var{X} and @var{Y} are @index{independent},
 %%    i.e., they are bound to terms which have no variables in
 %%    common. For example, @tt{indep(X,Y)} holds for @tt{X=f(Z),Y=g(K)}
 %%    and also for @tt{X=f(a),Y=X} (since both @tt{X} and @tt{Y} are
 %%    bound to ground terms). It does not hold for @tt{X=f(Z),Y=g(Z)} and
 %%    for @tt{X=Y}.").

:- true prop indep(X,Y) + native(indep([[X,Y]]))
	# "@var{X} and @var{Y} do not have variables in common.".
 
indep(A,B) :- 
        mark(A,Ground),  % Ground is var if A ground
        nonvar(Ground),  % If 1st argument was ground, no need to proceed
        marked(B), !,
        fail.
indep(_,_).

mark('$$Mark', no )  :- !.        % Mark the variable, signal variable found
mark( Atom   , _  )  :- atomic(Atom),!.
mark(Complex , GR)  :- mark(Complex,1,GR).

mark(Args,Mth,GR) :-
        arg(Mth,Args,ThisArg),!,
        mark(ThisArg,GR),
        Nth is Mth+1,
        mark(Args,Nth,GR). 	
mark(_,_,_).

marked( Term )  :-
        functor(Term,F,A),
        (  A > 0, !, marked(Term,1)
        ;  F = '$$Mark' ).

marked(Args,Mth) :-
        arg(Mth,Args,ThisArg),!,
        (  marked(ThisArg)
        ;  Nth is Mth+1,
           marked(Args,Nth)).

 %% :- prop indep(X,Y) 
 %% # "The terms to which @var{X} and @var{Y} are bound do not have
 %%    variables in common.".

 %% :- comment(ground(X), "The term @var{X} does not contain variables
 %% (and, hence, is not a variable itself).").
 %% 
 %% :- prop ground(X) # "The term @var{X} does not contain variables
 %% (and, hence, is not a variable itself).".
 %% 
 %% ground(Term):-
 %%         nonvar(Term),
 %%         functor(Term,_,N),
 %%         ground(N,Term).
 %% 
 %% ground(N,Term):-
 %%         N > 0,
 %%         arg(N,Term,Arg),
 %%         ground(Arg),
 %%         N1 is N-1,
 %%         ground(N1,Term).
 %% ground(0,_).
 %% 


 %% :- comment(indep(X), "The variables in each pair of the list
 %% @tt{@var{X}} are pairwise independent.").

:- true prop indep(X) + native(indep(X))
	# "The variables in pairs in @tt{@var{X}} are pairwise independent.".

indep([]).
indep([[X,Y]|L]):- indep(X,Y), indep(L).
