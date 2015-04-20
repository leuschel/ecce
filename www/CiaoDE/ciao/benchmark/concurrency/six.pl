 %% :- module(six, 
 %%         [
 %%             six_one/0,
 %%             six_two/0,
 %%             six_three/0,
 %%             six_four/0,
 %%             six_five/1,
 %%             six_six/2,
 %%             six_seven/1
 %%         ], []).

:- use_module(library(system)).
:- use_module(displaynl).
:- use_module(library(dummy)).

walltime(T):- statistics(walltime, [_, T]).

displaynl(W):-
        display(W),
        nl.



% Six: the shared concurrent database.
% Facts in Ciao Prolog can be made concurrent: this means that the declaration

:- concurrent fact/1.

% makes fact/1 to be concurrent, so that several concurrent goals can
% access and alter it in a ordered manner with well defined
% operational semantics.  Addition of facts can be made using
% asserta_fact/1; access to facts can be made using current_fact/1,
% and deletion of facts is made with retract_fact/1.  Calling
% current_fact/1 with a concurrent fact as argument will suspend the
% call until a fact is present (instead of failing, as it is the
% behavior if it were not concurrent).

six_one:-
        retractall_fact(fact(_)),               % Make sure DB is empty
        launch_goal(wait_for_fact(fact(_))),
        pause(2),
        walltime(X),
        displaynl(asserting(fact(X))),
        asserta_fact(fact(X)),
        pause(1).

wait_for_fact(Fact):-
        current_fact(Fact),
        retract_fact(Fact),
        displaynl(got(Fact)).

% Actually, retract_fact/1 also blocks waiting for a fact to be
% present, so the above code can also be written:
% 
%   wait_for_fact(Fact):-
%           retract_fact(Fact),
%           displaynl(got(Fact)).
% 
% and *should* be written so.  The reason is that, since several
% agents may be willing to retrieve the same piece of information, it
% may happen that the particular fact we are interested in is deleted
% in the time from the call to current_fact/1 to the call to
% retract_fact/1.  Probably we want and atomic access and retrieval of
% some matching fact: this is done using retract_fact/1.

% 
% When current_fact/1 is forced to backtrack on a concurrent
% predicate, the call waits for another clause of the predicate to be
% present in the database.  Thus, we can modelate strean communication
% using the database.  Two points to take into account:
% 
%  * We are not removing the facts from the database, so a later call to
%    wait_for_several_facts (without erasing them previously) will not
%    have to wait.
% 
%  * We are using assertz_fact/1 instead of assert_fact/1 to make sure
%    that the newly added facts appear at the end of the list of the
%    clauses of the predicate --- which is where the pointer of
%    current_fact/1 moves when making backtracking.
% 
% The fact with the argument 'end' will mark the end of the insertion
% of facts.  If we do not mark it, wait_for_several_facts/0 will
% remain looping forever waiting for an instance of the clause to
% appear.  There are other means to avoid that, and we will see them
% later.  Of course, one of them is to kill the thread waiting for
% more facts.

six_two:-
        retractall_fact(fact(_)),
        launch_goal(wait_for_several_facts),
        assert_several_facts(10, 1).         %% Number of facts, pause

assert_several_facts(0, _Pause):-
        assertz_fact(fact(end)).
assert_several_facts(N, Pause):-
        N > 0,
        assertz_fact(fact(N)),
        pause(Pause),
        N1 is N - 1,
        assert_several_facts(N1, Pause).

wait_for_several_facts:-
        current_fact(fact(What)),
        displaynl(got(What)),
        What = end.

% Let's stress the implementation...

number_of_facts(1000).

six_three:-
        retractall_fact(fact(_)),
        number_of_facts(NFacts),
        launch_goal(assert_several_facts(NFacts, 0)),
        launch_goal(wait_for_several_facts).


% Two-way communication: a concurrent goal produces tokens, which are
% consumed by another concurrent goal, which in turn produces another
% token which is brought back to the initial producer.  Note that
% there are cuts right after the retract_fact/1 calls.  Those cuts are
% of outmost importance when it comes to performance: otherwise every
% call to retract_fact/1 adds an entry noting its existence, and this
% entry has to be updated by every assertion and retraction which
% happens later.  In general terms, if you do not cut when you retract
% or consult, then the cost of every further assertion/retraction is
% proportional to the number of uncut assertions+retractions made so
% far, and thus the complexity is, at least, proportional to the
% square of the total number of assertions/retractions.  Try removing
% the cuts, and you'll see it takes much longer.


:- concurrent go_token/1, return_token/1. 

number_of_tokens(50000).

six_four:-
        retractall_fact(go_token(_)),
        retractall_fact(return_token(_)),
        launch_goal(token_return, GId1),
        launch_goal(token_go, GId2),
        number_of_tokens(NTokens),
        asserta_fact(go_token(NTokens)),  % This actually triggers execution...
        join_and_release_goals([GId1, GId2]),
        displaynl('Goals finished').

token_go:-
        retract_fact(go_token(What)), !,
        Answ is What - 1,
        assertz_fact(return_token(Answ)),
        Answ = 0.

token_return:-
        retract_fact(return_token(What)), !,
        assertz_fact(go_token(What)),
        What = 0.


% add the numbers from 1 to N.  One worker writes down facts for the
% numbers, the other reads them and adds.  Note the cuts after
% retraction; try with big numbers, it should work.

six_five(N):-
        retractall_fact(fact(_)),
        launch_goal(generate_numbers(N)),
        launch_goal(sum_up_numbers, Id2),
        join_and_release_goal(Id2),
        retract_fact(fact(Res)),
        displaynl(result(Res)).

generate_numbers(0):-
        assertz_fact(fact(0)).
generate_numbers(N):-
        N > 0,
        assertz_fact(fact(N)),
        N1 is N - 1,
        generate_numbers(N1).

sum_up_numbers:-
        retract_fact(fact(CurrNum)), !,
        sum_up_numbers(CurrNum, 0, Res),
        asserta_fact(fact(Res)).

sum_up_numbers(0, SoFar, SoFar).
sum_up_numbers(Current, Added, Res):-
        Current > 0,
        NewAdded is Current + Added,
        retract_fact(fact(NewCurrent)), !,
        sum_up_numbers(NewCurrent, NewAdded, Res).



% Add the numbers from one to Max, using NumThreads.  All the numbers
% are put on a pool, a fact per number.  Facts are retracted in pairs,
% added, and the result is put back in the pool.  Every fact carries
% the number of numbers which have been added to it.  When only one
% fact remains in the pool, the threads exit.  We can detect this
% situation because it carries the number of facts that have been
% added to produce that final fact.  One point to take into account:
% threads need to retract facts in pairs, but that is done one by one,
% in fact.  This means that when few pairs remain left, threads might
% starve unless measures are taken: each thread gets a fact, and tries
% to grab another one.  If it fails to retract the second fact (note
% the retract_fact_nb/1 used: _nb stands for "non-blocking"), the
% first fact retracted is put back in the database, and the game
% starts again.  In fact, this is not completely right: the scenario
% thread 1 gets fact A, thread 2 gets fact B (and there are no more
% left), both fail, put facts back, and start over, can be repeated ad
% infinitum.  Ah, yes, when a thread detects it has retracted the
% final fact, it puts it back to the database and exits.  I know, it
% is not elegant, and it is not even correct.  I will accept better
% algorithms.
%%
% Ah, yes, one more thing: it is a *very bad* idea to wait insistently
% (for example, in a retract_fact/1 or in a current_fact/1) for an
% instance of a predicate in which others are asserting and retracting
% clauses, as this makes everything very slow.  I.e., making the
% six_six/2 predicate wait for retract_fact(to_add(Max, Result))
% *before* doing a join_goal will make the process awfully sluggish
% (apart from making the rest of the code incorrect, because some
% other threads would not finish, anyway).  The reason for this is
% that the (suspended) retract_fact/1 call has to be reactivated and
% reevaluated every time a new fact is added to the predicate (and,
% sometimes, when facts are deleted as well).  This needs blocking for
% some moments the access to this particular predicate.  This imposes
% an overall burden which is uncceptable. If you want to wait for a
% clause, better choose a different predicate name for that effect.


:- concurrent to_add/2.

six_six(NumThreads, Max):-
        retractall_fact(to_add(_,_)),
        walltime(_),
        launch_goal(put_seeds(Max)),
        launch_them(NumThreads, add_up_numbers(Max), GoalIds),
        join_and_release_goals(GoalIds),
        retract_fact(to_add(Max, Result)),
        walltime(T),
        displaynl(result(Result, T)).


put_seeds(0).
put_seeds(N):-
        N > 0,
        N1 is N - 1,
        WhatToPut is N1 * 2 + 1,
        asserta_fact(to_add(1, WhatToPut)),
        put_seeds(N1).

launch_them(0, _, []).
launch_them(N, Goal, [Id|Ids]):-
        N > 0,
        launch_goal(Goal, Id),
        N1 is N - 1,
        launch_them(N1, Goal, Ids).

add_up_numbers(Max):-
        retract_fact(to_add(Added, Addition)),
        test_if_stop_or_add(Added, Max, Addition).

test_if_stop_or_add(Max, Max, Result):- !,
        assertz_fact(to_add(Max, Result)).   %% Leave tuple
test_if_stop_or_add(Added, _Max, SoFar):-
        ( 
            retract_fact_nb(to_add(Added1, SoFar1)) -> %% Has implicit cut
            %% If we get the second pair we need to add, go ahead
            Added2 is Added + Added1,
            SoFar2 is SoFar + SoFar1,
            assertz_fact(to_add(Added2, SoFar2))
        ;   %% Otherwise, put back the tuple we got
            assertz_fact(to_add(Added, SoFar))
        ),
        fail.
        
 

% Yes, the code below does precisely what I told you not to do: wait
% for clauses of a predicate others were modifying, but anyway coding
% it like this is easier.  I could assign a different predicate to
% every fork, but it would not add up to readability.  Yes, it goes
% slow.


:- concurrent
        action/2,
        start_philosophers/0,
        f1/0, f2/0, f3/0, f4/0, f5/0,
        room/0.

six_seven(Cicles):-
        launch_philosophers(5, Cicles, GoalIds),
        asserta_fact(start_philosophers),
        join_and_release_goals(GoalIds),
        retract_fact_nb(start_philosophers),
        retract_fact_nb(action(What, Who)),
        displaynl(action(What, Who)),
        fail.
 
launch_philosophers(0, _Cicles, []).
launch_philosophers(N, Cicles, [G|Gs]):- 
        N > 0,
        launch_goal(philo(N, Cicles), G),
        N1 is N - 1,
        launch_philosophers(N1, Cicles, Gs).

philo(N, Time):-
        current_fact(start_philosophers),
        philosopher(N, Time).
philosopher(_N, 0).
philosopher(Fork, Time):-
        Time > 0,
        NextTime is Time - 1,
        NextFork is (Fork mod 5) + 1 ,
        fork(Fork, F1),
        fork(NextFork, F2),
        retract_fact(room),
        retract_fact(F1),
        retract_fact(F2),
        assertz_fact(action(eating, Fork)),
        some_time_eating(100),
        assertz_fact(action(thinking, Fork)),
        assertz_fact(F1),
        assertz_fact(F2),
        assertz_fact(room),
        philosopher(Fork, NextTime).

some_time_eating(0).
some_time_eating(N):- 
        N > 0,
        N1 is N - 1,
        some_time_eating(N1).

fork(1,f1).
fork(2,f2).
fork(3,f3).
fork(4,f4).
fork(5,f5).

f1.
f2.
f3.
f4.
f5.

room.
room.
room.
room.
