:- module(conc, [main/1, test_speed/0], []).

:- use_module(library(format)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(prolog_sys)).
:- use_module(library(lists)).
% :- use_module(engine(concurrency)).
% :- use_module(engine(io_basic)).
% :- use_module(engine(basiccontrol)).


% Create agents, call several benchmarks.

test_speed:-
        create_agents(10),
        display('Producers and consumers'), 
        nl,
        p195(10000),
        p195(10000),
        p195(10000),
        p195(10000),
        p195(10000),
        p195(10000),
        display('add numbers, two processes'), 
        nl,
        p20(50000),
        p20(50000),
        p20(50000),
        p20(50000),
        p20(50000),
        display('add numbers, several processes'), 
        nl,
        p21(5, 10000),
        p21(5, 10000),
        p21(5, 10000),
        p21(5, 10000),
        p21(5, 10000),
        display('Philosophers'), 
        nl,
        p225(500),
        p225(500),
        p225(500),
        p225(500),
        p225(500).

main([_]):- test_speed.
main([]):-
        message("Creating agents"),
        create_agents(10),
        message("Waiting on an already finished goal several times"),
        p1,
        message("Waiting for completion"),
        p2,
        message("Waiting for a failed goal, releasing"),
        p3,
        message("Failure of local goal"),
        p4,
        message("Deep backtracking, same thread"),
        p5,
        message("Shallow backtracking"),
        p6,
        message("Deep backtracking, separate thread"),
        p7,
        message("Shallow backtracking, separate thread"),
        p8,
        message("Cut in deep backtracking, local thread"),
        p9,
        message("Cut in shallow backtracking, local thread"),
        p10,
        message("Cut in deep backtracking, separate thread"),
        p11,
        message("Cut in shallow backtracking, separate thread"),
        p12,
        message("Several remote backtrackings on a goal"),
        p13,
        message("Several local backtrackings on a goal"),
        p14,
        message("Creating and releasing threads"),
        p15,
        message("Creating threads, automatic release"),
        p16,
        message("Starting a daemon"),
        p18,
        message("Waiting for facts"),
        p185,
        message("Bi-directional communication through the database"),
        p195(10000),
        message("Add the numbers from 1 to N using two threads"),
        p20(20000),
        message("Add the numers from 1 to N using M threads and a pool"),
        p21(5, 1000),
        message("The 5 philosophers"),
        p225(500).

create_agents(0):-
        pause(1).
create_agents(N):-
        eng_call(pause(1), create, create),
        N1 is N - 1,
        create_agents(N1).

message(S):-
        display('------------------------------------------------------'),
        nl,
        format("~s~n", S),
        display('------------------------------------------------------'),
        nl.



:- concurrent finished/0.

create(create).

%% Start, try waiting several times, release.

status:-
	display('-------------------------------------------'),
	nl,
	eng_status.

walltime(T):- statistics(walltime, [_,T]).

p1:-
	create(T),
	eng_call(true, create, T, Id),
	eng_wait(Id),
	eng_wait(Id),
	eng_release(Id).


%% Now wait will actually have to wait for completion.

p2:-
	create(T),
	eng_call(pause(5), create, T, Id),
	eng_wait(Id),
	eng_release(Id).

%% Failure of remote goals do not affect locally.   The wam is 
%% released immediately, but the goal definition remains until we free it.
p3:-
	eng_call(fail, create, create, Id),
	eng_wait(Id),
	eng_release(Id).

%% Failure of local goals does affect
p4:-
	eng_call(fail, create, self, _Id).
p4.


%% Handling of deep backtracking, self thread

p5:-
	eng_call(do_append([1,2,3,4,5]), create, self, Id),
	repeat,
	\+ eng_backtrack(Id, self),
	eng_release(Id).


do_append(L):-
	append(X, Y, L),
	display(append(X, Y, L)),
	nl.


%% Handling of shallow backtracking

p6:-
	eng_call(p(a,_), create, self, Id),
	repeat,
	\+ eng_backtrack(Id, self),
	eng_release(Id).

p(a, _):- display(saw_a), nl.
p(b, _):- display(saw_b), nl.
p(c, _):- display(saw_c), nl.
p(a, _):- display(saw_a), nl.


%% Handling of deep backtracking, new thread

p7:-
	eng_call(do_append_notify, create, self, Id),
	repeat,
	eng_backtrack(Id, create),
        eng_wait(Id),
	retract_fact_nb(finished),
	eng_release(Id).

do_append_notify:- do_append([1,2,3,4,5]).
do_append_notify:- asserta_fact(finished).


%% Handling of shallow backtracking, new thread

p8:-
	eng_call(do_p_notify, create, self, Id),
	repeat,
	eng_backtrack(Id, create),
        eng_wait(Id),
	current_fact_nb(finished),
	eng_release(Id).

do_p_notify:- p(a, _).
do_p_notify:- asserta_fact(finished).


%% Cut in deep backtracking, local

p9:-
	eng_call(do_append([1,2,3,4,5]), create, self, Id),
	eng_cut(Id),
	\+ eng_backtrack(Id, self),
	eng_release(Id).


%% Cut in shallow backtracking, local

p10:-
	eng_call(do_p_notify, create, self, Id),
	eng_cut(Id),
	\+ eng_backtrack(Id, self),
	eng_release(Id).



%% Cut in deep backtracking, remote

p11:-
	eng_call(do_append_notify, create, create, Id),
	eng_wait(Id),
	eng_cut(Id),
	\+ eng_backtrack(Id, self),
	eng_release(Id).


%% Cut in shallow backtracking, remote

p12:-
	eng_call(do_p_notify, create, create, Id),
	eng_wait(Id),
	eng_cut(Id),
	\+ eng_backtrack(Id, self),
	eng_release(Id).


%%%
%%% Stress the implementation...
%%%

%% Several backtrackings on a given goal.  This is remote, so all
%% backtrackings should work.  Also, backtracking and calls should be
%% protected, so no two threads should proceed at the same time.   

p13:-
	eng_call(do_append_notify, create, create, Id),
	eng_wait(Id),
	backtrack_a_lot(100, Id, create),
	eng_release(Id).


%% Several local backtracking calls to a goal

p14:-
	eng_call(do_append_notify, create, create, Id),
	eng_wait(Id),
	backtrack_a_lot(100, Id, self),
	eng_release(Id).

backtrack_a_lot(0, _, _Type).
backtrack_a_lot(N, Id, Type):-
	N > 0,
        backtrack_always_suceeds(Id, Type),
	eng_wait(Id),
	N1 is N - 1,
	backtrack_a_lot(N1, Id, Type).
	
backtrack_always_suceeds(Id, Type):- eng_backtrack(Id, Type), !.
backtrack_always_suceeds(_Id, _Type).


%% Make sure all threads are eventually disposed

num_threads(30).

%% This waits for the threads and explicitly releases them
p15:-
        Pause = 0,
        num_threads(NumThreads),
        walltime(_),
        create_thread_list(NumThreads, List),
        walltime(TCreate),
        format("~d milliseconds needed to start the threads~n", [TCreate]),
        format("Pausing the main thread for ~d seconds~n", [Pause]),
        pause(Pause), % Give some time for the threads to finish their task
        format("Restarting the main thread~n", []),
        walltime(_),
        wait_and_release(List),
        walltime(TJoin),
        format("~d milliseconds waiting for and releasing the threads~n",
                [TJoin]).

create_thread_list(0, []).
create_thread_list(N, [Id|Ids]):-
        N > 0,
        eng_call(do_work, create, create, Id),
        N1 is N - 1,
        create_thread_list(N1, Ids).

wait_and_release([]).
wait_and_release([Id|Ids]):-
        eng_wait(Id),
        eng_release(Id),
        wait_and_release(Ids).


%% This starts the threads with no Id: the memory should be 
%% automatically released. 

do_work:- pause(1).

p16:-
        walltime(_),
        num_threads(NumThreads),
        create_thread_list_noId(NumThreads),
        walltime(TCreate),
        format("~d milliseconds needed to start the threads~n", [TCreate]).

create_thread_list_noId(0).
create_thread_list_noId(N):-
        N > 0,
        eng_call(do_work, create, create),
        N1 is N - 1,
        create_thread_list_noId(N1).


%% Test thread killing

p17:-
        num_threads(NumThreads),
        create_threads_non_stop(NumThreads),
        format("Threads created~n", []),
        pause(1),
        eng_killothers,
        format("Threads killed~n", []).

create_threads_non_stop(0).
create_threads_non_stop(N):-
        N > 0,
        eng_call(work_and_work, create, create),
        N1 is N - 1,
        create_threads_non_stop(N1).

work_and_work:- work_and_work.



%% A possible skeleton of a daemon.  A thread is launched and
%% receives queries.  Every query is just a number, generated by the
%% daemon itself, but which could come from, say, a socket connection.
%% When the query arrives a process is launched.  This process executes
%% (concurrently) the server code, which just prints out its thread
%% number and which query arrived.  Simultaneously, the main daemon is
%% "listening" for more queries, actually implementing a loop.  After
%% a given number of queries is receive, the initial damon dies.

p18:- daemon(10).

daemon(0):- pause(2).
daemon(N):-
        N > 0,
        wait_for_query(Query),
        eng_call(daemon_handler(Query), create, create),
        N1 is N - 1,
        daemon(N1).

daemon_handler(X):- 
        eng_self(Myself),
        display('thread '),
        display(Myself),
        display(' received query '),
        display(X),
        nl.

wait_for_query(Q):-
        pause(1),
        random(Q).



:- concurrent proceed/0.

p185:-
        eng_call(wait_for_proceed, create, create),
        pause(1),
        assertz_fact(proceed),
        assertz_fact(proceed),
        assertz_fact(proceed),
        display(asserted), nl.

wait_for_proceed:-
        display(waiting), nl,
        retract_fact(proceed),
        retract_fact(proceed),
        retract_fact(proceed).


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

number_of_tokens(200).

p19:-
        number_of_tokens(N),
        p195(N).


 %% Perform several small producers/consumers; this is to check that we
 %% are removing the chain links upon failure. 

p192(0).
p192(N):-
        N > 0,
        p195(100),
        N1 is N - 1,
        p192(N1).


p195(NTokens):-
        walltime(_),
        retractall_fact(go_token(_)),
        retractall_fact(return_token(_)),
        eng_call(token_return, create, create, GId1),
        eng_call(token_go, create, create, GId2),
        asserta_fact(go_token(NTokens)),  % This actually triggers execution...
        eng_wait(GId1),
        eng_release(GId1),
        eng_wait(GId2),
        eng_release(GId2),
        walltime(T),
        display('Goals finished in '), 
        display(T),
        display(' ms.'),
        nl.

token_go:-
        retract_fact(go_token(What)),
        Answ is What - 1,
        assertz_fact(return_token(Answ)),
        Answ = 0.

token_return:-
        repeat,
        retract_fact(return_token(What)),
        assertz_fact(go_token(What)),
        What = 0.



% add the numbers from 1 to N.  One worker writes down facts for the
% numbers, the other reads them and adds.  Note the cuts after
% retraction; try with big numbers, it should work.

:- concurrent counter/1.

p20(N):-
        walltime(_),
        retractall_fact(counter(_)),
        eng_call(generate_numbers(N), create, create),
        eng_call(sum_up_numbers, create, create, Id2),
        eng_wait(Id2),
        eng_release(Id2),
        retract_fact(counter(Res)),
        display(result(Res)), nl,
        walltime(T),
        display(time(T)), nl.

p205(0, _N).
p205(Cicles, N):-
        Cicles > 0,
        Cicles1 is Cicles - 1,
        p20(N),
        p205(Cicles1, N).

generate_numbers(0):-
        assertz_fact(counter(0)).
generate_numbers(N):-
        N > 0,
        assertz_fact(counter(N)),
        N1 is N - 1,
        generate_numbers(N1).

sum_up_numbers:-
        retract_fact(counter(CurrNum)), !,
        sum_them_up(CurrNum, 0, Res),
        asserta_fact(counter(Res)).

sum_them_up(0, SoFar, SoFar).
sum_them_up(Current, Added, Res):-
        Current > 0,
        NewAdded is Current + Added,
        retract_fact(counter(NewCurrent)), !,
        sum_them_up(NewCurrent, NewAdded, Res).
        



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

p215(0, _Threads, _Max).
p215(Cicles, Threads, Max):-
        Cicles > 0,
        Cicles1 is Cicles - 1,
        p21(Threads, Max),
        p215(Cicles1, Threads, Max).

p21(NumThreads, Max):-
        retractall_fact(to_add(_,_)),
        walltime(_),
        eng_call(put_seeds(Max), create, create),
        launch_them(NumThreads, add_up_numbers(Max), GoalIds),
        wait_and_release_goals(GoalIds),
        retract_fact(to_add(Max, Result)),
        walltime(T),
        display(result(Result)), nl,
        display(time(T)), nl.

wait_and_release_goals([]).
wait_and_release_goals([G|Gs]):-
        eng_wait(G), 
        eng_release(G),
        wait_and_release_goals(Gs).

put_seeds(0).
put_seeds(N):-
        N > 0,
        N1 is N - 1,
        asserta_fact(to_add(1, N)),
        put_seeds(N1).

launch_them(0, _, []).
launch_them(N, Goal, [Id|Ids]):-
        N > 0,
        eng_call(Goal, create, create, Id),
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

p22(Cicles):-
        walltime(_),
        launch_philosophers(5, Cicles, GoalIds),
        asserta_fact(start_philosophers),
        wait_and_release_goals(GoalIds),
        walltime(T),
        display(time(T)), nl,
        retract_fact_nb(start_philosophers),
        retract_fact_nb(action(What, Who)),
        display(action(What, Who)), nl,
        fail.

p225(Cicles):-
        walltime(_),
        launch_philosophers(5, Cicles, GoalIds),
        asserta_fact(start_philosophers),
        wait_and_release_goals(GoalIds),
        walltime(T),
        display(time(T)), nl,
        retract_fact_nb(start_philosophers),
        retractall_fact(action(_What, _Who)).

launch_philosophers(0, _Cicles, []).
launch_philosophers(N, Cicles, [G|Gs]):- 
        N > 0,
        eng_call(philo(N, Cicles), create, create, G),
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
