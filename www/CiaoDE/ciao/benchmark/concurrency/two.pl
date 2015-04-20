:- module(two,
        [
            two_one/0,
            two_two/0,
            two_three/0,
            two_four/0
        ], []).

:- use_module(sleep, [sleep_and_print/2]).
:- use_module(displaynl, [displaynl/1]).


% Second module: waiting for threads.

% 2.1: launch a thread and wait for its completion.  Goals which are
% started with an Id are not released upon completion: they stay in
% waiting state, and have to be explicitly freed.  this, which for now
% seems to be a burden, is really useful, as we will see later.


two_one:-
        launch_goal(sleep_and_print(2, finished_sleeping(2)), GoalId),
        displaynl(goal_launched(GoalId)),
        join_and_release_goal(GoalId),
        displaynl(goal_joined).


% 2.2: launch two threads and wait for their completion.  join_goal
% just waits for its argument to finish.  Therefore, actions as
% "wait for the first of these to goals to complete" or "fail if the
% goal has not been completed" cannot be expressed.

two_two:-
        launch_goal(sleep_and_print(4, finished_sleeping(4)), Goal_1_Id),
        displaynl(thread_1_launched),
        launch_goal(sleep_and_print(2, finished_sleeping(2)), Goal_2_Id),
        displaynl(thread_2_launched),
        join_and_release_goal(Goal_1_Id),
        join_and_release_goal(Goal_2_Id).
 

% 2.3: launch two threads, wait for the first one.

two_three:-
        launch_goal(sleep_and_print(4, finished_sleeping(4))),
        displaynl(thread_1_launched),
        launch_goal(sleep_and_print(2, finished_sleeping(2)), Goal_2_Id),
        displaynl(thread_2_launched),
        join_and_release_goal(Goal_2_Id).


% 2.4: launch a thread, kill it.

two_four:-
        launch_goal(sleep_and_print(4, finished_sleeping(4)), Goal_1_Id),
        displaynl(thread_1_launched),
        sleep_and_print(1, about_to_kill_thread(Goal_1_Id)),
        kill_goal(Goal_1_Id).
