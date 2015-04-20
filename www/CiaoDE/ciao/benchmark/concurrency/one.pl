:- module(one,
        [
            one_one/0,
            one_two/0,
            one_three/0,
            one_four/0,
            one_five/0
        ], []).

:- use_module(sleep, [sleep_and_print/2]).
:- use_module(displaynl, [displaynl/1]).


% First module: launching threads.

% 1.1: launch a thread that sleeps for two seconds and then prints a message.
% The parent thread will issue a message after the child thread starts.
% Then it will wait for 4 seconds, and issue another message.
%%
% N.B.: in the first examples we will rely on timing predicates to
% ensure correct rendezvous.  This is no, of course, the way to do do
% it, but it is usually useful in small examples.  We will use proper
% synchronization primitives in due time.



one_one:-
        eng_call(sleep_and_print(2, finished_sleeping(2)), create, create),
        displaynl(first_thread_launched),
        sleep_and_print(4, finished_sleeping(4)).
        

% 1.2: if the longest thread is launched first, then the shortest one
% (which is now the main application thread) will finish first.  The
% longest thread simply remains executing when the control has
% returned to the shell.

one_two:- 
        eng_call(sleep_and_print(4, finished_sleeping(4)), create, create),
        displaynl(first_thread_launched),
        sleep_and_print(2, finished_sleeping(2)).


% 1.3: When both threads are detached from the main thread, control is
% returned to the toplevel, and messages are printed independently.

one_three:- 
        eng_call(sleep_and_print(4, finished_sleeping(4)), create, create),
        displaynl(first_thread_launched), 
        eng_call(sleep_and_print(2, finished_sleeping(2)), create, create),
        displaynl(second_thread_launched).


% 1.4: Recursive creation of threads (which do barely anything, but
% they are there, anyway...).  

one_four:- 
        create(10),
        goal_status.

create(0).
create(N):-
        eng_call(sleep_and_print(N, finished(N)), create, create),
        displaynl(created(N)),
        N1 is N - 1,
        create(N1).


% 1.5: What happens to variables in threads: the eng_call/[1,2,3]
% family of calls copies the goal(s) to be executed to the space of
% the newly created thread, including the variables, which become
% fresh copies.  This means that those variables cannot be used to
% communicate threads.  We will see later how this can be
% accomplished.

one_five:-
        eng_call(sleep_and_print(3, variable_in_separate_thread(Variable))),
        sleep_and_print(1, proceeding_in_main_thread),
        Variable = a,                                                
        displaynl(variable_in_main_thread(Variable)),
        sleep_and_print(3, finished_main_thread).
