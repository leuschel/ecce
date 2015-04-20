 %% and_prolog.pl -- &-Prolog simulator
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro
 %% Created On      : Thu Jan 15 19:31:22 1998
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Fri Jan 16 17:59:43 1998
 %% Update Count    : 67
 %% Status          : Unknown, Use with caution!


:- concurrent pending/2.                    %% pending(Goal, Id)
:- concurrent finished/2.                   %% finished(Goal, Id)


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% This waits for goals, executes them, and put the bindings back.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scheduler:-
        write('Scheduler starts...'), nl,
        get_goal(G, Id),
        write('scheduler got goal'(G,Id)), nl,
        launch_goal(scheduler),
        execute_goal(G),
        put_result(G, Id).

get_goal(Goal, Id):-
        WaitFor = pending(Goal, Id),
        current_fact(WaitFor), %% Not really correct -- need atomic retract!
        retract_fact(WaitFor).

execute_goal(Goal):-
        call(Goal).

put_result(Goal, Id):-
        assertz_fact(finished(Goal, Id)).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% The "execute" part: calls to this go in user code.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(950,xfx,[(&)]).

'&'(GoalA, GoalB) :-                         %% Put A, execute here B.
        put_goal(GoalA, Id),
        call(GoalB),
        get_result(GoalA, Id).

put_goal(Goal, Id):-
        new_id(Id),
        assertz_fact(pending(Goal, Id)).

get_result(Goal, Id):-
        WaitFor = finished(Goal, Id),
        current_fact(WaitFor),
        retract_fact(WaitFor).           %% Ditto
