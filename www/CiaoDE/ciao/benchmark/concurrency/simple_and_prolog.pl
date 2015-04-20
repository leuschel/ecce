 %% simple_and_prolog.pl -- Simple &-Prolog emulator
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro
 %% Created On      : Fri Jan 16 17:28:49 1998
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Tue Feb  3 00:31:17 1998
 %% Update Count    : 18
 %% Status          : Unknown, Use with caution!


:- concurrent pending/2.                    %% pending(Goal, Id)
:- concurrent finished/2.                   %% finished(Goal, Id)


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% This restricted version does not remove goals or invoke another
 %% scheduler; it is based on a backtracking procedure.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic simple_scheduler_running/0.

s_scheduler:-
        write('Simple scheduler starts...'), nl,
        %% open_predicate(pending(_,_)),
        current_fact(pending(Goal, Id)),
        write('Simple scheduler got goal'(Goal,Id)), nl,
        wrapped_call(Goal, Result),
        assertz_fact(finished(Result, Id)),
        write('Simple scheduler put result'(Result,Id)), nl,
        fail.

wrapped_call(Goal, Result):-
        call(Goal),
        Result = Goal.
wrapped_call(_Goal, -1).

:- op(950,xfy,[(&)]).

'&'(GoalA, (GoalB & GoalC)):- !,
        new_id(Id),
        assertz_fact(pending(GoalA, Id)),
        GoalB & GoalC,
        current_fact(finished(GoalA, Id)).
'&'(GoalA, GoalB) :-  %% Launch scheduler if needed
        (
            current_fact(simple_scheduler_running) ->
            true
        ;
            launch_goal(s_scheduler),
            asserta_fact(simple_scheduler_running)
        ),
        new_id(Id),
        simple_and(GoalA, GoalB, Id).
simple_and(GoalA, GoalB, Id):-
        write('simple_and received'(GoalA, GoalB)), nl,
        assertz_fact(pending(GoalA, Id)),
        call(GoalB),
        '$metachoice'(Choice),
        write('simple_and executed'(GoalB)), nl,
        current_fact(finished(Goal, Id)),
        (
            Goal = -1 ->
            '$metacut'(Choice),
            fail
        ;
            Goal = GoalA
        ).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Counter management
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic counter/1.

counter(0).                              %% Initial

new_id(Counter):-
        lock_atom(counter),
        retract_fact(counter(Counter)),
        NewCounter is Counter + 1,
        asserta_fact(counter(NewCounter)),
        unlock_atom(counter).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Test cases.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


a(1).
a(2).

b(a).
b(b).

c(*).
c(+).

p(X, Y):- a(X) & b(Y).


q1(X, Y, Z):- p(X,Y) & c(Z).
