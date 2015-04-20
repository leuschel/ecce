makelist(0, _, L) :- !, L=[].
makelist(N, X, [Y|L]) :- N>0, N1 is N-1, makelist(N1, X, L), X=Y.

 %% Replacing the second rule above from the following, the complexity
 %% of slist(N) changes from O(N^2) to O(N), and also the runtime
 %% decreases a lot. 

%% makelist(N, X, [X|L]) :- N>0, N1 is N-1, makelist(N1, X, L).

sumlist([],    In, Out) :- In=Out.
sumlist([X|L], In, Out) :- Mid is X+In, sumlist(L, Mid, Out).

slist(N) :-
        makelist(N, X, L),
        X=1,
        statistics(runtime,_),
%%        ctime(T1),
        sumlist(L, 0, S),
        statistics(runtime,[_,Time]),
%%        ctime(T2),
%%        Time is T2 - T1,
        write(Time), nl.
