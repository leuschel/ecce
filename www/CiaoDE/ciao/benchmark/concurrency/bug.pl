

main:-
        size(S),
        int_list(S, Il),
        copy_list(Il, _S1).

size(50000).

/* Leave choice points */

int_list(N, [Nc|List]):-
        N > 0,
        N1 is N - 1,
        int_list(N1, List),
        N = Nc.
int_list(N, [0]):-
        N < 1.

copy_list([A], [A]).
copy_list([A,B|L], [Ac,B1|List]):-
        copy_list([B|L], [B1|List]),
        A = Ac.
