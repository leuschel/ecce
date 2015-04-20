producer(0,L) :- !, L = [].
producer(N,L) :- N > 0,
        format('Producing ~d~n', [N]),
        flush_output,
        L = [N|T],
        N1 is N-1,
        producer(N1,T).

consumer(L) :-
        ask(L=[]), !.
consumer(L) :-
        ask(L=[H|T]),
        format('Consumed ~d~n', [H]),
        flush_output,
        consumer(T).

gopc(L) :-
        producer(10,L) &,
        consumer(L).

gocp(L) :-
        consumer(L) &,
        producer(10,L).
