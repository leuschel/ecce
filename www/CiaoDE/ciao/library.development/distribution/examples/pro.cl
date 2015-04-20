producer(0,T) :- !, T = [].
producer(N,T) :- N > 0,
        format('Producing ~d~n', [N]),
        flush_output,
        T = [N|Ns],
        N1 is N-1,
        producer(N1,Ns).

consumer(T) :-
        wait(T),
        consumer_body(T).

consumer_body([]).
consumer_body([H|T]) :-
        format('Consumed ~d~n', [H]),
        flush_output,
        consumer(T).

gopc(L) :-
        producer(10,L) &,
        consumer(L).

gocp(L) :-
        consumer(L) &,
        producer(10,L).
