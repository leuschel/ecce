map(_,[],[]).
map(P,[H|T],[PH|PT]) :-
        Call =.. [P,H,PH],
        call(Call),
        map(P,T,PT).

filter(_,[],[]).
filter(P,[H|T],[H2|FT]) :-
        Call =.. [P,H], 
        call(Call),!,
        inc(H,H2),
        filter(P,T,FT).
filter(P,[_|T],FT) :-
        filter(P,T,FT).

inc(X,Y) :- Y is X+1.

times3(X,Y) :- Y is X*3.
greater100(X) :- X>100.


slice1(L,R) :- map(times3,[],L), filter(greater100,L,R).
slice2(L,R) :- map(times3,[1,2,3],L), filter(greater100,L,R).