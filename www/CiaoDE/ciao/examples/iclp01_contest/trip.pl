
trip(S0,S1,End,G0,Gn,Trip):-
    do,
    between(S0,S1,S),
    sched(S,G0,Gn,_E,End,Trip), !,
    retractall(departure(_Gate,_Hour)),
    retractall(arrival(_,_)).

do:-
    trip(H,List),
    proc(List,H),
    fail.
do.

between(S,_S1,S).
between(S0,S1,S):- S is S0+1, S =< S1.

proc([],_H).
proc([G0,G1|Gs],H):-
    do(G0,G1,H,H1),
    proc([G1|Gs],H1).

do(wait(X),_G1,H,H1):- !,
    H1 is H+X.
do(G0,G1,H,H1):-
    asserta(departure(G0,H)),
    asserta(arrival(G1,H)),
    H1 is H+1.

sched(S,G0,Gn,E,End,[G0,Gn]):-
    connection(G0,Gn),
    possible(S,G0,Gn),
    E is S+1,
    E =< End.
sched(S,G0,Gn,E,End,[G0|P]):-
    connection(G0,G1),
    possible(S,G0,G1),
    S1 is S+1,
    sched(S1,G1,Gn,E,End,P).
sched(S,G0,Gn,E,End,[wait(N)|P]):-
    L is End-S+1,
    between(1,L,N),
    S1 is S+N,
    S1 =< End,
    sched(S1,G0,Gn,E,End,P).

possible(S,G0,_G1):-
    departure(G0,S), !,
    fail.
possible(S,_G0,G1):-
    arrival(G1,S), !,
    fail.
possible(_,_,_).

%departure(Gate,Hour).

% actually, the hour of depart from source gate
%arrival(Gate,Hour).
