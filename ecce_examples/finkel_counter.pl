r(T,S) :- reach(s(0),0,0,0,0,0,0,T,S).

reach(P1,P2,P3,P4,P5,P6,P7,[],S) :-
  S = [P1,P2,P3,P4,P5,P6,P7].
reach(s(P1),P2,P3,P4,P5,P6,P7,[t1|T],R) :- reach(P1,s(P2),P3,P4,P5,P6,P7,T,R).
reach(P1,s(P2),P3,P4,P5,P6,P7,[t2|T],R) :- reach(P1,P2,s(P3),P4,P5,P6,P7,T,R).
reach(P1,P2,s(P3),P4,P5,P6,P7,[t3|T],R) :- reach(P1,P2,P3,s(P4),P5,P6,P7,T,R).
reach(P1,P2,P3,s(P4),P5,P6,P7,[t4|T],R) :- reach(P1,P2,s(P3),P4,s(P5),P6,P7,T,R).
reach(s(P1),P2,P3,P4,P5,P6,P7,[t5|T],R) :- reach(P1,P2,P3,P4,P5,s(P6),P7,T,R).
reach(P1,P2,P3,P4,P5,s(P6),P7,[t6|T],R) :- reach(P1,P2,P3,s(P4),s(P5),P6,P7,T,R).
reach(s(P1),P2,P3,P4,P5,P6,P7,[t7|T],R) :- reach(P1,P2,P3,P4,P5,P6,s(P7),T,R).
reach(P1,P2,P3,P4,P5,P6,s(P7),[t8|T],R) :- reach(P1,s(P2),P3,P4,s(P5),P6,P7,T,R).
