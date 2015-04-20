/* file matchapp.pro */

matchapp:match(P,S) :- matchapp:append(S1,_,S),matchapp:append(_,P,S1).

matchapp:append([],L,L).
matchapp:append([H|L1],L2,[H|L3]) :- matchapp:append(L1,L2,L3).
