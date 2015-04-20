/* file matchapp.pro */

match(P,S) :- append(S1,_,S),append(_,P,S1).

append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).
