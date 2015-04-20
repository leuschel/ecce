
length(List,Length):- length1(List,0,Length).

length1([],L,L).
length1([H|List],Acc,Length):- Acc1 is Acc+1, length1(List,Acc1,Length).
