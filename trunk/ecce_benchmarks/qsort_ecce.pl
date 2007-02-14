qsort(As,Bs):- qsort_dl(As,Bs,[]).

qsort_dl([],R,R).
qsort_dl([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsort_dl(L2,R1,R2),
	qsort_dl(L1,R,[X|R1]).


partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- 
	E =< C, 
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E > C, 
	partition(R,C,Left,Right1).


