fix_list([X|Xs],Res) :-
	count_number(X,Number),
	write(Number),nl,
	redo_list(Number,0,[X|Xs],Res).


redo_list(N,N,_,[]).
redo_list(Max,Now,In,[R|Rs]) :-
	New is Now + 1,
	redo_list0(Now,In,R),
	write(R),nl,
	redo_list(Max,New,In,Rs).

redo_list0(_,[],[]).
redo_list0(Nr,[X|Rest],[Val|More]) :-
	get_number(Nr,X,Val),
	redo_list0(Nr,Rest,More).


get_number(Nr,List,X) :-
	get_number(Nr,0,List,X).

get_number(Nr,Nr,[X|_],X).
get_number(Nr,Now,[_|Rest],Result) :-
	New is Now + 1,
	get_number(Nr,New,Rest,Result).


count_number([],0).
count_number([_|Xs],N) :-
	count_number(Xs,New),
	N is New + 1.
