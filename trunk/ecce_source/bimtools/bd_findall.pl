/* ------------ */
/* bd_findall/3 */
/* ------------ */

init_bd_findall.
bd_findall(X,G,L) :- findall(X,G,L).
critical_bd_findall_nesting :- fail.

/* findall version by Bart Demoen, giving an error message when */
/* the nesting of findall's overflows (limit 256) */

/* BELOW: doesn't work yet under SICSTUS ?! */

/*
init_bd_findall :-
	initbagof.

bd_findall(X,G,L) :-
	upbagof ,
	(findall(X,G,L) -> downbagof ; downbagof , fail) .



upbagof :-
	recorded(bagof,nesting,N) ,
	M is N + 1 ,
	rerecord(bagof,nesting,M) ,
	(M > 255 -> write('findall nesting overflow\n') ; true) .

downbagof :-
	recorded(bagof,nesting,N) ,
	M is N - 1 ,
	rerecord(bagof,nesting,M) .

initbagof :- rerecord(bagof,nesting,0) .


critical_bd_findall_nesting :-
	recorded(bagof,nesting,N),
	N > 254.
*/