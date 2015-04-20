/* file: liftsolve.pro */


/* --------------------- */
/* solve(GrRules,NgGoal) */
/* --------------------- */


solve(GrRules,[]).
solve(GrRules,[NgH|NgT]) :-
	non_ground_member(term(clause,[NgH|NgBody]),GrRules),
	   /* print(matching_clause(NgH,NgBody)),nl, */
	solve(GrRules,NgBody),
	solve(GrRules,NgT).

/* -------------------------------------- */
/* non_ground_member(NgExpr,GrListOfExpr) */
/* -------------------------------------- */

non_ground_member(NgX,[GrH|GrT]) :-
	make_non_ground(GrH,NgX).
non_ground_member(NgX,[GrH|GrT]) :-
	non_ground_member(NgX,GrT).


/* --------------------------------------------------------- */
/* make_non_ground(GroundRepOfExpr,NonGroundRepOfExpr) */
/* --------------------------------------------------------- */

/* ex. ?-make_non_ground(pos(term(f,[var(1),var(2),var(1)])),X). */

make_non_ground(G,NG) :-
	mkng(G,NG,[],Sub).


mkng(var(N),X,[],[sub(N,X)]).
mkng(var(N),X,[sub(N,X)|T],[sub(N,X)|T]).
mkng(var(N),X,[sub(M,Y)|T],[sub(M,Y)|T1]) :-
	N \= M,
	mkng(var(N),X,T,T1).
mkng(term(F,Args),term(F,IArgs),InSub,OutSub) :-
	l_mkng(Args,IArgs,InSub,OutSub).
l_mkng([],[],Sub,Sub).
l_mkng([H|T],[IH|IT],InSub,OutSub) :-
	mkng(H,IH,InSub,IntSub),
	l_mkng(T,IT,IntSub,OutSub).


/* ------- */
/* member */
/* ------- */

member(X,[X|T]).
member(X,[Y|T]) :-
	member(X,T).

/* ------- */
/* append */
/* ------- */

append([],L,L).
append([H|T],M,[H|T2]) :-
	append(T,M,T2).

