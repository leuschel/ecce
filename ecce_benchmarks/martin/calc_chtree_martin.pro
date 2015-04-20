
/* to use this package do:
  
  ?-load('/Calvin/Programming/ecce/ecce_benchmarks/martin/calc_chtree_martin.pro').  */

/* some sample queries: */
/* local:unfold(transpose:transpose(A,B),C) */
/* local:unfold(transpose:nullrows(A),C) */

/* some sample specialisation tasks: */
/* transpose:transpose([[a,b,c],[1,2,3]],_R) */
/* transpose:transpose([[X1,X2,X3,X4,X5,X6,X7,X8,X9],Xr2,Xr3],Xtrm) */

:- load(local).

:- load(advisor).
:- load(applast).
:- load(doubleapp).
:- load(grammar).
:- load(groundunify).
:- load(match).
:- load(matchapp).
:- load(map).
:- load(maxlength).
:- load(regexp1).
:- load(relative).
:- load(remove).
:- load(remove2).
:- load(rev).
:- load(rev_acc_type).
:- load(rotateprune).
:- load(ssuply).
:- load(transpose).
:- load(upto).

/* replaces the standard calc_chtree file */

calc_chtree(G,_TopGoalVarlist,_UnfHist,Chtree) :-
/*	print(startunf(G,Chtree)),nl, */
	local:unfold(G,Chtree).