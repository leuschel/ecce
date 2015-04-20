:- mode hanoi(i,i,i,i,o).

hanoi(0,A,B,C,[]).
hanoi(s(N),A,B,C,[R1,mv(A,B),R2]) :-
	hanoi(N,A,C,B,R1),
	hanoi(N,C,B,A,R2).

/* to achieve tupling of hanoi:
	hanoi(N,A,B,C), ... , hanoi(N,A,B,C)
			       ^^^^^^^
			       remove (but can affect c.a.s. !)
/* By turning off the whistle and using the msv transfromation
  with functionality ECCE obtains: */
 
/* Specialised Predicates: 
hanoi__1(X1,X2,X3,X4,X5) :- hanoi(X1,X2,X3,X4,X5).
hanoi_conj__2(X1,X2,X3,X4,X5,X6) :-
	hanoi(X1,X2,X3,X4,X5),
	hanoi(X1,X3,X4,X2,X6).
hanoi_conj__3(X1,X2,X3,X4,X5,X6,X7) :-
	hanoi(X1,X2,X3,X4,X5), hanoi(X1,X3,X4,X2,X6), hanoi(X1,X4,X2,X3,X7).
hanoi__4 :- hanoi(0,_RA1,_RA2,_RA3,[]).
*/

/*
hanoi(X1,X2,X3,X4,X5) :- 
    hanoi__1(X1,X2,X3,X4,X5).
hanoi__1(0,X1,X2,X3,[]).
hanoi__1(s(X1),X2,X3,X4,[X5,mv(X2,X3),X6]) :- 
    hanoi_conj__2(X1,X2,X4,X3,X5,X6).
hanoi_conj__2(0,X1,X2,X3,[],[]).
hanoi_conj__2(s(X1),X2,X3,X4,[X5,mv(X2,X3),X6],[X7,mv(X3,X4),X5]) :- 
    hanoi_conj__3(X1,X2,X4,X3,X5,X6,X7).
hanoi_conj__3(0,X1,X2,X3,[],[],[]).
hanoi_conj__3(s(X1),X2,X3,X4,[X5,mv(X2,X3),X6],[X7,mv(X3,X4),X5],[X6,mv(X4,X2),X7]) :- 
    hanoi_conj__3(X1,X2,X4,X3,X5,X6,X7).

*/

:- mode repl(i,i,o).

repl(leaf(N),[H|T],leaf(H)).
repl(node(L,R),Xs,node(R1,R2)) :-
	repl(L,Xs,R1),
	sdrop(L,Xs,DXs),
	repl(R,DXs,R2).

:- mode sdrop(i,i,o).
sdrop(leaf(N),[H|T],T).
sdrop(node(L,R),Xs,Res) :-
	sdrop(L,Xs,R1),
	sdrop(R,R1,Res).

/* to achieve tupling of repl(T,Xs,R1),sdrop(T,Xs,R2):
	repl(L,Xs,R1),
	sdrop(L,Xs,DXs), <--
	repl(R,DXs,R2),     | --- Functionality needed
	sdrop(L,Xs,R1),  <--
	sdrop(R,R1,Res).
*/
