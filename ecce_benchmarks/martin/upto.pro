/* Example from Wadler */
/* Goal from wadler: 

   sum(squares(upto(1,n))) => sumsquaresupto(N,S)
   sumtr(squaretr(xt)) => sumtrsquaretr(XT,S)

*/
upto:square_square(L,SSL) :-
	upto:squares(L,SL),
	upto:squares(SL,SSL).

upto:sumsquaresupto(N,S) :-
   upto:upto(1,N,Ns),
   upto:squares(Ns,SONs),
   upto:sum(SONs,S).

upto:sumtrsquaretr(XT,S) :-
   upto:squaretr(XT,SOXt),
   upto:sumtr(SOXt,S).

upto:upto(M,N,[]) :- M>N.
upto:upto(M,N,[M|Ms]) :- 
	M =< N,
	M1 is M + 1,
	upto:upto(M1,N,Ms).

upto:sum(Ns,S) :- upto:sum1(Ns,0,S).

upto:sum1([],S,S).
upto:sum1([N|Ns],A,S) :-
	A1 is A + N,
	upto:sum1(Ns,A1,S).

upto:square(N,SON) :- SON is N * N.

upto:squares([],[]).
upto:squares([N|Ns],[SON|SONs]) :-
	upto:square(N,SON),
	upto:squares(Ns,SONs).

upto:sumtr(leaf(X),X).
upto:sumtr(branch(Xt,Yt),S) :-
	upto:sumtr(Xt,SX),
	upto:sumtr(Yt,SY),
	S is SX + SY.

upto:squaretr(leaf(X),leaf(SOX)) :- 
	upto:square(X,SOX).
upto:squaretr(branch(Xt,Yt),branch(SOXt,SOYt)) :-
	upto:squaretr(Xt,SOXt),
	upto:squaretr(Yt,SOYt).
