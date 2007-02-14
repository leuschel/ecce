/* Example from Wadler */
/* Goal from wadler: 

   sum(squares(upto(1,n))) => sumsquaresupto(N,S)
   sumtr(squaretr(xt)) => sumtrsquaretr(XT,S)

*/
square_square(L,SSL) :-
	squares(L,SL),
	squares(SL,SSL).

sumsquaresupto(N,S) :-
   upto(1,N,Ns),
   squares(Ns,SONs),
   sum(SONs,S).

sumtrsquaretr(XT,S) :-
   squaretr(XT,SOXt),
   sumtr(SOXt,S).

upto(M,N,[]) :- M>N.
upto(M,N,[M|Ms]) :- 
	M =< N,
	M1 is M + 1,
	upto(M1,N,Ms).

sum(Ns,S) :- sum1(Ns,0,S).

sum1([],S,S).
sum1([N|Ns],A,S) :-
	A1 is A + N,
	sum1(Ns,A1,S).

square(N,SON) :- SON is N * N.

squares([],[]).
squares([N|Ns],[SON|SONs]) :-
	square(N,SON),
	squares(Ns,SONs).

sumtr(leaf(X),X).
sumtr(branch(Xt,Yt),S) :-
	sumtr(Xt,SX),
	sumtr(Yt,SY),
	S is SX + SY.

squaretr(leaf(X),leaf(SOX)) :- 
	square(X,SOX).
squaretr(branch(Xt,Yt),branch(SOXt,SOYt)) :-
	squaretr(Xt,SOXt),
	squaretr(Yt,SOYt).
