:- module(stat, [
	lsumstat/3,
	lwsumstat/3,
	lsumstat_aux/5,
	lsumstat/4,
	lwsumstat/4,
	lsumstat_aux/7,
	laverage/2,
	lmean/2,
	lwmean/2,
	lgeometric_mean/2,
	lwgeometric_mean/2,
	lvariance/2,
	lwvariance/2,
	variance/4,
	lsumstat2/7,
	lsumstat2_aux/13,
	covariance/5,
	lcovariance2/2,
	regression/9,
	lregression/4,
	mlregression/2,
	genregression/4,
	ttransmul/2
], [assertions,regtypes]).

:- use_module(library(lists)).
:- use_module(library('math/mmatrix'), [rmultiply/3, madd/3,
   mtriang_to_rect/2, msolve/3, vmultiply/3, rmultiply/3, vadd/3]).

:- comment(title,"Statistical Utilities.").

:- comment(author,"Edison Mera").

:- comment(module,"A complete library of statistical utilities.  This
   library will be used in conjunction with the profiler tools.").

:- push_prolog_flag(multi_arity_warnings, off).


:- true pred lsumstat(List,Number,Sum): list(number) * term * term =>
   list(number) * number * number # "Unifies @var{Number} with the
   length of the list and @var{Sum} with the total sum of the numbers
   in the list @var{List}.".

lsumstat(X,N,S):-
	lsumstat_aux(X,0,0,N,S).

:- true pred lsumstat_aux(List,Number0,Sum0,Number,Sum): list(number)
   * number * number * term * term => list(number) * number * number *
   number * number # "Similar to @pred{lsumstat/3}, but takes
   @var{Number0} and @var{Sum0} as the initial values of @var{Number}
   and @var{Sum}.".

lsumstat_aux([],N,S,N,S).
lsumstat_aux([X|Xs],N0,S0,N,S):-
	N1 is N0+1,
	S1 is S0+X,
	lsumstat_aux(Xs,N1,S1,N,S).

lwsumstat(X,N,S) :-
	lwsumstat_aux(X,0,0,N,S).

lwsumstat_aux([],N,S,N,S).
lwsumstat_aux([(W,X)|Xs],N0,S0,N,S) :-
	N1 is N0+W,
	S1 is S0+W*X,
	lwsumstat_aux(Xs,N1,S1,N,S).

:- true pred lsumstat(List,Number,Sum1,Sum2): list(number) * term *
   term * term => list(number) * number * number * number # "Unifies
   @var{Number} with the length of the list, @var{Sum1} with the total
   sum of the numbers in the list and @var{Sum2} with the total sum of
   the squarers of the numbers in the list @var{List} respectively.".

lsumstat(X,N,S1,S2):-
	lsumstat_aux(X,0,0,0,N,S1,S2).

:- true pred lsumstat_aux(List,Number0,Sum10,Sum20,Number,Sum1,Sum2):
   list(number) * number * number * number * term * term * term =>
   list(number) * number * number * number * number * number * number
   # "Similar to @pred{lsumstat/4}, but takes @var{Number0},
   @var{Sum10} and @var{Sum20} as the initial values of @var{Number},
   @var{Sum1} and @var{Sum2} respectively.".

lsumstat_aux([],N,S1,S2,N,S1,S2).
lsumstat_aux([X|Xs],N1,S11,S21,N,S1,S2) :-
        N2 is N1+1,
	S12 is S11+X,
	S22 is S21+X*X,
	lsumstat_aux(Xs,N2,S12,S22,N,S1,S2).

:- true pred lwsumstat(List,Number,Sum1,Sum2): list(pair) * term *
   term * term => list(pair) * number * number * number # "Similar to
   lsumstat/4 for a list of weighted values.".

lwsumstat(X,N,S1,S2):-
	lwsumstat_aux(X,0,0,0,N,S1,S2).

lwsumstat_aux([],N,S1,S2,N,S1,S2).
lwsumstat_aux([(W,X)|Xs],N1,S11,S21,N,S1,S2) :-
        N2 is N1+W,
	S12 is S11+W*X,
	S22 is S21+W*(X*X),
	lwsumstat_aux(Xs,N2,S12,S22,N,S1,S2).

llog([],[]).
llog([X|L],[Y|R]) :-
	Y is log(X),
	llog(L,R).

llogw([],[]).
llogw([(W,X)|L],[(W,Y)|R]) :-
	Y is log(X),
	llogw(L,R).

:- pop_prolog_flag(multi_arity_warnings).

:- true pred lmean(List,Mean): list(number) * term =>
   list(number) * number # "Unifies @var{Mean} with the mean of
   the list @var{List}.".

lmean(L,A) :-
	lsumstat(L,N,S),
	A is S/N.

%this is for compatibility and must be deleted in the future
laverage(L,A) :- lmean(L,A).


:- true pred lgeometric_mean(List,Mean): list(number) * term =>
   list(number) * number # "Unifies @var{Mean} with the geometric
   mean of the list @var{List}.".

lgeometric_mean(L,A) :-
	llog(L,L1),
	lsumstat(L1,N,S),
	A is exp(S/N).

:- true pred lwmean(List,Mean): list(pair) * term =>
   list(pair) * number # "Unifies @var{Mean} with the mean of
   the list @var{List}.".

lwmean(L,A) :-
	lwsumstat(L,N,S),
	A is S/N.

:- true pred lwgeometric_mean(List,Mean): list(pair) * term =>
   list(pair) * number # "Unifies @var{Mean} with the geometric
   mean of the weighted values @var{List}.".

lwgeometric_mean(L,A) :-
	llogw(L,L1),
	lwsumstat(L1,N,S),
	A is exp(S/N).

:- true pred lvariance(List,Variance): list(number) * term =>
   list(number) * number # "Unifies @var{Variance} with the variance
   of the list of weighted values @var{List}.".

lvariance(L,V) :-
	lsumstat(L,N,S1,S2),
	variance(N,S1,S2,V).

:- true pred lwvariance(List,Variance): list(pair) * term =>
   list(pair) * number # "Unifies @var{Variance} with the variance of
   the list of weighted values @var{List}.".

lwvariance(L,V) :-
	lwsumstat(L,N,S1,S2),
	variance(N,S1,S2,V).

:- true pred variance(N,Sum1,Sum2,Variance): number * number * number *
   term => number * number * number * number # "Unifies @var{Variance} with the
   variance, being @var{N} the number of data, @var{Sum1} the sum of
   the data and @var{Sum2} the sum of the square of the data".

variance(N,S1,S2,V):-
	V is (S2 - (S1*S1)/N)/N.

:- regtype pair(P).

pair(P) :-
	P=(X,Y),
	number(X),
	number(Y).

:- true pred lsumstat2(List,N,Sx,Sy,Sx2,Sxy,Sy2): list(pair) * term *
   term * term * term * term * term => list(pair) * number * number *
   number * number * number * number # "Unifies N with the length, Sx
   with the sum of the independent variable, Sy with the sum of the
   dependent variable, Sx2 with the sum of squares of the independent
   variable, Sxy with the sum of the product between the independent
   and the dependent variables, and Sy2 with the sum of squares of the
   dependent variable.".

lsumstat2(L,N,Sx,Sy,Sx2,Sxy,Sy2):-
	lsumstat2_aux(L,0,0,0,0,0,0,N,Sx,Sy,Sx2,Sxy,Sy2).

:- true pred lsumstat2_aux(List, N0, Sx0, Sy0, Sx20, Sxy0, Sy20, N,
   Sx, Sy, Sx2, Sxy, Sy2) : list(pair) * number * number * number *
   number * number * number * term * term * term * term * term * term
   => list(pair) * number * number * number * number * number *
   number * number * number * number * number * number * number #
   "Same as @pred{lsumstat/7} but taken initial values for the
   returned data.".

lsumstat2_aux([],N,Sx,Sy,Sx2,Sxy,Sy2,N,Sx,Sy,Sx2,Sxy,Sy2).
lsumstat2_aux([L|Ls],N1,Sx1,Sy1,Sx21,Sxy1,Sy21,N,Sx,Sy,Sx2,Sxy,Sy2):-
	(X,Y)=L,
        N_2 is 1+N1,
	Sx_2 is X+Sx1,
	Sy_2 is Y+Sy1,
	Sx2_2 is X*X+Sx21,
	Sxy_2 is X*Y+Sxy1,
	Sy2_2 is Y*Y+Sy21,
	lsumstat2_aux(Ls,N_2,Sx_2,Sy_2,Sx2_2,Sxy_2,Sy2_2,N,Sx,Sy,Sx2,Sxy,Sy2).

:- true pred covariance(N,Sx,Sy,Sxy,C): number * number * number *
   number * term => number * number * number * number * number #
   "Unifies @var{C} with the covariance.".

covariance(N,Sx,Sy,Sxy,C):-
	C is (Sxy - (Sx*Sy)/N)/N.

:- true pred lcovariance2(List,C): list(pair) * term => list(pair) *
   number # "Unifies @var{C} with the covariance of the pair list
   @var{L}.".

lcovariance2(L,C) :-
	lsumstat2(L,Sx,Sy,_Sx2,Sxy,_Sy2,N),
	covariance(N,Sx,Sy,Sxy,C).

:- true pred regression(N, Sx, Sy, Sx2, Sxy, Sy2, B0, B1, R): number *
   number * number * number * number * number * term * term * term =>
   number * number * number * number * number * number * number *
   number * number # "Unifies B0, B1 with the parameters of the linear
   regression, and R with the correlation rate.".

regression(N,Sx,Sy,Sx2,Sxy,Sy2,B0,B1,R):-
	covariance(N,Sx,Sy,Sxy,C),
	variance(N,Sx,Sx2,Vx),
	B1 is C/Vx,
	B0 is (Sy-B1*Sx)/N,
	variance(N,Sy,Sy2,Vy),
	R is C/sqrt(Vx*Vy).

:- true pred lregression(L, B0, B1, R): list(pair) * term * term *
   term => list(pair) * number * number * number # "Given the point
   list @var{L}, Unifies B0, B1 with the parameters of the linear
   regression, and R with the correlation rate.  Remember that the
   linear model is as follows: Y^ = B0 + B1 * X, where Y^ is the
   expected value of the dependent variable Y.".

lregression(L, B0, B1, R) :-
	lsumstat2(L, N, Sx, Sy, Sx2, Sxy, Sy2),
	regression(N, Sx, Sy, Sx2, Sxy, Sy2, B0, B1, R).

%triangular product combinations.
tcom([],[]).
tcom([X|Xs],[C2|C1]) :-
	rmultiply(X,[X|Xs],C2),
	tcom(Xs,C1).

:- true pred ttransmul(X,S): list(list(number)) * term => list(list(number)) * list(list(number)) #
   "Unifies @var{S} with the matrix product @var{X}' * @var{X}, where @var{X} is triangular.".

ttransmul([],[]).
ttransmul([L|Ls],S) :-
	ttransmul(Ls,S2),
%	(X,Y)=L,
%	tcom([Y|X],C),
	tcom(L,C),
	madd(C,S2,S).

:- true pred mlregression(L,B): list * term => list * list #
   "Multivariant linear regression.  @var{L} contains the data and @var{B} the
   parameter list.".

mlregression(L,B):-
	ttransmul(L,[[_|Y]|X]),
	mtriang_to_rect(X,X1),
	msolve(X1,Y,B).

:- true pred genregression(Data, Pattern, Vars, B): list * term * term
   * term => list * term * term * list # "Generic linear regression.
   @var{Data} contains the data, @var{Pattern} contains a list of
   arithmetic expressions that represent the components of the linear
   model. @var{Vars} contains the list of variables used in the
   expression Pattern.  For example, if the model is:

   Y = B00+B10*X1+B01*X2+B11*X1*X2

   To use @var{genregression/4} with a given data list we must write:

?- genregression([[3,2,2],[5,1,2],[7,1,1],[9,2,1]], [1,X1,X2,X1*X2],[X1,X2],B).

B = [2.99999999999959,6.000000000000256,2.000000000000256,-4.00000000000016] ? 

yes

The exact model is:

   Y=3+6*X1+2*X2-4*X1*X2

".

genregression(Data, Pattern, Vars, B) :-
	applydatatovar(Data, Pattern, Vars, L),
	mlregression(L, B).

:- data genregression_elem/1.

applydatatovar([], _, _, []).
applydatatovar([[Y|D]|Ds], Pattern, Vars, [[Y|L]|Ls]) :-
	(   Vars = D,
	    evalpattern(Pattern, L),
	    assertz_fact(genregression_elem(L)),
	    fail
	)
 ;
	current_fact(genregression_elem(L)),
	retract_fact(genregression_elem(L)),
	applydatatovar(Ds, Pattern, Vars, Ls).

evalpattern([], []).
evalpattern([P|Ps],[L|Ls]) :-
	L is P,
	evalpattern(Ps, Ls).

% now we will define the equation that find the solution in (X'X)*B = (X'*Y)

tmultiply([],[],[]).
tmultiply([X|Xs],[B|Bs],[Y|Ys]) :-
	vmultiply(X,[B|Bs],Y),
	tmultiply(Xs,Bs,Ys1),
	rmultiply(B,X,Ys2),
	vadd(Ys1,Ys2,Ys).

% ?- main.
% [9.053849885836955,0.5202789721838336,0.2397463704130682]

% main :-
% 	mlregression([[64,1,58,111],[78,1,84,131],[83,1,78,158],[88,1,81,147],
% 	[89,1,82,121],[99,1,102,165],[101,1,85,174],[102,1,102,169]],A),
% 	write(A).
