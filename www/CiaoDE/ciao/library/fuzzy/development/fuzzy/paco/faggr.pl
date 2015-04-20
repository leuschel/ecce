:- module(faggr,[dlukalist/2,dprodlist/2,inject/3,
	         lukalist/2,minim/2,maxim/2,prodlist/2],
		[clpr,hiord]).

:- meta_predicate inject(?,pred(3),?).

inject([],_,_).
inject([T],_,T).
inject([X,Y|Rest],P,T):-
	P(X,Y,T0),
	inject([T0|Rest],P,T).

:- meta_predicate =>(pred(3),goal,goal,?).

=>(Formula,X,Y,M):- 
	functor(X,_,Ax),
	arg(Ax,X,Mx),
	functor(Y,_,Ay),
	arg(Ay,Y,My),
	call(X),
	call(Y),
%% 	functor(For,Formula,3),
%% 	arg(1,For,Mx),
%% 	arg(2,For,My),
%% 	arg(3,For,M),
%% 	call(For).
	call(Formula,Mx,My,M).
%	M .=. Mx * My.


lukalist([],_).
lukalist([X],X).
lukalist([X,Y|Rest],Min):-
	luka(X,Y,M),
	lukalist([M|Rest],Min).

luka(X,Y,M):- Z1.=.0,Z2.=. X + Y  - 1,max(Z1,Z2,M).

minim([],_).
minim([X],X).
minim([X,Y|Rest],Min):-
	min(X,Y,M),
	minim([M|Rest],Min).

min(X,Y,Z):- X .=<. Y , Z .=. X.
min(X,Y,Z):- X .>. Y, Z .=. Y .

maxim([],_).
maxim([X],X).
maxim([X,Y|Rest],Max):-
	max(X,Y,M),
	maxim([M|Rest],Max).

max(X,Y,Z):- X .>=. Y, Z .=. X.
max(X,Y,Z):- Y .>. X, Z .=. Y.

prodlist([],_).
prodlist([X],X).
prodlist([X,Y|Rest],Prod):-
	prod(X,Y,M),
	prodlist([M|Rest],Prod).

prod(X,Y,M):- M .=. X * Y.

dprodlist([],_).
dprodlist([X],X).
dprodlist([X,Y|Rest],Prod):-
	dprod(X,Y,M),
	dprodlist([M|Rest],Prod).

dprod(X,Y,M):- M .=. X + Y - (X * Y).

dlukalist([],_).
dlukalist([X],X).
dlukalist([X,Y|Rest],L):-
	dluka(X,Y,M),
	dlukalist([M|Rest],L).

dluka(X,Y,M):- Z1.=.1,Z2.=. X + Y, min(Z1,Z2,M).
