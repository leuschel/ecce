:- module(makeiff,_).

% Author: Bart Demoen K.U.Leuven
% Date: March 23 1993
% Most recent documented change: March 25 1993
% Purpose: transform a flat program in its 'iff' form, i.e.
% 		all unifications are replaced by appropriate calls to iff
%
%% Example: ?- makeiff((a(Z) :- Z = f(g(X),Y) , T = [1] , b(T) , true),N) .
%		N = a(Z) :- iff(Z,X,Y) , iff(T) , b(T) , true
%
%	spurious calls to true/0 appear at the end of the clauses

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(dec10_io)).

:- use_module('../bimtools').

makeiff((H :- B) , (H :- NewB)) :- ! ,
			makeiffbody(B,NewB,true) .
makeiff(H,(H :- true)) .

makeiffbody((G1,G2),B,TB) :- ! , makeiffbody(G1,B,TT) , makeiffbody(G2,TT,TB) .
makeiffbody(Goal,B,TB) :- treatedbuiltin(Goal,B,TB) , ! .
makeiffbody(Goal,(Goal,TB),TB) :-
				(builtin(Goal) -> told , write(builtin_not_treated(Goal)) , fail
				;
				true
				) .

builtin(_G) :- fail. /* is_built_in_literal(_G) */

treatedbuiltin(fail,(fail_iff,TB),TB) :- ! .
treatedbuiltin(true,TB,TB) :- ! .
treatedbuiltin(current_op(X,Y,Z),(iff(X),iff(Y),iff(Z),T),T) :- ! .
treatedbuiltin(Var = Term,B,TB) :- makeiffgoal(Var,Term,B,TB) , ! .
treatedbuiltin(_ = _,(fail_iff,TB),TB) :- ! .

treatedbuiltin(X is Y,B,TB) :- ! , makeiffgoal(true,(X+Y),B,TB) .
treatedbuiltin(X =:= Y,B,TB) :- ! , makeiffgoal(true,(X+Y),B,TB) .
treatedbuiltin('<>'(X,Y),B,TB) :- ! , makeiffgoal(true,(X+Y),B,TB) .
treatedbuiltin(X < Y,B,TB) :- ! , makeiffgoal(true,(X+Y),B,TB) .
treatedbuiltin(X > Y,B,TB) :- ! , makeiffgoal(true,(X+Y),B,TB) .
treatedbuiltin(X =< Y,B,TB) :- ! , makeiffgoal(true,(X+Y),B,TB) .
treatedbuiltin(X >= Y,B,TB) :- ! , makeiffgoal(true,(X+Y),B,TB) .

treatedbuiltin(write(_X),Tail,Tail) :- ! .

treatedbuiltin(integer(X),(iff(X),Tail),Tail) :- ! .

treatedbuiltin(get0(X),(iff(X),Tail),Tail) :- ! .

treatedbuiltin(display(_X),Tail,Tail) :- ! .

treatedbuiltin(ground(X),B,TB) :- ! , makeiffgoal(true,X,B,TB) .

treatedbuiltin(compound(_X),Tail,Tail) :- ! .

treatedbuiltin(var(_X),Tail,Tail) :- ! .

treatedbuiltin(functor(_X,Y,Z),(iff(true,Y,Z),Tail),Tail) :- ! .

treatedbuiltin(arg(X,Y,Z),(iff(X), iff(Y,_,Z) ,Tail),Tail) :- ! .

makeiffgoal(F,T,B,TB) :- varlist(T,V) ,
			(Goal =.. [iff,F|V] , functor(Goal,_,Arity) , Arity < 5 , ! ,
					B = (Goal,TB)
			;
			makeiffgoal([F|V],B,TB)
			) .

makeiffgoal(L,(Goal,T),T) :- L = [_X1] , ! , Goal =.. [iff|L] .
makeiffgoal(L,(Goal,T),T) :- L = [_X1,_X2] , ! , Goal =.. [iff|L] .
makeiffgoal(L,(Goal,T),T) :- L = [_X1,_X2,_X3] , ! , Goal =.. [iff|L] .
makeiffgoal(L,(Goal,T),T) :- L = [_X1,_X2,_X3,_X4] , ! , Goal =.. [iff|L] .
makeiffgoal([X1,X2,X3|R],(Goal,T),TB) :- ! , Goal =.. [iff,X1,X2,X3,New] ,
				makeiffgoal([New|R],T,TB) .
makeiffgoal(L,(Goal,TB),TB) :- ! , Goal =.. [iff|L] .


flat_to_iff(In,Out) :-
		see(In) ,
		tell(Out) ,
		flat_to_iff ,
		seen ,
		told .

flat_to_iff :- read(Cl) , Cl \== end_of_file , ! ,
		(makeiff(Cl,NewCl) , writeq(NewCl) , write(' .') , nl , fail ; flat_to_iff) .
flat_to_iff .

% Author: Bart Demoen K.U.Leuven
% Date: March 23 1993
% Most recent documented change: March 25 1993
% Purpose: just some definitions of the iff predicate: 4 is enough because makeiff
%		catches larger arities and composes from these

fail_iff :- fail .

iff(true).

iff(true,true).
iff(false,false).

iff(true,true,true).
iff(false,true,false).
iff(false,false,true).
iff(false,false,false).

iff(true,true,true,true).
iff(false,true,false,false).
iff(false,false,true,false).
iff(false,false,false,true).
iff(false,true,true,false).
iff(false,true,false,true).
iff(false,false,true,true).
iff(false,false,false,false).

has_semantics(fail_iff,0) .
has_semantics(iff,1) .
has_semantics(iff,2) .
has_semantics(iff,3) .
has_semantics(iff,4) .
