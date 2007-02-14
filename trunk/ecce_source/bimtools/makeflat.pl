:- module(makeflat,_).
% Author: Bart Demoen K.U.Leuven
% Date: March 23 1993
% Last registered change: March 25 1993
% Purpose: transform a Prolog program in its 'flat' form, i.e.
% 		all unifications are explicit and every unification has at least
%		one (syntactic) variable argument
%	The meaning of the program is not guaranteed to be the same
%
% This file contains a predicate makeflat/2 which given a Prolog clause in
% argument 1, produces a term in argument 2 which is the flat version.
%
% Example: ?- makeflat((a(f(g(X),Y)) :- b([1])),N) .
%		N = a(Z) :- Z = f(g(X),Y) , T = [1] , b(T)
%
%	so, flat does not mean that all structures have been straightened out
%
%	spurious calls to true/0 appear at the end of the clauses

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(dec10_io)).

makeflat((H :- B) , (NewH :- NewB)) :- ! ,
			makeflatgoal(H,NewH,NewB,TailNewB) ,
			makeflatbody(B,TailNewB,true) .
makeflat(H,(NewH :- NewB)) :- 
			makeflatgoal(H,NewH,NewB,true) .


makeflatgoal(H,NewH,B,TB) :-
		H =.. [Name|ArgsH] ,
		makenewarglistandunifgoals(ArgsH,NewArgs,B,TB) ,
		NewH =.. [Name|NewArgs] .

makenewarglistandunifgoals([],[],T,T) .
makenewarglistandunifgoals([Var|RestArgs],[Var|NewRestArgs],B,TB) :-
			var(Var) , ! ,
			makenewarglistandunifgoals(RestArgs,NewRestArgs,B,TB) .
makenewarglistandunifgoals([Term|RestArgs],[Var|NewRestArgs],(Var = Term,B),TB) :-
			makenewarglistandunifgoals(RestArgs,NewRestArgs,B,TB) .

makeflatbody((G1,G2),B,TB) :- ! , makeflatbody(G1,B,TT) , makeflatbody(G2,TT,TB) .
makeflatbody(!,TB,TB) :- ! .
makeflatbody((Var1 = Var2),B,TB) :- var(Var1) , var(Var2) , ! , Var1 = Var2 , B = TB .
makeflatbody((Var = Term),B,TB) :- var(Var) , ! , B = (Var = Term , TB) .
makeflatbody((Term = Var),B,TB) :- var(Var) , ! , B = (Var = Term , TB) .
makeflatbody((Term1 = Term2),B,TB) :- 
			Term1 =.. [N|A1] ,
			Term2 =.. [N|A2] ,
			makeunifies(A1,A2,B,TB) , ! .
makeflatbody((_Term1 = _Term2),(fail,T),T) :- ! .
makeflatbody(Goal,B,TB) :- makeflatgoal(Goal,NewGoal,B,TT) , TT = (NewGoal,TB) .

makeunifies([],[],T,T) .
makeunifies([A1|R1],[A2|R2],B,BT) :-
		makeflatbody(A1 = A2,B,TT) , makeunifies(R1,R2,TT,BT) .

prolog_to_flat(In,Out) :-
		see(In) ,
		tell(Out) ,
		prolog_to_flat ,
		seen ,
		told .

prolog_to_flat :- read(Cl) , Cl \== end_of_file , ! ,
		(makeflat(Cl,NewCl) , writeq(NewCl) , write(' .') , nl , fail ; prolog_to_flat) .
prolog_to_flat .
