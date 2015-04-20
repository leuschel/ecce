:- module('partition.clpfd',['partition.clpfd:partition_goal'/3]).

%:- dynamic partition_goal/3.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').

'partition.clpfd:partition_goal'(Goal,Nrs,ResSplitGoal) :-
	divide_constraint_goal_with_nrs(Goal,Nrs,OGoal,ONrs,CGoal,CNrs),
	partition_goal_classic_conj(OGoal,ONrs,SplitOGoal),
	print(proj(SplitOGoal,CGoal,CNrs)),nl,
	project_constraints(SplitOGoal,CGoal,CNrs,ResSplitGoal).

wrap_into_built_in([],[]).
wrap_into_built_in([H|T],[built_in(H)|WT]) :-
	wrap_into_built_in(T,WT).

project_constraints([],_,_,[]).
project_constraints([split_goal(Goal,SplitInd)|T],CGoal,CNrs,
		    [split_goal(NewGoal,NewInd)|ST]) :-
	((SplitInd = [_|_])
	 -> (project_constraint_with_nrs(CGoal,CNrs,Goal,PCG,PCN),
	     append(Goal,PCG,NewGoal),
	     append(SplitInd,PCN,NewInd)
	    )
	 ;  (NewGoal = Goal, NewInd = SplitInd)
	),
	project_constraints(T,CGoal,CNrs,ST).

/* has to be improved !!!! */
project_constraint_with_nrs([],[],_,[],[]).
project_constraint_with_nrs([C|T],[N|TN],Term,ResC,ResN) :-
	(true /*sharing(C,Term)*/
	 -> (ResC = [C|TRC], ResN = [N|TRN])
	 ;  (ResC = TRC, ResN = TRN)
	),
	project_constraint_with_nrs(T,TN,Term,TRC,TRN).


/*
| ?- assert((temppred(X) :- (X #> Y , Y#> 2))).
 
true ?
 
yes
| ?- call_residue(temppred(X),R).
 
R = [[X]-(X in 4..sup)] ?
 
yes     

*/
	
divide_constraint_goal_with_nrs([],[],[],[],[],[]).
divide_constraint_goal_with_nrs([Lit|T],[SelNr|NT],
		       OrdLits,OrdNrs,ConstrLits,ConstrNrs) :-
   (is_constraint_literal(Lit)
    -> (ConstrLits = [Lit|C2], ConstrNrs = [SelNr|CN2],
	O2 = OrdLits, ON2 = OrdNrs)
    ;  (OrdLits = [Lit|O2], OrdNrs = [SelNr|ON2],
	C2 = ConstrLits, CN2 = ConstrNrs)
   ),
   divide_constraint_goal_with_nrs(T,NT,O2,ON2,C2,CN2).




partition_goal_classic_conj([],[],[]).
partition_goal_classic_conj([Lit|T],[SelNr|TN],SGoal) :-
	is_built_in_literal(Lit),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],built_in(SelNr))|ST])
	 ;  (SGoal = ST) /* do not start conjunction with built-in */
	),
	partition_goal_classic_conj(T,TN,ST).
partition_goal_classic_conj([Lit|T],[SelNr|TN],SGoal) :-
	is_negative_literal(Lit,_Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],neg(SelNr))|ST])
	 ;  (SGoal = ST) /* do not start conjunction with negation */
	),
	partition_goal_classic_conj(T,TN,ST).
partition_goal_classic_conj([Atom|T],[SelNr|TN],
		[ split_goal([Atom|InBlock],[SelNr|InNrs]) | ST]) :-
	get_block_standard_pd(T,TN,[Atom],InBlock,NotInBlock,InNrs,NotInNrs),
	partition_goal_classic_conj(NotInBlock,NotInNrs,ST).


get_block_standard_pd(T,TN,_,[],T,[],TN).

'partition.clpfd:get_block'([],[],_InBlockSoFar,[],[],[],[]).
'partition.clpfd:get_block'([H|T],[HN|TN],InBlockSoFar,InBlock,NotInBlock,
			InSelNr,NotInSelNr) :-
	((pp_cll(sharing(InBlockSoFar,[H]))
	 )
	 -> (InBlock = [H|InBlock2],
	     InSelNr = [HN|InSelNr2],
	     'partition.clpfd:get_block'(T,TN,[H|InBlockSoFar],InBlock2,NotInBlock,
			    InSelNr2,NotInSelNr))
	 ;  (NotInBlock = [H|T],
	     NotInSelNr = [HN|TN],
	     InBlock = [],
	     InSelNr = [])
	).


