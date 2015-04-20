:- module('partition.conjunctivepd',['partition.conjunctivepd:partition_goal'/3]).

%:- dynamic partition_goal/3.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').

'partition.conjunctivepd:partition_goal'([],[],[]).
'partition.conjunctivepd:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_built_in_literal(Lit),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],built_in(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.conjunctivepd:partition_goal'(T,TN,ST).
'partition.conjunctivepd:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_negative_literal(Lit,_Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],neg(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.conjunctivepd:partition_goal'(T,TN,ST).
'partition.conjunctivepd:partition_goal'([Atom|T],[SelNr|TN],
		[ split_goal([Atom|InBlock],[SelNr|InNrs]) | ST]) :-
	'partition.conjunctivepd:get_partition'(T,TN,[Atom],InBlock,NotInBlock,InNrs,NotInNrs),
	'partition.conjunctivepd:partition_goal'(NotInBlock,NotInNrs,ST).


'partition.conjunctivepd:get_partition'(T,TN,InBlockSoFar,InBlock,NotInBlock,InNrs,NotInNrs) :-
	'partition.conjunctivepd:get_block'(T,TN,InBlockSoFar,InBlock1,NotInBlock1,InNrs1,NotInNrs1),
	((InBlockSoFar == InBlock1)
	 -> (InBlock = InBlock1, NotInBlock = NotInBlock1,
	     InNrs = InNrs1, NotInNrs = NotInNrs1)
	 ;  ('partition.conjunctivepd:get_partition'(T,TN,InBlock1,InBlock,NotInBlock,InNrs,NotInNrs))
	).

'partition.conjunctivepd:get_block'([],[],_InBlockSoFar,[],[],[],[]).
'partition.conjunctivepd:get_block'([H|T],[HN|TN],InBlockSoFar,InBlock,NotInBlock,
			InSelNr,NotInSelNr) :-
	(pp_cll(sharing(InBlockSoFar,[H]))
	 -> (InBlock = [H|InBlock2],
	     InSelNr = [HN|InSelNr2],
	     'partition.conjunctivepd:get_block'(T,TN,[H|InBlockSoFar],InBlock2,NotInBlock,
			    InSelNr2,NotInSelNr))
	 ;  (NotInBlock = [H|NotInBlock2],
	     NotInSelNr = [HN|NotInSelNr2],
	     'partition.conjunctivepd:get_block'(T,TN,InBlockSoFar,InBlock,NotInBlock2,
			    InSelNr,NotInSelNr2))
	).


