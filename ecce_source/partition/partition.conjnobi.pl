:- module('partition.conjnobi',['partition.conjnobi:partition_goal'/3]).

%:- dynamic partition_goal/3.

:- use_package( .('../ecce_no_rt2') ).


:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').


'partition.conjnobi:partition_goal'([],[],[]).
'partition.conjnobi:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_built_in_literal(Lit),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],built_in(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.conjnobi:partition_goal'(T,TN,ST).
'partition.conjnobi:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_negative_literal(Lit,Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],neg(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.conjnobi:partition_goal'(T,TN,ST).
'partition.conjnobi:partition_goal'([Atom|T],[SelNr|TN],
		[ split_goal([Atom|InBlock],[SelNr|InNrs]) | ST]) :-
	'partition.conjnobi:get_partition'(T,TN,[Atom],InBlock,NotInBlock,InNrs,NotInNrs),
	'partition.conjnobi:partition_goal'(NotInBlock,NotInNrs,ST).

'partition.conjnobi:get_partition'(T,TN,InBlockSoFar,InBlock,NotInBlock,InNrs,NotInNrs) :-
	'partition.conjnobi:get_block'(T,TN,InBlockSoFar,InBlock1,NotInBlock1,InNrs1,NotInNrs1),
	((InBlockSoFar == InBlock1)
	 -> (InBlock = InBlock1, NotInBlock = NotInBlock1,
	     InNrs = InNrs1, NotInNrs = NotInNrs1)
	 ;  ('partition.conjnobi:get_partition'(T,TN,InBlock1,InBlock,NotInBlock,InNrs,NotInNrs))
	).

'partition.conjnobi:get_block'([],[],InBlockSoFar,[],[],[],[]).
'partition.conjnobi:get_block'([H|T],[HN|TN],InBlockSoFar,InBlock,NotInBlock,
			InSelNr,NotInSelNr) :-
	((pp_cll(sharing(InBlockSoFar,[H])),
	  not(is_built_in_literal(H)),
	  not(is_negative_literal(H,_))
	)
	 -> (InBlock = [H|InBlock2],
	     InSelNr = [HN|InSelNr2],
	     'partition.conjnobi:get_block'(T,TN,[H|InBlockSoFar],InBlock2,NotInBlock,
			    InSelNr2,NotInSelNr))
	 ;  (NotInBlock = [H|NotInBlock2],
	     NotInSelNr = [HN|NotInSelNr2],
	     'partition.conjnobi:get_block'(T,TN,InBlockSoFar,InBlock,NotInBlock2,
			    InSelNr,NotInSelNr2))
	).


