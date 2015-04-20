:- module('partition.contig',['partition.contig:partition_goal'/3]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic partition_goal/3.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').


'partition.contig:partition_goal'([],[],[]).
'partition.contig:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_built_in_literal(Lit),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],built_in(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.contig:partition_goal'(T,TN,ST).
'partition.contig:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_negative_literal(Lit,_Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],neg(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.contig:partition_goal'(T,TN,ST).
'partition.contig:partition_goal'([Atom|T],[SelNr|TN],
		[ split_goal([Atom|InBlock],[SelNr|InNrs]) | ST]) :-
	'partition.contig:get_block'(T,TN,[Atom],InBlock,NotInBlock,InNrs,NotInNrs),
	'partition.contig:partition_goal'(NotInBlock,NotInNrs,ST).


'partition.contig:get_block'([],[],_InBlockSoFar,[],[],[],[]).
'partition.contig:get_block'([H|T],[HN|TN],InBlockSoFar,InBlock,NotInBlock,
			InSelNr,NotInSelNr) :-
	((not(is_built_in_literal(H)),
	  not(is_negative_literal(H,_)),
	  pp_cll(sharing(InBlockSoFar,[H]))
	 )
	 -> (InBlock = [H|InBlock2],
	     InSelNr = [HN|InSelNr2],
	     'partition.contig:get_block'(T,TN,[H|InBlockSoFar],InBlock2,NotInBlock,
			    InSelNr2,NotInSelNr))
	 ;  (NotInBlock = [H|T],
	     NotInSelNr = [HN|TN],
	     InBlock = [],
	     InSelNr = [])
	).


