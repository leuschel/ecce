:- module('partition.staticconjnobi',['partition.staticconjnobi:partition_goal'/3]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic partition_goal/3.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../static_dynamic_functors').


'partition.staticconjnobi:partition_goal'([],[],[]).
'partition.staticconjnobi:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_built_in_literal(Lit),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],built_in(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.staticconjnobi:partition_goal'(T,TN,ST).
'partition.staticconjnobi:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_negative_literal(Lit,Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],neg(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.staticconjnobi:partition_goal'(T,TN,ST).
'partition.staticconjnobi:partition_goal'([Atom|T],[SelNr|TN],
		[ split_goal([Atom|InBlock],[SelNr|InNrs]) | ST]) :-
	'partition.staticconjnobi:get_block'(T,TN,[Atom],InBlock,NotInBlock,InNrs,NotInNrs),
	'partition.staticconjnobi:partition_goal'(NotInBlock,NotInNrs,ST).


'partition.staticconjnobi:get_block'([],[],InBlockSoFar,[],[],[],[]).
'partition.staticconjnobi:get_block'([H|T],[HN|TN],InBlockSoFar,InBlock,NotInBlock,
			InSelNr,NotInSelNr) :-
	((pp_cll(sharing(InBlockSoFar,[H])),
	  \+(is_built_in_literal(H)),
	  \+(is_negative_literal(H,_)),
	  reverse([H|InBlockSoFar],CurGoal),  /* improve efficiency !! */
	  static_conjunction(CurGoal)
	)
	 -> (InBlock = [H|InBlock2],
	     InSelNr = [HN|InSelNr2],
	     'partition.staticconjnobi:get_block'(T,TN,[H|InBlockSoFar],InBlock2,NotInBlock,
			    InSelNr2,NotInSelNr))
	 ;  (NotInBlock = [H|NotInBlock2],
	     NotInSelNr = [HN|NotInSelNr2],
	     'partition.staticconjnobi:get_block'(T,TN,InBlockSoFar,InBlock,NotInBlock2,
			    InSelNr,NotInSelNr2))
	).


