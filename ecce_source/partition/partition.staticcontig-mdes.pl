:- module('partition.staticcontig-mdes',['partition.staticcontig-mdes:partition_goal'/3]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic partition_goal/3.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../static_dynamic_functors').
:- use_module('../modes').


'partition.staticcontig-mdes:partition_goal'([],[],[]).
'partition.staticcontig-mdes:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_built_in_literal(Lit),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],built_in(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.staticcontig-mdes:partition_goal'(T,TN,ST).
'partition.staticcontig-mdes:partition_goal'([Lit|T],[SelNr|TN],SGoal) :-
	is_negative_literal(Lit,Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],neg(SelNr))|ST])
	 ;  (SGoal = ST)
	),
	'partition.staticcontig-mdes:partition_goal'(T,TN,ST).
'partition.staticcontig-mdes:partition_goal'([Atom|T],[SelNr|TN],
		[ split_goal([Atom|InBlock],[SelNr|InNrs]) | ST]) :-
	pp_mnf(get_potential_io_args(Atom,AInArgs,AOutArgs)),
	'partition.staticcontig-mdes:get_block'(T,TN,[AInArgs],[AOutArgs],
	          InBlock,NotInBlock,InNrs,NotInNrs),
	'partition.staticcontig-mdes:partition_goal'(NotInBlock,NotInNrs,ST).

'partition.staticcontig-mdes:get_block'([],[],_InArgsSoFar,_OutArgsSoFar,[],[],[],[]).
'partition.staticcontig-mdes:get_block'([H|T],[HN|TN],InArgsSoFar,OutArgsSoFar,InBlock,NotInBlock,
			InSelNr,NotInSelNr) :-
	pp_mnf(get_potential_io_args(H,HInArgs,HOutArgs)),
	(( (pp_cll(sharing(InArgsSoFar,[HOutArgs]));
	    pp_cll(sharing(OutArgsSoFar,[HInArgs]))),
	  not(is_built_in_literal(H)),
	  not(is_negative_literal(H,_)),
	  /* reverse([H|InBlockSoFar],CurGoal),static_conjunction(CurGoal)
		use if static_conj depends on order of literals */
	  static_conjunction([H|InBlockSoFar])
	)
	 -> (InBlock = [H|InBlock2],
	     InSelNr = [HN|InSelNr2],
	     'partition.staticcontig-mdes:get_block'(T,TN,[HInArgs|InArgsSoFar],[HOutArgs|OutArgsSoFar],
	     		    InBlock2,NotInBlock,
			    InSelNr2,NotInSelNr))
	 ;  (NotInBlock = [H|T],
	     NotInSelNr = [HN|TN],
	     InBlock = [],
	     InSelNr = [])
	).


