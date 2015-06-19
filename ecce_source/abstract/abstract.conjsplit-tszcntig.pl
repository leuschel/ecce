:- module('abstract.conjsplit-tszcntig',['abstract.conjsplit-tszcntig:abstract_parent'/6,
	                                 'abstract.conjsplit-tszcntig:abstract_leaf'/6]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').
:- use_module('../homeomorphic').

:- use_module('../partition/partition').

:- use_module( 'abstract.common.pl' ).

'abstract.conjsplit-tszcntig:abstract_parent'(_GoalID,Goal,_Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	findall(split(MSG,CN,R,RN),
	  ('abstract.conjsplit-tszcntig:find_embedding_conjunction'(WhistleGoal,Goal,1,
							C,CN,R,RN),
               msg(WhistleGoal,C,MSG)),
          SPs), /* we don't have to use find_contig as we abstract the parent */
	find_minimally_general_element(SPs,Min),
	Min = split(MG,_MGN,_Rest,_RestN),
	(variant_of(MG,WhistleGoal)
	 -> (debug_print('abstract_parent unsuccessful'),debug_nl,fail)
	 ;  (debug_print(abstract_parent(wg(WhistleGoal),mg(MG))),debug_nl)
	),
	pp_mnf(partition_goal(MG,NewWhistleGoals)),
	construct_none_chtrees(NewWhistleGoals,NewWhistleChtrees).

'abstract.conjsplit-tszcntig:abstract_leaf'(_GoalID,Goal,_Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	'abstract.conjsplit-tszcntig:split'(WhistleGoal,Goal,NewGoals,NewChtrees).
	/* copy(split(Goal,WhistleGoal,NewGoals),S),
	numbervars(S,1,_),
	debug_print(S),debug_nl. */


'abstract.conjsplit-tszcntig:split'(WhistleGoal,Goal,NewGoals,NewChtrees) :-
  findall(split(MSG,CN,R,RN,R2,RN2),
	  ('abstract.conjsplit-tszcntig:find_contig_embedding_conjunction'(WhistleGoal,Goal,1,
							C,CN,R,RN,R2,RN2),
               msg(WhistleGoal,C,MSG)),
          SPs),
  find_minimally_general_element(SPs,Min),
  Min = split(MG,MGN,Rest,RestN,Rest2,RestN2),!,
  pp_mnf(partition_goal(MG,MGN,MGSplit)),
  pp_mnf(partition_goal(Rest,RestN,RestSplit)),
  pp_mnf(partition_goal(Rest2,RestN2,RestSplit2)),
  pp_mnf(append(MGSplit,RestSplit2,IntSplit)),
  pp_mnf(append(RestSplit,IntSplit,NewGoals)),
  construct_none_chtrees(NewGoals,NewChtrees).
'abstract.conjsplit-tszcntig:split'(_WhistleGoal,Goal,NewGoals,NewChtrees) :-
	print('!'),
	Goal = [Lit|Rest],
	get_literal_numbers(Rest,2,RestN),
	pp_mnf(partition_goal([Lit],[1],MGSplit)),
	pp_mnf(partition_goal(Rest,RestN,RestSplit)),
	pp_mnf(append(MGSplit,RestSplit,NewGoals)),
	construct_none_chtrees(NewGoals,NewChtrees).



/* For splitting */
'abstract.conjsplit-tszcntig:find_embedding_conjunction'([],Rest,SelNr,[],[],Rest,RN) :-
	get_literal_numbers(Rest,SelNr,RN).
'abstract.conjsplit-tszcntig:find_embedding_conjunction'([PA|PAs],[A|As],SelNr,
		[A|C],[SelNr|CN],R,RN) :-
  atoms_have_same_predicate(PA,A,_),
  \+(strict_instance_of(PA,A)),
  mixtus_term_size_embedded(PA,A),
  S1 is SelNr + 1,
  'abstract.conjsplit-tszcntig:find_embedding_conjunction'(PAs,As,S1,C,CN,R,RN).
'abstract.conjsplit-tszcntig:find_embedding_conjunction'(PAs,[A|As],SelNr,
		C,CN,[A|R],[SelNr|RN]) :-
  S1 is SelNr + 1,
  'abstract.conjsplit-tszcntig:find_embedding_conjunction'(PAs,As,S1,C,CN,R,RN).


'abstract.conjsplit-tszcntig:find_contig_embedding_conjunction'(WhistleGoal,Goal,SelNr,C,CN,[],[],R2,RN2) :-
	'abstract.conjsplit-tszcntig:fcec'(WhistleGoal,Goal,SelNr,C,CN,R2,RN2).
'abstract.conjsplit-tszcntig:find_contig_embedding_conjunction'(PAs,[A|As],SelNr,
		C,CN,[A|R],[SelNr|RN],R2,RN2) :-
  S1 is SelNr + 1,
  'abstract.conjsplit-tszcntig:find_contig_embedding_conjunction'(PAs,As,S1,C,CN,R,RN,R2,RN2).

'abstract.conjsplit-tszcntig:fcec'([],Rest,SelNr,[],[],Rest,RN) :-
	get_literal_numbers(Rest,SelNr,RN).
'abstract.conjsplit-tszcntig:fcec'([PA|PAs],[A|As],SelNr,[A|C],[SelNr|CN],R,RN) :-
  atoms_have_same_predicate(PA,A,_),
  \+(strict_instance_of(PA,A)),
  mixtus_term_size_embedded(PA,A),
  S1 is SelNr + 1,
  'abstract.conjsplit-tszcntig:fcec'(PAs,As,S1,C,CN,R,RN).

