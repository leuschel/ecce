:- module('abstract.conjsplit',['abstract.conjsplit:abstract_parent'/6,'abstract.conjsplit:abstract_leaf'/6]).

:- use_package( .('../ecce_no_rt2') ).


%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').
:- use_module('../homeomorphic').

:- use_module('../partition/partition').

:- use_module( 'abstract.common.pl' ).


'abstract.conjsplit:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	findall(split(MSG,CN,R,RN),
	  ('abstract.conjsplit:find_homeomorphic_embeddeding_conjunction'(WhistleGoal,Goal,1,
							C,CN,R,RN),
               msg(WhistleGoal,C,MSG)),
          SPs),
	find_minimally_general_element(SPs,Min),
	Min = split(MG,MGN,Rest,RestN),
	(variant_of(MG,WhistleGoal)
	 -> (debug_print('abstract_parent unsuccessful'),debug_nl,fail)
	 ;  (debug_print(abstract_parent(wg(WhistleGoal),mg(MG))),debug_nl)
	),
	pp_mnf(partition_goal(MG,NewWhistleGoals)),
	construct_none_chtrees(NewWhistleGoals,NewWhistleChtrees).


'abstract.conjsplit:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	gt_node_goal(WhistleGoalID,WhistleGoal),
	'abstract.conjsplit:split'(WhistleGoal,Goal,NewGoals,NewChtrees).
	/* copy(split(Goal,WhistleGoal,NewGoals),S),
	numbervars(S,1,_),
	debug_print(S),debug_nl. */


'abstract.conjsplit:split'(WhistleGoal,Goal,NewGoals,NewChtrees) :-
  findall(split(MSG,CN,R,RN),
	  ('abstract.conjsplit:find_homeomorphic_embeddeding_conjunction'(WhistleGoal,Goal,1,
							C,CN,R,RN),
               msg(WhistleGoal,C,MSG)),
          SPs),
  find_minimally_general_element(SPs,Min),
  Min = split(MG,MGN,Rest,RestN),
  pp_mnf(partition_goal(MG,MGN,MGSplit)),
  pp_mnf(partition_goal(Rest,RestN,RestSplit)),
  pp_mnf(append(MGSplit,RestSplit,NewGoals)),
  construct_none_chtrees(NewGoals,NewChtrees).


/* For splitting */

'abstract.conjsplit:find_homeomorphic_embeddeding_conjunction'([],[],_SelNr,[],[],[],[]).
'abstract.conjsplit:find_homeomorphic_embeddeding_conjunction'([PA|PAs],[A|As],SelNr,
		[A|C],[SelNr|CN],R,RN) :-
  \+(strict_instance_of(PA,A)),
  homeomorphic_embedded(PA,A),
  S1 is SelNr + 1,
  'abstract.conjsplit:find_homeomorphic_embeddeding_conjunction'(PAs,As,S1,C,CN,R,RN).
'abstract.conjsplit:find_homeomorphic_embeddeding_conjunction'(PAs,[A|As],SelNr,
		C,CN,[A|R],[SelNr|RN]) :- 
  S1 is SelNr + 1,
  'abstract.conjsplit:find_homeomorphic_embeddeding_conjunction'(PAs,As,S1,C,CN,R,RN).

