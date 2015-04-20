:- module('abstract.rul-conjcntg',['abstract.rul-conjcntg:abstract_parent'/6,
	                           'abstract.rul-conjcntg:abstract_leaf'/6]).

:- use_package( .('../ecce_no_rt2') ).

/* conjunctive split with termsize embedded subconjunction
 + apply RUL widening */

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').
:- use_module('../global_tree').

:- use_module( 'abstract.common.pl' ).

%:- use_module('../rul/INTERFACE').

%%%:- ecce_use_module('rul/ecceRUL').
%%%:- ecce_use_module('rul/instance_entails').

'abstract.rul-conjcntg:abstract_parent'(_GoalID,Goal,_Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :-
	
	print(start_rul_abstract_parent(_GoalID,Goal,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees)),nl,
	
	divide_constraint_rul_goal(Goal,OGoal,CGoal),
	gt_node_goal(WhistleGoalID,WhistleGoal),
	divide_constraint_rul_goal(WhistleGoal,OWhistleGoal,CWGoal),
	print(whistle_goal(OWhistleGoal,CWGoal)),nl,
	
	gt_node_goal(WhistleGoalID,WhistleGoal),
	print('abstract.rul-conjcntg:find_embedding_conjunction'(OWhistleGoal,Goal)),nl,
	findall(split(C,CN,R,RN),
	  ('abstract.rul-conjcntg:find_embedding_conjunction'(OWhistleGoal,Goal,1,
							C,CN,R,RN)),
          SPs),
	print(split_candidates(SPs)),debug_nl,
	find_minimally_general_element(SPs,Min),
	print(min(Min)),nl,
	Min = split(MG,_MGN,Rest,_RestN),
	divide_constraint_rul_goal(Rest,ORest,CRest),
	print(calling_widen(OWhistleGoal,CWGoal,MG,CRest,AGoal,AConstr)),
	nl,
	(widen(OWhistleGoal,CWGoal,MG,CRest,AGoal,AConstr)
	 ->  (print(widened_ordinary_goal(AGoal)),nl,
	      (true -> print_rul(AConstr) ; true),
	      nl)
	 ;   (print('widen failed: abstract_parent unsuccessful'),
	      debug_nl,fail)
	),
	append(AGoal,[AConstr],NewGoal),
	((variant_of(AGoal,OWhistleGoal),
	 instance_of_entails(AGoal,AConstr,OWhistleGoal,CWGoal),
	 instance_of_entails(OWhistleGoal,CWGoal,AGoal,AConstr))
	 -> (print('abstract_parent unsuccessful'),nl,fail)
	 ;  (print(abstract_parent(wg(WhistleGoal),ag(NewGoal))),debug_nl)
	),
	pp_mnf(partition_goal(NewGoal,NewWhistleGoals)),
	construct_none_chtrees(NewWhistleGoals,NewWhistleChtrees).

'abstract.rul-conjcntg:abstract_leaf'(_GoalID,Goal,_Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	print(abstract_leaf),nl,nl,
	gt_node_goal(WhistleGoalID,WhistleGoal),
	divide_constraint_rul_goal(WhistleGoal,OWhistleGoal,CWGoal),
	print(whistle_goal(OWhistleGoal,CWGoal)),nl,
	split(OWhistleGoal,Goal,NewGoals,NewChtrees).
	/* copy(split(Goal,WhistleGoal,NewGoals),S),
	numbervars(S,1,_),
	debug_print(S),debug_nl. */


split(WhistleGoal,Goal,NewGoals,NewChtrees) :-
  findall(split(MSG,CN,R,RN,R2,RN2),
	  ('abstract.rul-conjcntg:find_contig_embedding_conjunction'(WhistleGoal,Goal,1,
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
split(_WhistleGoal,Goal,NewGoals,NewChtrees) :-
	print('!'),
	Goal = [Lit|Rest],
	get_literal_numbers(Rest,2,RestN),
	pp_mnf(partition_goal([Lit],[1],MGSplit)),
	pp_mnf(partition_goal(Rest,RestN,RestSplit)),
	pp_mnf(append(MGSplit,RestSplit,NewGoals)),
	construct_none_chtrees(NewGoals,NewChtrees).



construct_none_chtrees([],[]).
construct_none_chtrees([_H|T],[none|TN]) :-
	construct_none_chtrees(T,TN).


/* For splitting */
'abstract.rul-conjcntg:find_embedding_conjunction'([],[],_SelNr,[],[],[],[]).
'abstract.rul-conjcntg:find_embedding_conjunction'([PA|PAs],[A|As],SelNr,
		[A|C],[SelNr|CN],R,RN) :-
  atoms_have_same_predicate(PA,A,_),
  /* not(strict_instance_of(PA,A)),
  mixtus_term_size_embedded(PA,A), because of RUL component
   sometimes termsize can decrease and whistle still blow */
  S1 is SelNr + 1,
  'abstract.rul-conjcntg:find_embedding_conjunction'(PAs,As,S1,C,CN,R,RN).
'abstract.rul-conjcntg:find_embedding_conjunction'(PAs,[A|As],SelNr,
		C,CN,[A|R],[SelNr|RN]) :- 
  S1 is SelNr + 1,
  'abstract.rul-conjcntg:find_embedding_conjunction'(PAs,As,S1,C,CN,R,RN).


'abstract.rul-conjcntg:find_contig_embedding_conjunction'(WhistleGoal,Goal,SelNr,C,CN,[],[],R2,RN2) :-
	'abstract.rul-conjcntg:fcec'(WhistleGoal,Goal,SelNr,C,CN,R2,RN2).
'abstract.rul-conjcntg:find_contig_embedding_conjunction'(PAs,[A|As],SelNr,
		C,CN,[A|R],[SelNr|RN],R2,RN2) :-
  S1 is SelNr + 1,
  'abstract.rul-conjcntg:find_contig_embedding_conjunction'(PAs,As,S1,C,CN,R,RN,R2,RN2).

'abstract.rul-conjcntg:fcec'([],Rest,SelNr,[],[],Rest,RN) :-
	get_literal_numbers(Rest,SelNr,RN).
'abstract.rul-conjcntg:fcec'([PA|PAs],[A|As],SelNr,[A|C],[SelNr|CN],R,RN) :-
  atoms_have_same_predicate(PA,A,_),
  /* not(strict_instance_of(PA,A)),
  mixtus_term_size_embedded(PA,A), see remark above */
  S1 is SelNr + 1,
  'abstract.rul-conjcntg:fcec'(PAs,As,S1,C,CN,R,RN).
