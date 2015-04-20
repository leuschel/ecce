:- module( 'abstract.rul-std' , ['abstract.rul-std:abstract_parent'/6,
	                         'abstract.rul-std:abstract_leaf'/6]).
/* file: abstract.rul.pro */

%:- ecce_use_module('rul/INTERFACE').
%%%:-ecce_use_module('rul/ecceRUL').

% :- initialization(retractall(rul_active(_)),assert(rul_active(yes))).

%:- dynamic abstract_parent/6.
%:- dynamic abstract_leaf/6.


'abstract.rul-std:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees) :-
	debug_print(start_rul_abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees)),debug_nl,
	divide_constraint_rul_goal(Goal,OGoal,CGoal),
	gt_node_goal(WhistleGoalID,WhistleGoal),
	divide_constraint_rul_goal(WhistleGoal,OWhistleGoal,CWGoal),
	trace_print(calling_ecceRUL_widen(OWhistleGoal,CWGoal,OGoal,CGoal,AGoal,AConst)),debug_nl,
	(widen(OWhistleGoal,CWGoal,OGoal,CGoal,AGoal,AConstr)
	 ->  (trace_print(widened_ordinary_goal(AGoal)),debug_nl,
	      (trace_printing(on) -> print_rul(AConstr) ; true),
	      debug_nl)
	 ;   (print('widen failed: abstract_parent unsuccessful'),debug_nl,fail)
	),
	analyticFold:l_goalRULification(AGoal,NewAGoal,AConstr,NewAConstr),
	debug_print(rULified_widened_ordinary_goal(NewAGoal)),debug_nl,
	(trace_printing(on) -> print_rul(NewAConstr) ; true),
	append(NewAGoal,[NewAConstr],NewGoal),
	debug_print(NewGoal),debug_nl,
	NewWhistleGoals = [split_goal(NewGoal,FSI)],
	get_full_split_indicator(NewGoal,1,FSI), debug_print(fsi(FSI)),debug_nl,
	NewWhistleChtrees = [none],
	debug_print(end_abstract_parent(GoalID,Goal,Chtree,WhistleGoalID,
		NewWhistleGoals,NewWhistleChtrees)),
	print(w),debug_nl.

'abstract.rul-std:abstract_leaf'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees) :-
	debug_print(abstract_leaf_calling_abstract_parent),debug_nl,
	'abstract.rul-std:abstract_parent'(GoalID,Goal,Chtree,WhistleGoalID,NewGoals,NewChtrees).


get_full_split_indicator([],Nr,[]).
get_full_split_indicator([H|T],Nr,[Nr|FST]) :-
	Nr1 is Nr + 1,
	get_full_split_indicator(T,Nr1,FST).
