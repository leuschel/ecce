:- module(unfold_helper,[manual_unfold/0,manual_unfold/2,manual_unfold/4,manual_select_positive_literal/4,print_goal/2,manual_chpath/5,unfhist_indent/1]).

:- use_package( .(ecce_no_rt) ).


:- use_module(library(lists)).

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(bimtools).
:- use_module(unfold_history).
:- use_module(calc_chtree).
:- use_module(static_dynamic_functors).
:- use_module(parametric_files).


:- use_module('more_specific/more_specific').
:- use_module(depth_bound).


manual_unfold :-
	print('Goal to unfold manually ==>'),
	read(Goal),
	((Goal = [_|_])
	 -> (PEGoal = Goal)
	 ;  (PEGoal = [Goal])
	),
	(term_is_of_type(PEGoal,goal,no)
	 -> (true)
	 ;  (print('ILLEGAL GOAL: '),print(goal),nl,
	     print('Contains variables as literals (use call/1)'),nl,
	     print(' or is an open-ended list.'),nl,
	     fail
	    )
	),
	(goal_contains_undefined_literal(PEGoal)
	 -> (print('### Goal contains undefined calls --> will fail !'),nl,
	     print('### Be sure to read in all necessary files !'),nl
	    )
	 ;  (true)
	),
	varlist(PEGoal,Top),
	manual_unfold(Top,PEGoal).

manual_unfold(Top,PEGoal) :-
	reset_static_functors,
	print('-> parameters: '),print_parameters,nl,
	print('-> calculating static functors'),nl,
	calculate_static_functors,
	calculate_static_functors_for_query(PEGoal),
	manual_unfold(PEGoal,Top,[],Chtree),
	print_chtree(Chtree).

manual_unfold([],_TopGoalVarlist,_UnfHist,success) :- print('success'),nl.
manual_unfold([H|T],TopGoalVarlist,UnfHist,Chtree) :-
    unfhist_indent(UnfHist),print_goal(TopGoalVarlist,[H|T]),
    ((pp_cll(more_specific_transformation([H|T])),
      not(dead([H|T])) )
      ->
	(select_callable_built_in([H|T],NrOfBI,SelBI)
	 -> ((unfhist_indent(UnfHist),print(calling_built_in(SelBI)),
	      call_built_in(SelBI))
	     -> (print(' - success'),nl,copy(SelBI,CSelBI),
		 IntUnfHist = [sel(CSelBI,NrOfBI,0,bi)|UnfHist],
		 pp_mnf(split_list([H|T],NrOfBI,Left,Sel,Right)),
		 append(Left,Right,NewGoal),
		 get_predicate(SelBI,BI),
		 manual_unfold(NewGoal,TopGoalVarlist,IntUnfHist,SubChtree),
		 ((SubChtree = empty)
		  -> (Chtree=empty)
		  ;  (Chtree=built_in_eval(NrOfBI,BI,SubChtree))
		 )
		)
	     ;  (print(' - failed'),nl,
		 Chtree=empty)
	    )
	 ;
        (select_removable_literal([H|T],NrOfNegLit,SelNegLit)
	 -> (copy(SelNegLit,CSelNegLit),
	     IntUnfHist = [sel(CSelNegLit,NrOfNegLit,0,neg)|UnfHist],
	     pp_mnf(split_list([H|T],NrOfNegLit,Left,Sel,Right)),
	     append(Left,Right,NewGoal),
	     (is_negative_literal(SelNegLit,NgAtm)
		-> get_predicate(NgAtm,NegPred)
		;  get_predicate(SelNegLit,NegPred)
	     ),
	     manual_unfold(NewGoal,TopGoalVarlist,IntUnfHist,SubChtree),
	     ((SubChtree = empty)
		-> (Chtree=empty)
		;  (Chtree=remove(NrOfNegLit,NegPred,SubChtree))
	     )
	    )
	 ;  ((pp_cll(depth_bound_ok(UnfHist)),
	      (critical_bd_findall_nesting
		-> (print(bd_findall_critical),nl,fail) ; true),
		    unfhist_indent(UnfHist),
	            manual_select_positive_literal([H|T],TopGoalVarlist,
					NrOfSelLiteral,SelLit) )
	      -> (copy(SelLit,CSelLit),
	          IntUnfHist = [sel(CSelLit,NrOfSelLiteral,0,DI)|UnfHist],
		  (undeterminate([H|T],NrOfSelLiteral)
		   -> (DI = nondet,
			bd_findall(CP,manual_chpath([H|T],TopGoalVarlist,
				  NrOfSelLiteral,IntUnfHist,CP), Chpaths),
	               ((Chpaths=[])
	                -> (Chtree=empty)
	                ;  (Chtree=select(NrOfSelLiteral,Chpaths))
	               )
		      )
		   ;  (DI = det, unfhist_indent(UnfHist),print(determinate),nl,
			manual_chpath([H|T],TopGoalVarlist,
					NrOfSelLiteral,IntUnfHist,CP)
			   -> (Chtree=select(NrOfSelLiteral,[CP]))
			   ;  (Chtree=empty)
		      )
		  )
	         )
	     ;  (unfhist_indent(UnfHist),print(stop),nl,
		 Chtree=stop) /* select_positive_literal stopped unfolding */
	   )
	)
	)
     ; (unfhist_indent(UnfHist),print('dead'),nl,
	Chtree=empty) /* msv or dead found out that the goal will always fail */
    ).


manual_select_positive_literal(Goal,_Top,NrOfSelLiteral,Sel) :-
	print(' select =>'),
	read(NrOfSelLiteral),
	NrOfSelLiteral>0,
	split_list(Goal,NrOfSelLiteral,_Left,Sel,_Right),
	print(selected(Sel)),nl.


print_goal(Top,Goal) :-
	print(Top),print(' '),
	print_goal2(1,Goal).

print_goal2(_N,[]) :- print('.'),nl.
print_goal2(N,[H|T]) :-
	print(N),print(': '),
	print(H),print(' '),
	N1 is N + 1,
	print_goal2(N1,T).


manual_chpath(Goal,TopGoalVarlist,NrOfSelLiteral,UnfHist,Chpath) :-
	pp_mnf(split_list(Goal,NrOfSelLiteral,Left,Sel,Right)),
	(is_negative_literal(Sel,_NegatedAtom)
	-> (print('### Error negative literal selected in manual_chpath'),nl,
	    print('### '),print(Sel),nl,fail)
	;  ((pp_cll(is_built_in_literal(Sel)))
	    -> (print('### Error built-in selected in manual_chpath'),nl,
	        print('### '),print(Sel),nl,fail)
	    ; true
           )
	),
	claus(ClauseNr,Sel,Body),
	unfhist_indent(UnfHist),print(match_clause(ClauseNr)),nl,
	pp_mnf(append(Body,Right,IntGoal)),
	pp_mnf(append(Left,IntGoal,NewGoal)),
	pp_mnf(update_unfold_history(UnfHist,NrOfSelLiteral,
				Body,NewUnfHist)),
	manual_unfold(NewGoal,TopGoalVarlist,NewUnfHist,SubTree),
	not(SubTree=empty),
	Chpath=match(ClauseNr,SubTree).

unfhist_indent([]).
unfhist_indent([_H|T]) :-
	print('  '),
	unfhist_indent(T).
