:- module(chtree_tools,[
        remove_incorrect_builtins/3,
        has_builtins_which_generate_bindings/1,
        msg_chtree/3,
        precise_msg_chtree/3,
        transform_chtree_into_chterm/2,
        transform_chterm_into_chtree/2,
        calc_pruning_factor/2
    ]).

:- use_package( .(ecce_no_rt) ).


/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */


:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

/* file: chtree_tools.pro */

/*
:- ensure_consulted('$BIMTOOLS_PATH/typechecker.pro').
:- ensure_consulted('$BIMTOOLS_PATH/prepost.pro').
:- ensure_consulted('$BIMTOOLS_PATH/bd_findall.pro').
:- ensure_consulted('$BIMTOOLS_PATH/StdLists.pro').
*/
:- use_module(bimtools).

:- use_module(calc_chtree).
:- use_module('more_specific/more_specific').

:- include( multi_meta ).

/* --------------------------- */
/* remove_incorrect_builtins/3 */
/* --------------------------- */

pre_condition(remove_incorrect_builtins(Chtree1,Goal,_Chtree2)) :-
	term_is_of_type(Chtree1,chtree),
	term_is_of_type(Goal,goal).
post_condition(remove_incorrect_builtins(_Chtree1,_Goal,Chtree2)) :-
	term_is_of_type(Chtree2,chtree).

/* remove those built-ins that by abstraction become no longer
   evaluable and generate complex bindings */
/* e.g. X is 5+1  ->  X is 5+Y  Problem ! */
/* e.g. 3\=2      ->   X\=2     No problem because no binding was generated */


remove_incorrect_builtins(empty,_Goal,empty).
remove_incorrect_builtins(stop,_Goal,stop).
remove_incorrect_builtins(success,_Goal,success).
remove_incorrect_builtins(select(SelLitNr,Chpaths),Goal,
			  select(SelLitNr,RChpaths)) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	remove_incorrect_builtins_chpaths(Chpaths,Left,Sel,Right,RChpaths).
remove_incorrect_builtins(remove(SelLitNr,Predicate,SubTree),Goal,
			  remove(SelLitNr,Predicate,RSubTree)) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,_Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	pp_cll(remove_incorrect_builtins(SubTree,NewGoal,RSubTree)).
remove_incorrect_builtins(built_in_eval(SelLitNr,BI,SubTree),Goal,RChtree) :-
	pp_cll(more_specific_transformation(Goal)),
	pp_mnf(split_list(Goal,SelLitNr,Left,Sel,Right)),
	pp_mnf(append(Left,Right,NewGoal)),
	(is_callable_built_in_literal(Sel)
	  -> (call_built_in(Sel),
	      RChtree = built_in_eval(SelLitNr,BI,RSubTree),
	      pp_cll(remove_incorrect_builtins(SubTree,NewGoal,RSubTree))
	     )
	  ;  (built_in_generates_bindings(Sel)
	      -> (RChtree = stop, debug_print(removing(Sel)),debug_nl)
	/* Instead of stop, can we keep/adapt the rest of the tree ?????? */
	      ;  (RChtree = built_in_eval(SelLitNr,BI,RSubTree),
		  pp_cll(remove_incorrect_builtins(SubTree,NewGoal,RSubTree))) 
	     )
	).

remove_incorrect_builtins_chpaths([],_Left,_Sel,_Right,[]).
remove_incorrect_builtins_chpaths([match(Nr,Chtree)|Rest],Left,Sel,Right,
				  [match(Nr,RChtree)|RRest]) :-
	copy(c(Left,Sel,Right),c(L,S,R)),
	claus(Nr,S,Body),
	pp_mnf(append(Body,R,IntGoal)),
	pp_mnf(append(L,IntGoal,NewGoal)),
	pp_mnf(remove_incorrect_builtins(Chtree,NewGoal,RChtree)),
	remove_incorrect_builtins_chpaths(Rest,Left,Sel,Right,RRest).



/* -------------------------------------- */
/* has_builtins_which_generate_bindings/1 */
/* -------------------------------------- */

pre_condition(has_builtins_which_generate_bindings(Chtree1)) :-
	term_is_of_type(Chtree1,chtree).
post_condition(has_builtins_which_generate_bindings(_Chtree1)).


has_builtins_which_generate_bindings(select(_SelLitNr,Chpaths)) :-
	has_builtins_which_generate_bindings_chpaths(Chpaths).
has_builtins_which_generate_bindings(remove(_SelLitNr,_Predicate,SubTree)) :-
	pp_cll(has_builtins_which_generate_bindings(SubTree)).
has_builtins_which_generate_bindings(built_in_eval(_SelLitNr,BI,SubTree)) :-
	(get_predicate(SelBI,BI),built_in_generates_bindings(SelBI))
	;
	(has_builtins_which_generate_bindings(SubTree)).


/* has_builtins_which_generate_bindings_chpaths([]) :- fail. */
has_builtins_which_generate_bindings_chpaths([match(_Nr,Chtree)|Rest]) :-
	(has_builtins_which_generate_bindings(Chtree) ;
	 has_builtins_which_generate_bindings_chpaths(Rest)).


/* ------------ */
/* msg_chtree/3 */
/* ------------ */

pre_condition(msg_chtree(Chtree1,Chtree2,_PChtreeMSG)) :-
	term_is_of_type(Chtree1,chtree),
	term_is_of_type(Chtree2,chtree).
post_condition(msg_chtree(_Chtree1,_Chtree2,PChtreeMSG)) :-
	term_is_of_type(PChtreeMSG,chtree).


msg_chtree(Chtree1,Chtree2,PChtreeMSG) :-
	msg(Chtree1,Chtree2,ChtreeMSG),
	pp_mnf(prune_off_chtree(ChtreeMSG,PChtreeMSG)).

/* the MSG will replace certain subparts by variables, prune_off_chtree
  will replace the affected parts by stop and thus obtain a ground
  term again */



pre_condition(prune_off_chtree(_Chtree,_PChtree)).
post_condition(prune_off_chtree(_Chtree,PChtree)) :-
	term_is_of_type(PChtree,chtree).

prune_off_chtree(X,stop) :- var(X),!.
prune_off_chtree(empty,empty).
prune_off_chtree(stop,stop).
prune_off_chtree(success,success).
prune_off_chtree(select(Nr,Chpaths),Result) :-
	(var(Nr)
	 -> (Result = stop)
	 ;  (prune_off_chpaths(Chpaths,PChpaths),
	      ((PChpaths = stop)
		-> (Result = stop)
		;  (Result = select(Nr,PChpaths))
	      )
	    )
	).
prune_off_chtree(remove(Nr,Pred,Chtree),Result) :-
	((var(Nr) ; var(Pred))
	 -> (Result = stop)
	 ;  (prune_off_chtree(Chtree,PChtree),
	     Result = remove(Nr,Pred,PChtree)
	    )
	).
prune_off_chtree(built_in_eval(Nr,BI,Chtree),Result) :-
	((var(Nr) ; var(BI))
	 -> (Result = stop)
	 ;  (prune_off_chtree(Chtree,PChtree),
	     Result = built_in_eval(Nr,BI,PChtree)
	    )
	).
prune_off_chtree(built_in_simplify(Nr,BI,Chtree),Result) :-
	((var(Nr) ; var(BI))
	 -> (Result = stop)
	 ;  (prune_off_chtree(Chtree,PChtree),
	     Result = built_in_simplify(Nr,BI,PChtree)
	    )
	).

prune_off_chpaths(X,stop) :-
	var(X),!.
prune_off_chpaths([],[]).
prune_off_chpaths([match(Nr,Chtree)|Rest],Result) :-
	(var(Nr)
	 -> (Result = stop)
	 ;  (prune_off_chpaths(Rest,PRest),
		((PRest = stop)
		 -> (Result = stop)
		 ;  (prune_off_chtree(Chtree,PChtree),
		     Result = [match(Nr,PChtree)|PRest]
		    )
		)
	    )
	).



/* tests whether the first chtree can be obtained by extending (futher
   unfolding) the second one */
chtree_is_extension_of(empty,empty).
chtree_is_extension_of(_X,stop).
chtree_is_extension_of(success,success).
chtree_is_extension_of(select(Nr,Chpaths1),select(Nr,Chpaths2)) :-
	chpaths_are_extension_of(Chpaths1,Chpaths2).
chtree_is_extension_of(remove(Nr,Pred,Chtree1),
		       remove(Nr,Pred,Chtree2)) :-
	chtree_is_extension_of(Chtree1,Chtree2).

chpaths_are_extension_of([],[]).
chpaths_are_extension_of([match(Nr,Chtree1)|Rest1],[match(Nr,Chtree2)|Rest2]) :-
	chtree_is_extension_of(Chtree1,Chtree2),
	chpaths_are_extension_of(Rest1,Rest2).


/* --------------------------- */
/* transform_chtree_for_homo/2 */
/* --------------------------- */

/* transform Nr of selected literal into variable to ensure termination
	for conjunctive case ! */

transform_chtree_for_homo(Ch,Ch) :-!.  % <<< modified by MV >>>
%transform_chtree_for_homo(Ch,Ch) :-
%	lift_conjunctions(no),!.
transform_chtree_for_homo(Ch,HCh) :-
	trans_chtree(Ch,HCh).

trans_chtree(empty,empty).
trans_chtree(stop,stop(success)). /* make sure stop embeds in succes */
trans_chtree(success,success).
trans_chtree(select(_Nr,Paths),select(_V,HPaths)) :-
	/* make sure all literal numbers embed each other */
	trans_paths(Paths,HPaths).
trans_chtree(remove(_Nr,Pred,Chtree),remove(_V,Pred,HChtree)) :-
	trans_chtree(Chtree,HChtree).
trans_chtree(built_in_eval(_Nr,BI,Chtree),built_in_eval(_V,BI,HChtree)) :-
	trans_chtree(Chtree,HChtree).
trans_chtree(built_in_simplify(_Nr,BI,Chtree),
	     built_in_simplify(_V,BI,HChtree)) :-
	trans_chtree(Chtree,HChtree).

trans_paths([],[]).
trans_paths([match(Nr,Chtree)|Rest],[match(Nr,HChtree)|HRest]) :-
	trans_chtree(Chtree,HChtree),
	trans_paths(Rest,HRest).


/* ===================================== */
/* BETTER MSG USING Characteristic Terms */
/* ===================================== */


pre_condition(precise_msg_chtree(Chtree1,Chtree2,_PChtreeMSG)) :-
	term_is_of_type(Chtree1,chtree),
	term_is_of_type(Chtree2,chtree).
post_condition(precise_msg_chtree(_Chtree1,_Chtree2,PChtreeMSG)) :-
	term_is_of_type(PChtreeMSG,chtree).


precise_msg_chtree(Chtree1,Chtree2,PChtreeMSG) :-
	pp_mnf(transform_chtree_into_chterm(Chtree1,Chterm1)),
	pp_mnf(transform_chtree_into_chterm(Chtree2,Chterm2)),
	msg(Chterm1,Chterm2,MSGChterm),
	pp_mnf(transform_chterm_into_chtree(MSGChterm,PChtreeMSG)).


/* ---------------------------- */
/* transform_chtree_into_chterm */
/* ---------------------------- */

ecce_type(chterm,term(empty,[])).
ecce_type(chterm,term(success,[])).
ecce_type(chterm,term(stop,[term(success,[])])).
ecce_type(chterm,term(nomatch,[])).
ecce_type(chterm,term(select,[selected_literal_nr,match_term])).
ecce_type(chterm,term(remove,[selected_literal_nr,predicate,chterm])).
ecce_type(chterm,term(built_in_eval,[selected_literal_nr,predicate,chterm])).

ecce_type(match_term,nonvar).


pre_condition(transform_chtree_into_chterm(Chtree,_Chterm)) :-
	term_is_of_type(Chtree,chtree).
post_condition(transform_chtree_into_chterm(_Chtree,Chterm)) :-
	term_is_of_type(Chterm,chterm).
	/* print(transform_chtree_into_chterm(Chtree,Chterm)),nl. */

	
transform_chtree_into_chterm(empty,empty).
transform_chtree_into_chterm(stop,stop(success)).
	/* make sure stop embeds succes */
transform_chtree_into_chterm(success,success).
transform_chtree_into_chterm(select(Nr,Paths),Chterm) :-
	((Paths = [])
	 -> (Chterm = empty)
	 ;  (Chterm = select(Nr,HPaths),
	     trans_paths_matchterm(Paths,HPaths))
	).
transform_chtree_into_chterm(remove(Nr,Pred,Chtree),
			     remove(Nr,Pred,HChtree)) :-
	pp_mnf(transform_chtree_into_chterm(Chtree,HChtree)).
transform_chtree_into_chterm(built_in_eval(Nr,BI,Chtree),
			     built_in_eval(Nr,BI,HChtree)) :-
	pp_mnf(transform_chtree_into_chterm(Chtree,HChtree)).
transform_chtree_into_chterm(built_in_simplify(Nr,BI,Chtree),
	     		     built_in_simplify(Nr,BI,HChtree)) :-
	pp_mnf(transform_chtree_into_chterm(Chtree,HChtree)).


trans_paths_matchterm([match(Nr,Chtree)|Rest],MatchTerm) :-
	claus(Nr,Head,_Body),
	Head =.. [Pred|Args],
	generate_variable_arguments(Args,VarArgs),
	VarHead =.. [Pred|VarArgs],
	findall( ClausNr, claus(ClausNr,VarHead,_VB), ClauseNrs),
	generate_list_of_empty(ClauseNrs,EmptyMatches),
	construct_match_list([match(Nr,Chtree)|Rest],ClauseNrs,
				EmptyMatches,Matches),
	MatchTerm =.. [match,VarHead,ClauseNrs|Matches].
	
construct_match_list([],_ClauseNrs,Matches,Matches).
construct_match_list([match(Nr,Chtree)|Rest],ClauseNrs,InMatches,OutMatches) :-
	pp_mnf(transform_chtree_into_chterm(Chtree,ChTerm)),
	replace_in_match_list(Nr,ChTerm,ClauseNrs,InMatches,IntMatches),
	construct_match_list(Rest,ClauseNrs,IntMatches,OutMatches).

generate_variable_arguments([],[]).
generate_variable_arguments([_H|T],[_Var|VT]) :-
	generate_variable_arguments(T,VT).

generate_list_of_empty([],[]).
generate_list_of_empty([_H|T],[nomatch|VT]) :-
	generate_list_of_empty(T,VT).

replace_in_match_list(Nr,ChTerm,[],InMatches,OutMatches) :-
	print('### ERROR, no match in replace_in_match_list in chtree_tools.pl'),nl,
	print(replace_in_match_list(Nr,ChTerm,[],InMatches,OutMatches)),nl,
	OutMatches=InMatches.
replace_in_match_list(Nr,ChTerm,[Nr|_R],[M|RestM],[ChTerm|RestM]) :-
	((M=nomatch) -> (true)
	 ; (print('### ERROR: Double assignment in replace_in_match_list in chtree_tools.pl'),nl)
	),!.
replace_in_match_list(Nr,ChTerm,[Nr2|R],[M|RestM],[M|RRestM]) :-
	not(Nr=Nr2),
	replace_in_match_list(Nr,ChTerm,R,RestM,RRestM).



/* ---------------------------- */
/* transform_chterm_into_chtree */
/* ---------------------------- */



pre_condition(transform_chterm_into_chtree(_Chterm,_Chtree)).
post_condition(transform_chterm_into_chtree(_Chterm,Chtree)) :-
	term_is_of_type(Chtree,chtree).

transform_chterm_into_chtree(X,stop) :- var(X),!.
transform_chterm_into_chtree(empty,empty).
transform_chterm_into_chtree(stop(success),stop).
transform_chterm_into_chtree(success,success).
transform_chterm_into_chtree(select(Nr,MatchTerm),stop) :-
	(var(Nr) ; var(MatchTerm)), !.
transform_chterm_into_chtree(select(Nr,MatchTerm),select(Nr,Paths)) :-
	MatchTerm =.. [match,_VarAtom,ClauseNrs|SubChterms],
	transform_subchterms_into_path(ClauseNrs,SubChterms,Paths).
transform_chterm_into_chtree(remove(Nr,Pred,_Chtree),stop) :-
	(var(Nr) ; var(Pred)), !.
transform_chterm_into_chtree(remove(Nr,Pred,Chtree),
			     remove(Nr,Pred,HChtree)) :-
	pp_mnf(transform_chterm_into_chtree(Chtree,HChtree)).
transform_chterm_into_chtree(built_in_eval(Nr,BI,_Chtree),stop) :-
	(var(Nr) ; var(BI)), !.
transform_chterm_into_chtree(built_in_eval(Nr,BI,Chtree),
			     built_in_eval(Nr,BI,HChtree)) :-
	pp_mnf(transform_chterm_into_chtree(Chtree,HChtree)).
transform_chterm_into_chtree(built_in_simplify(Nr,BI,_Chtree),stop) :-
	(var(Nr) ; var(BI)), !.
transform_chterm_into_chtree(built_in_simplify(Nr,BI,Chtree),
	     		     built_in_simplify(Nr,BI,HChtree)) :-
	pp_mnf(transform_chterm_into_chtree(Chtree,HChtree)).


transform_subchterms_into_path(X,Y,[]) :- (var(X) ; var(Y)),!,
	print('### ERROR in transform_subchterms_into_path, args variables'),nl.
transform_subchterms_into_path([],[],[]).
transform_subchterms_into_path([CNr|RestC],[Chterm|RestChterms],Paths) :-
	pp_mnf(transform_chterm_into_chtree(Chterm,Chtree)),
	((Chtree=nomatch)
	 -> (Paths = RestPaths)
	 ;  (Paths = [match(CNr,Chtree)|RestPaths])
	),
	transform_subchterms_into_path(RestC,RestChterms,RestPaths).


/* ---------------------------------------------------------- */

pre_condition(calc_pruning_factor(Chtree,_PF)) :-
	term_is_of_type(Chtree,chtree).
post_condition(calc_pruning_factor(_Chtree,PF)) :-
	term_is_of_type(PF,real).

	
calc_pruning_factor(empty,1.0).
calc_pruning_factor(stop,0.0).
calc_pruning_factor(success,0.0).
calc_pruning_factor(select(_Nr,Paths),PF) :-
	((Paths = [])
	 -> (PF = 1.0)
	 ;  (calc_pruning_factor_paths(Paths,PF))
	).
calc_pruning_factor(remove(_Nr,_Pred,Chtree),PF) :-
	pp_mnf(calc_pruning_factor(Chtree,CPF)),
	PF is CPF + 1.0.
calc_pruning_factor(built_in_eval(_Nr,_BI,Chtree),PF) :-
	pp_mnf(calc_pruning_factor(Chtree,CPF)),
	PF is CPF + 1.0.
calc_pruning_factor(built_in_simplify(_Nr,_BI,Chtree),PF) :-
	pp_mnf(calc_pruning_factor(Chtree,PF)).


calc_pruning_factor_paths([match(Nr,Chtree)|Rest],PF) :-
	claus(Nr,Head,_Body),
	Head =.. [Pred|Args],
	generate_variable_arguments(Args,VarArgs),
	VarHead =.. [Pred|VarArgs],
	findall( ClausNr, claus(ClausNr,VarHead,_VB), ClauseNrs),
	length(ClauseNrs,NrOfClauses),
	length([match(Nr,Chtree)|Rest],NrOfMatches),
	l_calc_pruning_factor([match(Nr,Chtree)|Rest],0,SumPF),
	PF is (0.0 + NrOfClauses - NrOfMatches + SumPF) /NrOfClauses.

l_calc_pruning_factor([],Acc,Acc).
l_calc_pruning_factor([match(_Nr,Chtree)|Rest],Acc,PF) :-
	pp_mnf(calc_pruning_factor(Chtree,PFC)),
	NewAcc is Acc + PFC,
	l_calc_pruning_factor(Rest,NewAcc,PF).
