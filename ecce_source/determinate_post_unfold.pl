:- module(determinate_post_unfold,[perform_determinate_post_unfolding/0,calculate_post_unfolded_clauses/0,post_unfold_body/3,post_unfold_literal/3,purely_undeterminate/1]).


:- use_package( .(ecce_no_rt) ).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- use_module(library(lists)).


:- use_module(bimtools).
:- use_module(code_generator).
:- use_module(calc_chtree).


/* file: determinate_post_unfold.pro */


perform_determinate_post_unfolding :-
	reset_spec_prog,
	print('Post Unfolded Program:'),nl,
	calc_post_unfolded_clauses,
	print_specialised_program.

calculate_post_unfolded_clauses :-
	reset_spec_prog,
	calc_post_unfolded_clauses.

calc_post_unfolded_clauses :-
	claus(_Nr,Head,Body),
	(post_unfold_body(Body,NewBody,5)
	 -> assert_spec_clause(Head,NewBody)
	 ;  assert_spec_clause(Head,[fail])
	),
	fail.
calc_post_unfolded_clauses :- verbose_nl.


post_unfold_body([],[],_).
post_unfold_body([Lit|T],Res,MaxDepth) :-
	post_unfold_literal(Lit,PGoal,MaxDepth),
	post_unfold_body(T,PT,MaxDepth),
	append(PGoal,PT,Res).


post_unfold_literal(Lit,[],_) :-
	is_callable_built_in_literal(Lit),!,call_built_in(Lit).
post_unfold_literal(Lit,[Lit],_) :-
	is_built_in_literal(Lit),!.
post_unfold_literal(Lit,[Lit],_) :-
	is_negative_literal(Lit,_Atom),!.
post_unfold_literal(Lit,NewGoal,Depth) :-
	(purely_undeterminate(Lit)
	 -> (NewGoal = [Lit])
	 ;  (claus(_Nr,Lit,Body),
	     ((Depth>0)
		-> (length(Body,Len),
		    D1 is Depth - Len,
		    verbose_print('.'),
		    post_unfold_body(Body,NewGoal,D1)
		   )
		;  (NewGoal = Body)
	     )
	    )
	).

purely_undeterminate(Lit) :-
	copy(Lit,CpLit),
	(claus(Nr,CpLit,_Body)
	 -> (copy(Lit,CpLit2),
	     claus(Nr2,CpLit2,_Body2),
	     not(Nr=Nr2))
	 ;  (true /* it fails */)
	).
