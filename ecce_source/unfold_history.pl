:- module(unfold_history,[update_unfold_history/4,ancestor/2,covering_ancestor/3,contains_non_determinate_step/1,contains_non_determinate_step_at_top/1]).

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

:- use_module(bimtools).

/* file: unfold_history.pro */

:- include( multi_meta ).

:- use_package( .(ecce_no_rt) ).

/* ========================================= */
/*       UNFOLD HISTORY RELATED STUFF        */
/* ========================================= */

/* ++++++++++++++++ */
/* TYPE DEFINITIONS */
/* ++++++++++++++++ */
ecce_type(unfold_history,list(unfold_entry)).
ecce_type(unfold_entry,term(sel,[sel_atom,sel_pos,nr_added_atoms,det_info])).
ecce_type(sel_atom,nonvar).
ecce_type(sel_pos,integer).
ecce_type(nr_added_atoms,integer).
ecce_type(det_info,term(det,[])).
ecce_type(det_info,term(nondet,[])).
ecce_type(det_info,term(bi,[])).
ecce_type(det_info,term(neg,[])).
/* ++++++++++++++++ */

/* ----------------------- */
/* update_unfold_history/4 */
/* ----------------------- */

/* just add the number of body literals to the unfold history to be
	able to calculate covering atoms */

pre_condition(update_unfold_history(OldHist,NrOfSelLit,Body,_NewHist)) :-
	term_is_of_type(OldHist,unfold_history),
	term_is_of_type(NrOfSelLit,integer),
	term_is_of_type(Body,goal).
post_condition(update_unfold_history(_OldHist,_NrOfSelLit,_Body,NewHist)) :-
	term_is_of_type(NewHist,unfold_history).

update_unfold_history([sel(SelAtom,SelPos,0,DI)|Rest],
			_NrOfSel,Body,NewHist) :-
	length(Body,AddedLiterals),
	NewHist = [sel(SelAtom,SelPos,AddedLiterals,DI)|Rest].


/* ---------- */
/* ancestor/2 */
/* ---------- */


pre_condition(ancestor(UnfHist,_CoveringAncestor)) :-
	term_is_of_type(UnfHist,unfold_history).
post_condition(ancestor(_UnfHist,CoveringAncestor)) :-
	term_is_of_type(CoveringAncestor,literal).

ancestor([sel(AncSelLit,_Pos,_AddedLits,_DI)|_Rest],AncSelLit).
ancestor([sel(_SomeAncSelLit,_Pos,_AddedLits,_DI)|Rest],AncSelLit) :-
	ancestor(Rest,AncSelLit). /* look higher up */


/* ------------------- */
/* covering_ancestor/3 */
/* ------------------- */

/* succeeds once for every covering ancestor of the literal SelAtom
   which is at position SelPos */

pre_condition(covering_ancestor(SelPos,UnfHist,_CoveringAncestor)) :-
	term_is_of_type(UnfHist,unfold_history),
	term_is_of_type(SelPos,integer).
post_condition(covering_ancestor(_SelPos,_UnfHist,CoveringAncestor)) :-
	term_is_of_type(CoveringAncestor,literal).

covering_ancestor(SelPos,[sel(AncSelLit,Pos,AddedLits,_DI)|_Rest],AncSelLit) :-
	created_by(SelPos,Pos,AddedLits). /* we found one */
covering_ancestor(SelPos,[sel(_SomeAncSelLit,Pos,AddedLits,_DI)|Rest],AncSelLit) :-
	((SelPos < Pos)
	 -> (NewPos = SelPos)
	 ;  (created_by(SelPos,Pos,AddedLits)
	     -> (NewPos is Pos)
	     ;  (NewPos is SelPos + 1 - AddedLits))
	),
	covering_ancestor(NewPos,Rest,AncSelLit). /* look higher up */


created_by(X,Y,Z) :-
	YpZ is Y + Z,
	X >= Y,
	X < YpZ,!.


/* ------------------------------- */
/* contains_non_determinate_step/1 */
/* ------------------------------- */

pre_condition(contains_non_determinate_step(UnfHist)) :-
	term_is_of_type(UnfHist,unfold_history).
post_condition(contains_non_determinate_step(_UnfHist)).

contains_non_determinate_step([sel(_AncSelLit,_Pos,_AddedLits,nondet)|_Rest]).
contains_non_determinate_step([sel(_AncSelLit,_Pos,_AddedLits,_DI)|Rest]) :-
	contains_non_determinate_step(Rest).

/* -------------------------------------- */
/* contains_non_determinate_step_at_top/1 */
/* -------------------------------------- */

pre_condition(contains_non_determinate_step_at_top(UnfHist)) :-
	term_is_of_type(UnfHist,unfold_history).
post_condition(contains_non_determinate_step_at_top(_UnfHist)).

contains_non_determinate_step_at_top([sel(_AncSelLit,_Pos,_AddedLits,nondet)]).
contains_non_determinate_step_at_top([sel(_AncSelLit,_Pos,_AddedLits,DI)|Rest]) :-
	DI \= nondet,
	contains_non_determinate_step_at_top(Rest).
