:- module('more_specific.rul',['more_specific.rul:more_specific_transformation'/1]).

%:- use_module('../rul/INTERFACE').

:- dynamic more_specific_transformation/1.

%%%:- ecce_use_module('rul/ecceRUL').
%%%:- ecce_use_module('rul/unfold2').

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').

/* instantiates goals during the unfolding process to more specific versions */

'more_specific.rul:more_specific_transformation'(Goal) :-
	divide_constraint_rul_goal(Goal,OGoal,CGoal),
	project_and_check_constraint(OGoal,CGoal,_),
        copy_term(OGoal,O),
	l_goalUnfolding(OGoal,CGoal),
	(variant_of(O,OGoal) -> true ;
	     (print('**** -> '),nl,print(mst2(O,OGoal,CGoal)),nl,nl)),
	more_specific_transformation_classic(OGoal).

more_specific_transformation_classic([]).
more_specific_transformation_classic([CH|T]) :-
	peel_off_calls(CH,H),
	mst_instantiate_atom(H),!,
	more_specific_transformation_classic(T).

mst_instantiate_atom(Atom) :-
	mst_matching_heads(Atom,Heads),!,
	((Heads = [])
	 -> (true)  /* do nothing, dead literals will be detected anyway, and the atom could be a built-in or a negative literal */
	;  (mst_msg_of_list(Heads,MsgOfHeads),
	    Atom=MsgOfHeads)
	).

mst_matching_heads(Atom,Heads) :-
	bd_findall(Atom,claus(_Nr,Atom,_Body),Heads).

mst_msg_of_list([Atom1|Tail],MSG) :-
	mst_l_msg(Atom1,Tail,MSG).
mst_l_msg(Atom,[],Atom).
mst_l_msg(Atom,[Atom2|T],MSG) :-
	msg(Atom,Atom2,IntMSG),!,
	mst_l_msg(IntMSG,T,MSG).
	
