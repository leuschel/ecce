:- module('partition.rul',['partition.rul:partition_goal'/3]).

%:- use_module('../rul/INTERFACE').

%:- dynamic partition_goal/3.

%%:- ecce_use_module('rul/ecceRUL').
%%:- ecce_use_module('rul/analyticFold').

%:- initialization(retractall(rul_active(_)),assert(rul_active(yes))).

:- set_prolog_flag(multi_arity_warnings,off).
%:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').

:- use_module( 'partition.common' ).

'partition.rul:partition_goal'Goal,Nrs,ResSplitGoal) :-
    debug_nl,debug_nl,debug_print('partition: '),
    print_goal_for_dot(Goal),nl,
	debug_println(start_partition_goal(Goal,Nrs,ResSplitGoal)),
	divide_constraint_goal_with_nrs(Goal,Nrs,OGoal,ONrs,CGoal,CNrs),
	partition_goal_classic_conj(OGoal,ONrs,SplitOGoal),
	/* print(calling_proj(SplitOGoal,CGoal,CNrs)),nl,*/
	par_project_constraints(SplitOGoal,CGoal,CNrs,ResSplitGoal),
        debug_print(result_of_partition_goal(Goal,Nrs,ResSplitGoal)),trace_nl,
     debug_println(result(Nrs)).

wrap_into_built_in([],[]).
wrap_into_built_in([H|T],[built_in(H)|WT]) :-
	wrap_into_built_in(T,WT).

par_project_constraints([],_,_,[]).
par_project_constraints([split_goal(Goal,SplitInd)|T],CGoal,CNrs,
		    [split_goal(NewGoal,NewInd)|ST]) :-
	((SplitInd = [_|_])
	 -> (debug_println(call_proj_with_nrs(CGoal,CNrs,Goal,PCG,PCN,NGoal)),
             project_constraint_with_nrs(CGoal,CNrs,Goal,PCG,PCN,NGoal),
             /* print(res_proj_with_nrs(CGoal,CNrs,Goal,PCG,PCN,NGoal)),nl,*/
	     append(NGoal,PCG,NewGoal),
	     append(SplitInd,PCN,NewInd)
	    )
	 ;  (NewGoal = Goal, NewInd = SplitInd)
	), /* print(split),print_goal_for_dot(NewGoal),nl, */
	par_project_constraints(T,CGoal,CNrs,ST).



project_constraint_with_nrs([],_,Goal,
			    [NewC],[],NewGoal) :- !, 
       debug_println(one([],Goal)),
       
       project_RULify(Goal,NewGoal,rul__constraint__declaration([],[]),NewC),
       
       (debug_printing(on) -> print_rul(partition(NewGoal),NewC) ; true).
   /* generate empty constraint if none present */

project_constraint_with_nrs(CGoal,CNrs,Goal,[NewC],CNrs,NewGoal) :-
        debug_println(two(CGoal,Goal)),
	CGoal = [RULC],
	RULC = rul__constraint__declaration(_,_),!,
	
	project_RULify(Goal,NewGoal,RULC,NewC),
	
        (trace_printing(on) -> print_rul(partition(NewGoal),NewC) ; true),
	debug_nl.

project_constraint_with_nrs(CGoal,CNrs,Goal,CGoal,CNrs,Goal) :-
	print('### error in'),nl,
	print(project_constraint_with_nrs(CGoal,CNrs,Goal,CGoal,CNrs,Goal)),nl.



:- dynamic automatic_RULify/1.
automatic_RULify(no).

project_RULify(Goal,Goal,Rul,Rul) :- automatic_RULify(no).
project_RULify(Goal,NewGoal,RULC,New) :- automatic_RULify(yes),
       debug_println(call(l_goalRULification(Goal,NewGoal,RULC,NewC))),
       l_goalRULification(Goal,NewGoal,RULC,NewC),
       debug_println(rul(NewC)).



divide_constraint_goal_with_nrs([],[],[],[],[],[]).
divide_constraint_goal_with_nrs([Lit|T],[SelNr|NT],
		       OrdLits,OrdNrs,ConstrLits,ConstrNrs) :-
   (is_constraint_literal(Lit)
    -> (ConstrLits = [Lit|C2], ConstrNrs = [SelNr|CN2],
	O2 = OrdLits, ON2 = OrdNrs)
    ;  (OrdLits = [Lit|O2], OrdNrs = [SelNr|ON2],
	C2 = ConstrLits, CN2 = ConstrNrs)
   ),
   divide_constraint_goal_with_nrs(T,NT,O2,ON2,C2,CN2).




partition_goal_classic_conj([],[],[]).
partition_goal_classic_conj([Lit|T],[SelNr|TN],SGoal) :-
	is_built_in_literal(Lit),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],built_in(SelNr))|ST])
	 ;  (SGoal = ST) /* do not start conjunction with built-in */
	),
	partition_goal_classic_conj(T,TN,ST).
partition_goal_classic_conj([Lit|T],[SelNr|TN],SGoal) :-
	is_negative_literal(Lit,_Atom),!,
	(extract_positive_atom_from_literal(Lit,PosAtom)
	 -> (SGoal = [split_goal([PosAtom],neg(SelNr))|ST])
	 ;  (SGoal = ST) /* do not start conjunction with negation */
	),
	partition_goal_classic_conj(T,TN,ST).
partition_goal_classic_conj([Atom|T],[SelNr|TN],
		[ split_goal([Atom|InBlock],[SelNr|InNrs]) | ST]) :-
	get_block(T,TN,[Atom],InBlock,NotInBlock,InNrs,NotInNrs),
	partition_goal_classic_conj(NotInBlock,NotInNrs,ST).


get_block_standard_pd(T,TN,_,[],T,[],TN).



