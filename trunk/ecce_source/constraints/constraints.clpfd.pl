:- module('constraints.clpfd',_).
/* -------------------------------------------- */
/* Constraint manipulation routines for CLP(FD) */
/* -------------------------------------------- */

%%%
%%% Ciao does not support CLPFD!
%%%

:- use_module(library(clpfd)).

:- assert(clpfd:full_answer).


/*
 entails(p(X,Y),['#>'(X,Y)],p(Z,2),['#>'(Z,3)]). -> no
 entails(p(Z,2),['#>'(Z,3)],p(X,Y),['#>'(X,Y)]). -> true */

/* --------- */
/* entails/4 */
/* --------- */

 /* Example:
    ?- entails(p(X),[X #= Z+1, Z #= Y+1, Y#>1],p(V),[V#= W+1, W#>1]).
 */

 /* entails([X#>2, Y#>X], [Y#>1,X#>3]). */
 /* entails([X#>2, Y#>X], [Y#>1,X#>1]). */
 
entails(CGoal1,CGoal2) :-
	copy((CGoal1,CGoal2),(C1,C2)),
	assert_constraint(C1), print(asserted(C1)),nl,
	entailed(C2).

entails(OGoal1,CGoal1,OGoal2,CGoal2) :-
	/* only call if OGoal1 is instance of OGoal2 */
	copy((OGoal1,CGoal1),(O1,C1)),
	copy((OGoal2,CGoal2),(O2,C2)),
	O1 = O2,
	copy(O2,O2Copy),
	project_and_check_constraint(O2,C2,ProjC2),
	variant_of(O2,O2Copy), /* if not variant: instantiation happened,
	 meaning that the constraint is not entailed */
	assert_constraint(C1), print(asserted(C1)),nl,
	entailed(ProjC2).

assert_constraint([]).
assert_constraint([H|T]) :- 
	call(H), 
	assert_constraint(T).

check_negated([],_).
check_negated([C|T],Vars) :-
	\+(assert_negated(C,Vars)),
	  /* check that negation of C is not satisfiable */
	check_negated(T,Vars).

assert_negated(C,_Vars) :-
	call(C #<=> 0).
	/* varlist(Vars,RVars), vars might have become instantiated 
	labeling([],RVars). */

entailed([]).
entailed([C|T]) :- debug_print(checking(C)),debug_nl,
	\+(assert_negated(C,[])),debug_print(ent(C)),debug_nl,
	entailed(T).


/* widen(p(X),[X #> 2, X#<9],p(Z),[Z#>3],R,RC). */
/* widen(p(X,Y),[X#>8,Y#>8],p(V,W),[V2#>8,V#=V2+1,W2#>8,W#=W2-1],RG,RC). */
/* widen(p(X2,Y2),[X2#=X+1,X#>8,Y2#=Y-1,Y#>8],
         p(V3,W3),[V3#=V+1,V2#>8,V#=V2+1,W3#=W-1,W2#>8,W#=W2-1],RG,RC). */
/* widen(p(X,Y),[X#>8,Y#>8],p(V,W),[V#>9,W#>7],RG,RC). */
/* widen(p(X),[X#>2],p(8),[],RG,RC). */
/* widen(p(X),[X#>2,X#<10],p(8),[],RG,RC). */

widen(AncGoal,AncestorConstraint,DescGoal,DescConstraint,ResG,ResC) :-
	/* assume MSG can be taken of atom/conjunction */
      copy((AncGoal,AncestorConstraint),(AG,AC)),
      project_and_check_constraint(AG,AC,PAC),
      normalise_constraint(PAC,NPAC),
      print(normalise_constraint(PAC,NPAC)),
      copy((DescGoal,DescConstraint),(DG,DC)),
      /* assert_constraint(DC),!, print(asserted(DC)),nl, */
      debug_print(widen2_start(AG,NPAC,DG,DC,RG,RC)),debug_nl,
      widen2(AG,NPAC,DG,DC,RG,RC),
      debug_print(widen2_res(AG,NPAC,DG,DC,RG,RC)),debug_nl,
      fd_copy_term((RG,RC),(ResG,ResC),_T),
      debug_print(widen2_cpy(ResG,ResC,_T)),debug_nl.
widen(AG,_AC,DG,_DC,MSG,[]) :- msg(AG,DG,MSG).

widen2(AG,AC,DG,DC,RG,RC) :-
	var(AG),(var(DG);integer(DG)),!, debug_print(var(AG,AC,DG,DC)),debug_nl,
	keep_entailed([AG=DG|DC],AC,RC),
	debug_print(kept(RC)),nl,
	RG = AG,debug_print(ok),nl.
widen2(AG,AC,DG,DC,RG,RC) :-
	var(AG),nonvar(DG),!, debug_print(v_nv),debug_nl,
	RC=[],RG=_.
widen2(AG,AC,DG,DC,RG,RC) :-
	integer(AG),var(DG),!, debug_print(int_v),debug_nl,
	keep_entailed(DC,[DG#=AG],RC),
	RG=DG.
widen2(AG,AC,DG,DC,RG,RC) :-
	nonvar(AG),var(DG),!, debug_print(nv_v),debug_nl,
	RC=[],RG=_.
widen2(AG,AC,DG,DC,RG,RC) :-
	AG =.. [P|AArgs],
	DG =.. [P|DArgs],same_length(AArgs,DArgs),!,
	debug_print(func(P)),debug_nl,
	l_widen2(AArgs,AC,DArgs,DC,RArgs,RC),
	debug_print(func_res(P,RArgs,RC)),debug_nl,
	RG =.. [P|RArgs], debug_print(rg(RG)),debug_nl.
widen2(_AG,_AC,_DG,_DC,_RG,[]) :- debug_print(base),debug_nl.

l_widen2([],_,[],_,[],[]).
l_widen2([AH|AT],AC,[DH|DT],DC,[RH|RT],RC) :-
	widen2(AH,AC,DH,DC,RH,RC1),
	l_widen2(AT,AC,DT,DC,RT,RC2),
	append(RC1,RC2,RC).


keep_entailed(_DC,[],[]).
keep_entailed(DC,[C|T],Res) :- print(checking(DC,C)),nl,
	(entails(DC,[C]) -> (Res = [C|KT], print(keep(C)),nl)
	               ;  (Res = KT, print(rem(C)),nl)),
	keep_entailed(DC,T,KT).

:- dynamic temp_clpfd_pred/1.


normalise_constraint([],[]).
normalise_constraint([in(Var,'..'(Low,Up))|T],Res) :- !,
	((Low=inf) ->(Res=Res1) ; (Res = [Var#>=Low|Res1])),
	((Up=sup)  ->(Res1=NT)  ; (Res1 = [Var#=<Up | NT])),
	normalise_constraint(T,NT).
normalise_constraint([C|T],[C|NT]) :-
	normalise_constraint(T,NT).

/* Example:
?- project_and_check_constraint(p(X,Y),[X#>1,Y#>2,Z#>3],C).
*/

project_and_check_constraint(Goal,Constraint,ProjConstraint) :-
	varlist(Goal,Vars),
	retractall(temp_clpfd_pred(_)),
	assert(( temp_clpfd_pred(Vars) :- assert_constraint(Constraint))),
	call_residue(temp_clpfd_pred(Vars),CallResidue),debug_nl,
	debug_print(call_residue(CallResidue)),debug_nl,
	debug_print(calling_convert(Vars)),debug_nl,
	convert_call_residue(Vars,CallResidue,ProjConstraint).

convert_call_residue(_,[],[]).
convert_call_residue(Vars,['-'([Var|_],Constr)|T],Res) :-
	debug_print(check(Var)),debug_nl,
	(exact_member(Var,Vars)
	   /* improve: check whether relationship necessary */
	 -> (Res = [Constr|CT]) ; (Res = CT, debug_print(rem),debug_nl)
	),
	convert_call_residue(Vars,T,CT).


/* ------------------------ */
/* satisfiable_constraint/1 */
/* ------------------------ */

satisfiable_constraint(CGoal) :-
       copy(CGoal,C),
       satisfiable(C).

satisfiable([]).
satisfiable([C|T]) :-
	call(C #<=> BFlag),
	BFlag = 1, print(sat(C)),nl,
	satisfiable(T).


/* ------------------------ */
/*   project_constraint/3   */
/* ------------------------ */

/* has to be improved !!!! */

project_constraint([],_,[]).
project_constraint([C|T],Term,Res) :-
	(sharing(C,Term)
	 -> (Res = [C|TR]) ; (Res = TR)
	),
	project_constraint(T,Term,TR).