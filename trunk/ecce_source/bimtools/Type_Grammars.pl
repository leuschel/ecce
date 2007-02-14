/*
:- initialization(ensure_consulted('$BIMTOOLS_PATH/gensym.pro')).
%:- initialization(ensure_consulted('$BIMTOOLS_PATH/claus_database.pro')).
%:- initialization(ensure_consulted('$BIMTOOLS_PATH/instance.pro')).
:- initialization(ensure_consulted('$BIMTOOLS_PATH/StdLists.pro')).
:- initialization(ensure_consulted('$BIMTOOLS_PATH/Environments.pro')).
:- initialization(ensure_consulted('$BIMTOOLS_PATH/typechecker.pro')).
:- initialization(ensure_consulted('$BIMTOOLS_PATH/prepost.pro')).
:- initialization(ensure_consulted('$BIMTOOLS_PATH/debugging.pro')).
*/

/* ================== */
/* PRINTING FUNCTIONS */
/* ================== */

/* --------------------------- */
/* print_type_environment(Env) */
/* --------------------------- */
print_type_environment(Env) :- var(Env),!,
	print('### environment is a variable !'),nl.
print_type_environment(Env) :-
	lookup(Var,Env,ecce_type(Label,Grammar)),
	print(Var),print(':'),print(Label),print(' where '),nl,
	print_type_grammar(Grammar),
	fail.
print_type_environment(_E) :- nl.

/* --------------------------- */
/* print_type_grammar(Grammar) */
/* --------------------------- */
print_type_grammar(G) :- var(G),!,
	print('### grammar is a variable !'),nl.
print_type_grammar(Gramm) :-
	lookup(Label,Gramm,Rule),
	print(' '),print(Label),print(' --> '),
	print_type_rule(Rule),
	fail.
print_type_grammar(_G).

print_type_rule(R) :- var(R),!,
	print('### rule is a variable !'),nl.
print_type_rule(Rule) :-
	lookup(Constructor,Rule,Args),
	print(Constructor),
	((Args = [])
	 -> true
	 ; print(Args)
	),
	print(' | '),
	fail.
print_type_rule(_R) :- nl.


/* ======================= */
/* COMBINING TYPE-GRAMMARS */
/* ======================= */

/* ------------------------------------- */
/* intersection_type(T1,G1,T2,G2,UT,UG) */
/* ------------------------------------- */

intersection_type(T1,G1,T2,G2,InUGramm,OutUGramm) :-
	combine_type_label(T1,T2,T1T2),
	(lookup(T1T2,InUGramm,_RulesForT1T2)
	 -> (OutUGramm = InUGramm)
	 ;  (findall(UR,unify_rule(T1,G1,T2,G2,UR),Rules),
	     generate_unified_rules(Rules,[],T1T2Rules),
	     store(InUGramm,T1T2,T1T2Rules,InUGramm1),
	     unify_arguments(Rules,G1,G2,InUGramm1,OutUGramm)
	    )
	).
l_intersection_type([],[],_G1,_G2,G,G).
l_intersection_type([T1|H1],[T2|H2],G1,G2,IG,OG) :-
	intersection_type(T1,G1,T2,G2,IG,IG1),
	l_intersection_type(H1,H2,G1,G2,IG1,OG).

unify_arguments([],_G1,_G2,UGramm,UGramm).
unify_arguments([unify(Constructor,A1,A2)|T],G1,G2,InUGramm,OutUGramm) :-
	l_intersection_type(A1,A2,G1,G2,InUGramm,InUGramm1),
	unify_arguments(T,G1,G2,InUGramm1,OutUGramm).

generate_unified_rules([],URules,URules).
generate_unified_rules([unify(Constructor,A1,A2)|T],InURules,OutURules) :-
	l_combine_type_label(A1,A2,Rhs),
	pp_mnf(store(InURules,Constructor,Rhs,InURules1)),
	generate_unified_rules(T,InURules1,OutURules).

unify_rule(T1,G1,T2,G2,UR) :-
	pp_mnf(lookup(T1,G1,T1Rules)),
	pp_mnf(lookup(T2,G2,T2Rules)),
	lookup(Constructor,T1Rules,Args1),
	lookup(Constructor,T2Rules,Args2),
	UR = unify(Constructor,Args1,Args2).

combine_type_label(T1,T2,T1T2) :- 
	((T1==T2)
	 -> (T1T2 = T1)
	 ;  (string_concatenate(T1,'_',T1u),
	   /* NOT ENTIERELY SAFE WAY TO DO IT: CONFLICTS MIGHT ARISE !! */
	     string_concatenate(T1u,T2,T1T2))
	 ).
l_combine_type_label([],[],[]).
l_combine_type_label([T1|H1],[T2|H2],[T1T2|H1H2]) :-
	combine_type_label(T1,T2,T1T2),
	l_combine_type_label(H1,H2,H1H2).



/* ------------------------------------- */
/* union_type(T1,G1,T2,G2,UT,UG) */
/* ------------------------------------- */

/* union_environments(ProjVars,Env1,Env2,[],NewEnv) */

union_environments([],Env1,Env2,Env,Env).
union_environments([Var|TV],Env1,Env2,InUEnv,OutUEnv) :-
	(lookup(Var,Env1,ecce_type(L1,G1))
	 -> (lookup(Var,Env2,ecce_type(L2,G2))
	     -> (union_type(L1,G1,L2,G2,UL,UGramm),
	         store(InUEnv,Var,ecce_type(UL,UGramm),InUEnv1)
	        )
	     ;  store(InUEnv,Var,ecce_type(L1,G1),InUEnv1)
	    )
	 ;  (lookup(Var,Env2,ecce_type(L2,G2))
	     -> (store(InUEnv,Var,ecce_type(L2,G2),InUEnv1))
	     ;  (InUEnv1 = InUEnv,
	         print('### Warning Var No Type:'), print(Var),nl)
	    )
	),
	union_environments(TV,Env1,Env2,InUEnv1,OutUEnv).


union_type(T1,G1,T2,G2,UType,UGramm) :-
	gennum(GSC),
	standardise_apart(T2,G2,G1,GSC,[],RenG2),
	rename_type(T2,G1,GSC,RenT2),
	union_type2(T1,G1,RenT2,RenG2,[],UGramm),
	combine_type_label(T1,RenT2,UType).

/* supposes labels of G1 and G2 are renamed apart */

union_type2(T1,G1,T2,G2,InUGramm,OutUGramm) :-
	combine_type_label(T1,T2,T1T2),
	(pp_cll(lookup(T1T2,InUGramm,_RulesForT1T2))
	 -> (OutUGramm = InUGramm)
	 ;  (findall(UR,union_rule(T1,G1,T2,G2,UR),Rules),
	     generate_union_rules(Rules,[],T1T2Rules),
	     store(InUGramm,T1T2,T1T2Rules,InUGramm1),
	     union_arguments(Rules,G1,G2,InUGramm1,OutUGramm)
	    )
	).
l_union_type([],[],_G1,_G2,G,G).
l_union_type([T1|H1],[T2|H2],G1,G2,IG,OG) :-
	union_type2(T1,G1,T2,G2,IG,IG1),
	l_union_type(H1,H2,G1,G2,IG1,OG).

union_arguments([],_G1,_G2,UGramm,UGramm).
union_arguments([union(Constructor,A1,A2)|T],G1,G2,InUGramm,OutUGramm) :-
	l_union_type(A1,A2,G1,G2,InUGramm,InUGramm1),
	union_arguments(T,G1,G2,InUGramm1,OutUGramm).
union_arguments([old(Constructor,Nr,A1)|T],G1,G2,InUGramm,OutUGramm) :-
	((Nr=1) -> (OldG = G1) ; (OldG = G2)),
	l_copy_rules(A1,OldG,InUGramm,InUGramm1),
	union_arguments(T,G1,G2,InUGramm1,OutUGramm).

generate_union_rules([],URules,URules).
generate_union_rules([union(Constructor,A1,A2)|T],InURules,OutURules) :-
	l_combine_type_label(A1,A2,Rhs),
	pp_mnf(store(InURules,Constructor,Rhs,InURules1)),
	generate_union_rules(T,InURules1,OutURules).
generate_union_rules([old(Constructor,Nr,A1)|T],InURules,OutURules) :-
	pp_mnf(store(InURules,Constructor,A1,InURules1)),
	generate_union_rules(T,InURules1,OutURules).

union_rule(T1,G1,T2,G2,UR) :-
	pp_mnf(lookup(T1,G1,T1Rules)),
	pp_mnf(lookup(T2,G2,T2Rules)),
	(
	(lookup(Constructor,T1Rules,Args1),
	 (lookup(Constructor,T2Rules,Args2)
	  -> (UR = union(Constructor,Args1,Args2))
	  ;  (UR = old(Constructor,1,Args1))
	 )
	)
	;
	(lookup(Constructor,T2Rules,Args2),
	 not(lookup(Constructor,T1Rules,_Args1)),
	 UR = old(Constructor,2,Args2)
	)
	).

/* MANIPULATING - COPYING TYPE GRAMMARS */

/* ---------- */
/* copy_rules */
/* ---------- */

copy_rules(Type,Gramm,InCGramm,OutCGramm) :-
	(lookup(Type,InCGramm,_R)
	-> (InCGramm = OutCGramm)
	;  (pp_mnf(lookup(Type,Gramm,TRules)),
	    pp_mnf(store(InCGramm,Type,TRules,InCGramm1)),
	    findall(RhsLabel,rhs_label(TRules,_Cons,RhsLabel),Rhs),
	    l_copy_rules(Rhs,Gramm,InCGramm1,OutCGramm)
	    )
	).

l_copy_rules([],Gramm,CG,CG).
l_copy_rules([Type|T],Gramm,InCG,OutCG) :-
	copy_rules(Type,Gramm,InCG,InCG1),
	l_copy_rules(T,Gramm,InCG1,OutCG).


rhs_label(Rules,Constructor,RhsLabel) :-
	lookup(Constructor,Rules,Rhs),
	member(RhsLabel,Rhs).


/* ----------------------------------------------------- */
/* standardise_apart(T1,G1,WrtG2,InNewG1,OutNewG1) */
/* ----------------------------------------------------- */


standardise_apart(Type,Gramm,WrtG2,GSC,InCGramm,OutCGramm) :-
	rename_type(Type,WrtG2,GSC,NewType),
	(lookup(NewType,InCGramm,_R)
	-> (InCGramm = OutCGramm)
	;  (pp_mnf(lookup(Type,Gramm,TRules)),
	    rename_rules(TRules,WrtG2,GSC,NewTRules),
	    pp_mnf(store(InCGramm,NewType,NewTRules,InCGramm1)),
	    findall(RhsLabel,rhs_label(TRules,_Cons,RhsLabel),Rhs),
	    l_standardise_apart(Rhs,Gramm,WrtG2,GSC,InCGramm1,OutCGramm)
	    )
	).
	
l_standardise_apart([],Gramm,WrtG2,GSC,CG,CG).
l_standardise_apart([Type|T],Gramm,WrtG2,GSC,InCG,OutCG) :-
	standardise_apart(Type,Gramm,WrtG2,GSC,InCG,InCG1),
	l_standardise_apart(T,Gramm,WrtG2,GSC,InCG1,OutCG).

rename_type(Type,WrtG2,GSC,NewType) :-
	(lookup(Type,WrtG2,_Rules)
	 -> (string_concatenate(Type,'_',T1),
	     string_concatenate(T1,GSC,NewType))
	 ;  (NewType = Type)
	).

rename_rhs([],WrtG2,GSC,[]).
rename_rhs([H|T],WrtG2,GSC,[RH|RT]) :-
	rename_type(H,WrtG2,GSC,RH),
	rename_rhs(T,WrtG2,GSC,RT).

rename_rules(Rules,WrtG2,GSC,NewRules) :-
	findall(add(C,Rhs),renamed_rule(Rules,WrtG2,GSC,C,Rhs),AddList),
	add_ren_rules(AddList,[],NewRules).
renamed_rule(Rules,WrtG2,GSC,Constructor,NewRhs) :-
	lookup(Constructor,Rules,Rhs),
	rename_rhs(Rhs,WrtG2,GSC,NewRhs).
	
add_ren_rules([],G,G).
add_ren_rules([add(C,Rhs)|T],InG,OutG) :-
	store(InG,C,Rhs,InG1),
	add_ren_rules(T,InG1,OutG).
	


/* canon_type_grammar(Type,Grammar,CanonType,CanonGrammar) */

canon_type_grammar(Type,Grammar,t0,CanonGrammar) :-
	store([],Type,t0,Ren),
	canon_copy_rules(Type,Grammar,[],CanonGrammar,1,_OutNr,Ren,_OutRen).

canon_copy_rules(Type,Gramm,InCGramm,OutCGramm,InNr,OutNr,Renaming,OutRen) :-
	canon_rename(Type,RenType,InNr,InNr1,Renaming,Renaming1),
	(lookup(RenType,InCGramm,_R)
	-> (InCGramm = OutCGramm)
	;  (pp_mnf(lookup(Type,Gramm,TRules)),
	    pp_mnf(store(InCGramm,RenType,TRules,InCGramm1)),
	    /* MISSING: RENAME RHS !! */
	    /* use some ordering on constructors to ensure canonicity */
	    findall(RhsLabel,rhs_label(TRules,_Cons,RhsLabel),Rhs),
	    l_canon_copy_rules(Rhs,Gramm,InCGramm1,OutCGramm,InNr1,OutNr,
	    				 Renaming1,OutRen)
	    )
	).

/* MISSING: MINMIZE STATES (i.e. type labels)
 t0 -> a | f(t1)
 t1 -> a | f(t1)
collapse into:
 t0 -> a | f(t0)
*/

canon_rename(Type,RenType,InNr,OutNr,InRenaming,OutRenaming) :-
	(lookup(Type,InRenaming,RenType)
	 -> (OutNr = InNr, OutRenaming = InRenaming)
	 ;  (OutNr is InNr + 1,
	     string_concatenate(t,InNr,RenType),
	     store(InRenaming,Type,RenType,OutRenaming)
	    )
	).

canon_rename_rhs.

l_canon_copy_rules([],Gramm,CG,CG,Nr,Nr,Ren,Ren).
l_canon_copy_rules([Type|T],Gramm,InCG,OutCG,InNr,OutNr,Renaming,OutRen) :-
	canon_copy_rules(Type,Gramm,InCG,InCG1,InNr,InNr1,Renaming,Ren1),
	l_canon_copy_rules(T,Gramm,InCG1,OutCG,InNr,OutNr,Ren1,OutRen).
