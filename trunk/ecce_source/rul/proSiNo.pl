%##############################################################
%### CONSTRAINTS PROJECTION, SIMPLIFICATION, NORMALISATION ####
%##############################################################
%---(C)-Stefan-Gruner-University-of-Southampton-England-2001---

:- module(proSiNo, [simplify/2, constraintProjection/3,
		    normalisation/2, normalisation2/2,
		    satisfiable/3]).

%##############################################################
% Important Notice to the upgrade from SICSTUS 3.8.6 to SICSTUS
% 3.8.7 Prolog! calls of the form "term_variables(<term>,[_|_])"
% or "term_variables(<term>,[])" were allowed in Sicstus 3.8.6,
% but are now forbidden in Sicstus 3.8.7 and must be now written
% as "term_variables(<term>,VAR), VAR=[]" (or VAR=[_|_]). For
% this reason we have replaced those calls by calls to "ground"
% (case VAR=[]) and to not-ground "\+ ground" (case VAR=[_|_])
% below. The predicate "ground" has the same properties in both
% Sicstus 3.8.6 and 3.8.7 (hopefully!). ------- [SG:23.11.2001]
%##############################################################

:- use_module(library(terms)).
:- use_module(library(lists)).

:- use_module(gensym2,[seed/1,reset_seed/1]).

:- use_module(interSection,[interSection/7]).

:- use_module(auxil,[newName/2,onlyVariables/1,pruneRCD/2,wellDefined/3,listToGoal/2,fillUpWithANY/3,errorMsg/2,removeOrphans/3]).

:- use_module(prePostCon,[isRCD/1,isRULgoal/1,isName/1,isPrimitive/1,isSimple/1,isNormal/1,isRULgoal/1,rulHead/2,isGoals/1]).

:- use_module(self_check_rul).

%#################################################################


% (PUBLIC) SIMPLIFICATION: =======================================
% During the unfolding of a constraint program it may be the case
% that some variable constraint c(X) evaluates to c(t) where t is
% some term (but not a variable). In this case we have to test if
% c(t) is satisfiable w.r.t. the given RCD definition. If c(t) is
% indeed satisfiable we simplify it by concretisation (inlining)
% and rewriting. For example, if we have some clauses {c(X):-a(t),
% a(X):-b(s)} in our RCD, then c(t) is satisfiable and it will be
% replaced by b(s) and so on. We stop the satisfiability recursion
% as soon we reach a fact or another variable form, eg. d(Y). Note
% that satisfiability is a decidable property in RUL programs, and
% that the length of the constraint list may grow during the trans-
% formation as some conditions may vanish whilst others may appear.
% With the "true" entry in simplification case2 we ensure that the
% simplification result will be a proper list (and never something
% undefined). Afterwards we use "removeTrivials" to get rid of the
% unnecessary "true" terms in the constraint list. To avoid unwan-
% ted dependencies we return a fresh copy of the original program
% (which is done in the removeTrivials part of the whole procedure)
% ================================================================

:- initialization(assert_pre(proSiNo:simplify(S,_),  prePostCon:isRCD(S))).
:- initialization(assert_post(proSiNo:simplify(_,S), prePostCon:isRCD(S))).

simplify(OldRCD, NewRCD) :-

	simplification(OldRCD, IntermediateRCD),
	removeTrivials(IntermediateRCD, NewRCD),
	!.

/* simplify(X,_) :-

	errorMsg(simplify,X),
	!,
	fail. */

%---------------------------------------------------------------

removeTrivials(rul__constraint__declaration(OldConstr, OrigDef),
	       rul__constraint__declaration(NewConstr, CopyDef)) :-

	copy_term(OrigDef, CopyDef),

	delete(OldConstr, true, NewConstr).

%---------------------------------------------------------------

simplification(rul__constraint__declaration([C|OldConstr], D),
	       rul__constraint__declaration([C|NewConstr], D)) :-

	/* no transformation in this case: */
	
	isRULgoal(C),
	!,
	simplification(rul__constraint__declaration(OldConstr, D),
		       rul__constraint__declaration(NewConstr, D)).


simplification(rul__constraint__declaration([OldC|OldConstr], D),
	       rul__constraint__declaration([true|NewConstr], D)) :-

	copy_term(D, WorkDef),

	/* Fact found: condition vanishes */
	
	satisfiable(OldC, WorkDef, fact),
	!,
	simplification(rul__constraint__declaration(OldConstr, D),
	               rul__constraint__declaration(NewConstr, D)).


simplification(rul__constraint__declaration([OldC|OldConstr], D),
	       rul__constraint__declaration(ResultConstraint, D)) :-

	copy_term(D, WorkDef),

	satisfiable(OldC, WorkDef, SatResult),
	!,
	simplification(rul__constraint__declaration(SatResult,D),
	               rul__constraint__declaration(FixPoint, D)),
	
	         /* FixPoint replaces the first constraint OldC */
	
	append(FixPoint, NewConstr, ResultConstraint),

	simplification(rul__constraint__declaration(OldConstr, D),
		       rul__constraint__declaration(NewConstr, D)).


simplification(rul__constraint__declaration([], Definition),
	       rul__constraint__declaration([], Definition)) :- !.
                                             /* base case */


simplification(_,_) :-
	
	%user:debug_print('* Sub-Goal {'),
	%user:debug_print(BadGoal),
	%user:debug_print('} cannot be satisfied.'),
	%user:debug_nl,
	!,
	fail.

%==============================================================
%(PUBLIC)

satisfiable(ANY, _, fact) :- /* NEW:22/04/2002 */ 

       ANY =.. [any,_],
       !.

satisfiable(Goal, [proc(Pred/1,Clauses)|_], SatResult) :-

	Goal =.. [Pred,_],
	!,
	unify(Goal, Clauses, SatResult).

satisfiable(Goal, [_|RULprogram], SatResult) :-

	satisfiable(Goal, RULprogram, SatResult).

%--------------------------------------------------------------

unify(Goal, [(Fact:-true)|_], fact) :-

        Fact = Goal,
	!.

unify(Goal, [(Head:-SubGoals)|_], SatResult) :-

        Head = Goal, /* desired side-effect on SubGoals! */
        !,
	listToGoal(SatResult, SubGoals).

unify(Goal, [_|Clauses], SatResult) :-

	unify(Goal, Clauses, SatResult).


% (PUBLIC) CONSTRAINTPROJECTION: ================================
% Given a list of variables and an RCD the constraint part of the
% RCD is projected/filtered according to the input variables. The
% definition part of the RCD is not changed. For efficiency we do
% not check the syntax of the input constraint declaration here:
% instead we assume that such a check is done elsewhere and that
% the input RCD is syntactically correct at this point. ATTENTION
% the constraint must be in simplified form, (see simplification
% procedure above), before the projection is applied! (otherwise
% stupid results!)  ============================================

constraintProjection([],
		     rul__constraint__declaration(_,_),
		     rul__constraint__declaration([],[])) :- !.


constraintProjection(InputVarNames,
		     rul__constraint__declaration(InConstr,  InDef),
		     rul__constraint__declaration(OutConstr,OutDef)) :-

	/* PRECONDITION */
	isSimple(InConstr),

	/* PRECONDITION */
	isRCD(rul__constraint__declaration(InConstr, InDef)),

	/* PRECONDITION */
	onlyVariables(InputVarNames),
	
	removeOrphans(InputVarNames, InConstr, OutConstr),

	pruneRCD(rul__constraint__declaration(OutConstr, InDef),
		 rul__constraint__declaration(OutConstr,OutDef)),
	!.


constraintProjection(X,_,_) :-
	
	errorMsg(constraintProjection,X),
	!,
	fail.


% (PUBLIC) NORMALISATION: ======================================
% When a constraint contains more than one type condition on the
% same variable X, for example t1(X) AND t2(X), then we compute
% the intersection (t1 > t < t2) and replace the doubleconstraint
% t1(X) AND t2(X) by the more special constraint t(X). We repeat
% this procedure until no more situation t1(X) AND t2(X) can be
% found such that the number of different constraint names (e.g.
% t1, t2, t3) is not bigger than the number of different variable
% names (e.g. X1, X2, X3) in the end. The definition of the inter-
% section type t is added to the definition part of RCD. Note that
% normal constraints are also simple (see simplification), but not
% vice versa. To avoid unintended variable dependencies between
% the goals and the constraint definitions we create fresh copy.
% ==============================================================

normalisation(R1,R2) :-

	pp_cll(proSiNo:normalisation2(R1,R2)).

:- initialization(assert_pre(proSiNo:normalisation2(R1,_R2),
	      (prePostCon:isRCD(R1),
	       R1 = rul__constraint__declaration(C,_),
	       prePostCon:isSimple(C)))).

:- initialization(assert_post(proSiNo:normalisation2(_R1,R2),
	       prePostCon:isRCD(R2))).

%----------------------------------------------------------------

normalisation2(rul__constraint__declaration([],[]),C) :-
      /* MICHAEL added another base case (redundant) */
      !,
      C = rul__constraint__declaration([],[]).


normalisation2(rul__constraint__declaration(ThisConstr, OrigDef),
	       rul__constraint__declaration(ThisConstr, CopyDef)) :-
	
	isNormal(ThisConstr),
	!,
	copy_term(OrigDef, CopyDef).

normalisation2(rul__constraint__declaration(OldConstr,OldDef),
	       rul__constraint__declaration(NewConstr,NewDef)) :-

	copy_term(OldDef, WorkDef),
	
	getDoubleCondition(OldConstr, Cond1, Cond2),

	Cond1 =.. [TypeName1, Variable], 
	Cond2 =.. [TypeName2, _],

	seed(SeedIndex),

	interSection(TypeName1, TypeName2, NameUB,
		     DefUB, WorkDef, SeedIndex, OutNum),

	NewSeed is OutNum + 2,
	reset_seed(NewSeed),

	CondUB =.. [NameUB, Variable],

	normaliseConstraint(CondUB, Cond1, Cond2,
			    OldConstr, ResultConstr),

	append(WorkDef, DefUB, ResultDef),
	!,
	normalisation2(rul__constraint__declaration(ResultConstr,
						    ResultDef),
		       rul__constraint__declaration(NewConstr,
						    NewDef)).

%-----------------------------------------------------------------------

varTest(X) :- var(X), !.

varTest(A) :-
	print('* NORMALISATION ERROR: NON-VARIABLE CONSTRAINT ARGUMENT! *'),
	nl,
	print('* '),
	print(A),
	nl,
	print('* Solution: apply procedure proSiNo:simplification. *'),
	nl,
	fail.

%-----------------------------------------------------------------------

getDoubleCondition([Cond1,Cond2|_], Cond1, Cond2) :-
	
/* must succeed iff "normal" (see above) fails. */
	
	Cond1 =.. [_, Var1],
	Cond2 =.. [_, Var2],

	varTest(Var1),
	varTest(Var2),

	Var1 == Var2,
	!.

getDoubleCondition([Cond1, _|Constr], C1, C2) :-
	
/* fetch the first found double constraint */
	
	getDoubleCondition([Cond1|Constr], C1, C2),
	!.

getDoubleCondition([_, Cond2|Constr], C1, C2) :-

	getDoubleCondition([Cond2|Constr], C1, C2),
	!.

%------------------------------------------------------

normaliseConstraint(CondUB, Cond1, Cond2,
		    OldConstr, [CondUB|WithoutC2]) :-

	deleteConstr(OldConstr, Cond1, WithoutC1),
	
	deleteConstr(WithoutC1, Cond2, WithoutC2).

%------------------------------------------------------

deleteConstr([Constr|CList], C, Output) :-

	isRULgoal(C),

	C == Constr,
	!,
	deleteConstr(CList, C, Output).

deleteConstr([D|CList], C, [D|Output]) :-

	C \== D,
	!,
	deleteConstr(CList, C, Output).

deleteConstr([],_,[]).

%#################################################################
%############################## END ##############################
%#################################################################

/* test2(last([_252865|_252866],_252862),
     rul__constraint__declaration([any(_252872),
				   any(_252868),
				   any(_252866),
				   any(_252865),
				   any(_252865),
				   any(_252866),
				   any(_252862),
				   new_t46(_252868),
				   ex_new_t61(_278872),
				   ex_new_t62(_278890)],
    [proc(fact_70/1,
	  [(fact_70([]):-true)]),
     proc(fact_71/1,
	  [(fact_71(a):-true)]),
     proc(any/1,
	  [(any(_284119):-true)]),
     proc(new_t46/1,
	  [(new_t46([]):-true),
	   (new_t46([_284105|_284103]):-fact_71(_284105),
			                fact_70(_284103))]),
     proc(ex_new_t61/1,
	  [(ex_new_t61([_284054|_284052]):-fact_71(_284054),
                                           ex_new_t65(_284052))]),
     proc(ex_new_t62/1,
	  [(ex_new_t62([_284035|_284033]):-any(_284035),
	                                   any(_284033))]),
     proc(ex_new_t65/1,
	  [(ex_new_t65([_284014|_284016]):-any(_284016),
	                                   fact_71(_284014))])]),
     _293303,
     _293294).
*/



