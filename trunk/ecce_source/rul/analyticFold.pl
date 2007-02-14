%##############################################################
%###### FOLDING ANALYSIS SUPPORTING THE GOAL-RULIFICATION #####
%##############################################################
%---(C)-Stefan-Gruner-University-of-Southampton-England-2001---

:- module(analyticFold, [l_goalRULification/4,
	    goalRULification/4, goalFolding/4]).

:- use_module(library(terms)).
:- use_module(library(lists)).

:- use_module(self_check_rul).

:- use_module(gensym2,[seed/1]).

:- use_module(prePostCon,[isRCD/1,isName/1,isPrimitive/1,isNormal/1, rulHead/2,isGoals/1]).

:- use_module(compressor,[compress/2]).

:- use_module(auxil,[errorMsg/2,newName/2,listToGoal/2,fillUpWithANY/3]).

:- use_module(proSiNo,[simplify/2,normalisation/2,normalisation2/2,constraintProjection/3]).

%##############################################################

% (PUBLIC) L_GOALRULIFICATION: ============================
% Iterates on goalFolding to RULify a List of Goals similar
% to goalUnfolding and l_goalUnfolding in the module unfold.
% The list of goals represents a conjunction in the abstract
% partial deduction. =======================================

:- initialization(assert_pre(analyticFold:l_goalRULification(G1, _G2, R1, _R2),
	      (prePostCon:isRCD(R1),
	       prePostCon:isGoals(G1)))).

:- initialization(assert_post(analyticFold:l_goalRULification(_G1, G2, _R1, R2),
	       (prePostCon:isRCD(R2),
		prePostCon:isGoals(G2)))).

%---------------------------------------------------------

l_goalRULification(Goal, NewGoal, RulConstr, NewRulConstr3) :-

        mnf(proSiNo:simplify(RulConstr, RulConstr1)),

	nl,
	print(RulConstr1),
	nl,
	
        mnf(proSiNo:normalisation(RulConstr1, RulConstr2)),

	l_goalRULification2(Goal, NewGoal,
			    RulConstr2,
			    NewRulConstr),
	!,
	term_variables(NewGoal, GoalVars),
	!,
	constraintProjection(GoalVars,
			     NewRulConstr,
			     NewRulConstr2),
	!,
	compress(NewRulConstr2, NewRulConstr3).

%-------------------------------------------------------------------

l_goalRULification2([], [], RCD, FinalRCD) :-

        %user:debug_print(call(simplify(RCD, SRCD))),
	%user:debug_nl,
	/* print(call(simplify(RCD, SRCD))),nl, */
	
	simplify(RCD, SRCD),

        %user:debug_print(call(normalisation(SRCD,FinalRCD))),
	%user:debug_nl,
	
	normalisation(SRCD, FinalRCD),

	%user:debug_print(res_l_goalRULification([],[],RCD,FinalRCD)),
	%user:debug_nl,
	!.

l_goalRULification2([Call|T], [NewCall|RT], RCD, NewRCD) :-

        %user:debug_print(goalFolding(Call,RCD,NewCall,IntRCD)), 
	%user:debug_nl,

	%%%goalFolding(Call, RCD, NewCall, IntRCD),
	mnf(analyticFold:goalFolding(Call, RCD, NewCall, IntRCD)),
	
	/* print(result_of_goalFolding(Call, RCD, NewCall, IntRCD)),nl, */
	
	l_goalRULification2(T, RT, IntRCD, NewRCD),
	!.

l_goalRULification2(X,_,_,_) :-

	errorMsg(l_goalRULification2,X),
	!,
	fail.

%===================================================================

/* MOTIVATION: GOALFOLDING. In our old goalRULification procedure we
   had too many cases in which goal-internal subterm-relationsships
   had been lost in the RULification process. For example, take the
   goal g(f(X),f(X)) with constraint c(X). It has been transformed
   into g(Y,Z) with constraints s(Y),t(Z) which were defined as
   t(f(U)):-c(U) and s(f(V)):-c(V). Thus: we have only preserved
   the relation c(X), but in the RULified (or folded) goal g(Y,Z)
   itself we have lost the information that Y == Z. We now bring
   back some of the lost equivalence information! As g's old subterms
   f(X) and f(X) had been the same, their generated type definitions
   t(f(U)):-c(U) and s(f(V)):-c(V) are equivalent and can therefore
   be compressed to r(f(W)):-c(W). Thus we may replace the generated
   constraint s(Y) by r(Y) and t(Z) by r(Z), and this possibility is
   a safe indicator that Y and Z had been equivalent before the trans-
   formation. We can thus eventually transform g(Y,Z) with r(Y) and
   r(Z) into the final form g(A,A) with the constraint r(A). This is
   achieved by the goalFolding procedure in this module. But there is
   a PRINCIPLE source of un-precision in RULification (goalFolding)
   which we cannot overcome: the occurrance of identical variables
   in different terms. Example: g(X,f(X)). Because variables are
   the FixPoint of the folding procedure we cannot replace them by
   variables as we would do it with the terms. Thus in our example
   we will definitly produce g(X,Y) -- and without any other means
   of relational information than our RUL constraint definition
   (which is the basis of our calculus) we will certainly loose the
   old information that X is somehow within Y. The same is true for
   similar situations, for example g(f(X),h(f(X))) or g(f(X),h(X)).
   Nevertheless, better to have only one precision improvement than
   no one at all! :) From now on it is strongly recommended to use
   goalFolding/4 instead of less sophisticated old goalRULification/4.
   Note that neither Variable-Projection nor Pruning may be applied
   here, because goalFolding is called by l_goalRULification where
   all context information is needed!
*/

:- initialization(assert_pre(analyticFold:goalFolding(In1,In2,_,_),
	      (prePostCon:isGoals([In1]),
		  prePostCon:isRCD(In2)))).

:- initialization(assert_post(analyticFold:goalFolding(_,_, Out1, Out2),
	       (prePostCon:isGoals([Out1]),
		   prePostCon:isRCD(Out2)))).

%---------------------------------------------------------

goalFolding(InputGoal, InputRCD, OutputGoal, OutputRCD) :-

	/* case: precision can be improved */
	
	findMultipleArgs(InputGoal,
			 Multiples),
	
	goalRULification(InputGoal,
			 ResultGoal,
			 InputRCD,
			 ResultRCD),
	
	identifyVariables(Multiples,
			  ResultGoal,
			  OutputGoal),

	cutConstraints(ResultRCD, OutputRCD),
	!.


goalFolding(InputGoal, InputRCD, OutputGoal, OutputRCD) :-

	/* case: precision cannot be improved */

	goalRULification(InputGoal,
			 OutputGoal,
			 InputRCD,
			 OutputRCD),
	!.

goalFolding(X,_,_,_) :-

	errorMsg(goalFolding,X),
	!,
	fail.

%------------------------------------------------------------

cutConstraints(rul__constraint__declaration(InpC, Prog),
	       rul__constraint__declaration(CutC, Prog)) :-

	remove_duplicates(InpC, CutC).

%------------------------------------------------------------

identifyVariables(Multiples, InputGoal, OutputGoal) :-

	InputGoal =.. [Pred|InputVars],

	length(InputVars, Length),

	unification(Length, Multiples,
		    InputVars, OutputVars),

	OutputGoal =.. [Pred|OutputVars].

%------------------------------------------------------------
                                             % roughly tested

unification(InputNumber, [RefList|MultiList],
	    InputVariables, OutputVariables) :-

	unify(InputNumber, RefList,
	      InputVariables, ResultVars),

	unification(InputNumber, MultiList,
		    ResultVars, OutputVariables).

unification(_, [], Vars, Vars).

%------------------------------------------------------------
                                             % roughly tested

unify(InputNumber, [Num1,Num2], InVars, OutVars) :-

	Nth is InputNumber - Num1,
	Mth is InputNumber - Num2,

	nth(Nth, InVars, U),
	nth(Mth, InVars, V),

	U = V,

	OutVars = InVars,
	!.

unify(InputNumber, [Num1,Num2|RefList], InVars, OutVars) :-

	Nth is InputNumber - Num1,
	Mth is InputNumber - Num2,

	nth(Nth, InVars, U),
	nth(Mth, InVars, V),

	U = V,
	!,
	unify(InputNumber, [Num2|RefList], InVars, OutVars).

%------------------------------------------------------------
% findMultipleArgs: Variables will not be taken into account,
% as they will not be replaced by procedure goalRULification.
%------------------------------------------------------------
                                             % roughly tested
findMultipleArgs(InputGoal,_) :-
	
	InputGoal =.. [_|Arguments],
	
	no_doubles(Arguments),
	!,
	fail.

findMultipleArgs(InputGoal, Multiples) :-
	
	InputGoal =.. [_|Arguments],

	attachPositions(Arguments, PosArg),

	discardVariables(PosArg, PosTerms),
	
	searchMultiples(PosTerms, Multiples).

%--------------------------------------------------------------

discardVariables([[V,_]|OldPosArgs], NewPosArgs) :-

	var(V),
	!,
	discardVariables(OldPosArgs, NewPosArgs).

discardVariables([[A,P]|OldPosArgs], [[A,P]|NewPosArgs]) :-

	discardVariables(OldPosArgs, NewPosArgs).

discardVariables([],[]).

%--------------------------------------------------------------

searchMultiples([[E,Pos]|Elements], MultiplePositions) :-

	sameElement(E, Elements, PositionsE),

	PositionsE \== [],
	!,
	removeTreatedPositions(PositionsE,
			       Elements,
			       RestList),

	searchMultiples(RestList, RestPositions),

	append([[Pos|PositionsE]],
	       RestPositions,
	       MultiplePositions).

searchMultiples([_|Elements], MultiPositions) :-

	searchMultiples(Elements, MultiPositions).

searchMultiples([],[]).

%--------------------------------------------------------------

removeTreatedPositions([Pos|Positions],
		       [[_,Pos]|Elements], Output) :-
	!,
	removeTreatedPositions(Positions,
			       Elements, Output).

removeTreatedPositions([], List, List) :- !.

removeTreatedPositions(Positions, [E|Elements], Output) :-

	append(Elements, [E], Permutation),

	removeTreatedPositions(Positions, Permutation, Output).

%--------------------------------------------------------------

sameElement(E, [[ELEM,P]|Elements], [P|Positions]) :-

	ELEM == E,
	!,
	sameElement(E, Elements, Positions).

sameElement(E, [_|Elements], Positions) :-

	sameElement(E, Elements, Positions).

sameElement(_,[],[]).

%--------------------------------------------------------------

attachPositions([],[]).

attachPositions([E|Elements], [[E,Pos]|PosElements]) :-

	length(Elements, Pos),

	attachPositions(Elements, PosElements).


% GOALRULIFICATION: =============================================
% "goalRULification" is an artificial word which shall have the
% following meaning. Let g(term_1, term_2, ..., term_n) be a goal
% in the body of a clause. Then we replace the goal by a new goal
% g(VAR_1, VAR_2, ..., VAR_n) such that VAR_i (i=1,...,n) are new
% variables. Further, we introduce new RUL constraints c_1, c_2,
% ..., c_n such that the logical properties of each term_i (with
% i=1,...,n) are sufficiently described (and preserved) by terms
% c_i(VAR_i). goalRULification(OldGoal, NewGoal, OldRCD, NewRCD),
% thus (+,?,+,?), is the intended I/O-Usage of this predicate.
% To avoid un-intended unifications, we destroy possible variable
% sharings between the goals and the constraint definitions by
% creating a fresh copy of the constraint definitions, just for
% security. Note: The anti-operation of goalRULification is goal-
% Unfolding, but the equation "ID = RULification o Unfolding" is
% true only for special cases. [NEW!17/04/2002]: fillUpWithANY_
% woprojection is a projection that does not delete context info
% any more. ==================================================== 

:- initialization(assert_pre(analyticFold:goalRULification(_G1, _G2, R1, _R2),
              prePostCon:isRCD(R1))).

:- initialization(assert_post(analyticFold:goalRULification(_G1, _G2, _R1, R2),
               prePostCon:isRCD(R2))).

%---------------------------------------------------------------

goalRULification(ThisGoal, ThisGoal, ThisRCD, ThisRCD) :-

	isName(ThisGoal), /* constant goal, e.g. "p" */
	!.

goalRULification(ThisGoal, ThisGoal,
		 rul__constraint__declaration(OldC, OldProg),
		 rul__constraint__declaration(NewC, NewProg)) :-

	isPrimitive(ThisGoal),

	%%%fillUpWithANY([ThisGoal], OldC, NewC),
	mnf(auxil:fillUpWithANY([ThisGoal], OldC, NewC)),

	makeUniqueVariables(OldProg, NextProg), /* New:18/04/2002 */
	
	addAnyType(NextProg, NewProg),
	!.

goalRULification(OldTerm, NewTerm,
		 rul__constraint__declaration(OldC, OldDef),
		 rul__constraint__declaration(OutC, OutDef)) :-

	/* PRECONDITION */
	isNormal(OldC),
	
	copy_term(OldDef, DefCopy),

	addAnyType(DefCopy, WorkDef),

	%%%fillUpWithANY([OldTerm], OldC, BasisConstr),
	mnf(auxil:fillUpWithANY([OldTerm], OldC, BasisConstr)),

	OldTerm =.. [Name|OldArguments],

	/* OldArguments may contain variables! */

	length(OldArguments, TermLength),

	makeFreshVariables(TermLength, FreshVariables),

	crossOver(OldArguments,
		  FreshVariables,
		  NewArguments,
		  NotReplacedVars),

	/* NewArguments: all variables! */

	NewTerm =.. [Name|NewArguments],
	
	seed(SeedNumber),

	makeNewConstraints(SeedNumber,
			   NotReplacedVars,
			   NewArguments,
			   DummyConstraints),

	delete(DummyConstraints, none, Constraints),

	append(BasisConstr, Constraints, OutC),

	makeIntermediateFacts(DummyConstraints,
			      OldArguments,
			      IntermediateFacts,
			      BasisConstr),

	append(WorkDef,
	       IntermediateFacts,
	       IntermediateDef),

	iFactExpansion(IntermediateDef, RegularDef, BasisConstr),

	copy_term(RegularDef, NextDef),

	makeUniqueVariables(NextDef, OutDef),
	!.

%[New:04:04:2002]
goalRULification(OldTerm, NewTerm,
		 rul__constraint__declaration(OldC, OldDef),
		 rul__constraint__declaration(OutC, OutDef)) :-

	/* bad case: constraints not normal! */
	
	simplify(rul__constraint__declaration(OldC, OldDef),
		 rul__constraint__declaration(SimC, SimDef)),
	
	normalisation2(rul__constraint__declaration(SimC, SimDef),
		       rul__constraint__declaration(NewC, NewDef)),
	!,
	goalRULification(OldTerm, NewTerm,
		 rul__constraint__declaration(NewC, NewDef),
		 rul__constraint__declaration(OutC, OutDef)).


goalRULification(X,_,_,_) :-

	errorMsg(goalRULification,X),
	!,
	fail.

%---------------------------------------------------------------

makeFreshVariables(1, [_]) :- !.

makeFreshVariables(Counter, [_|Variables]) :-

	Counter > 1,

	CounterMinusOne is Counter - 1,

	makeFreshVariables(CounterMinusOne,
			   Variables).

%---------------------------------------------------------------

crossOver([], [], [], []).

% if an old Argument is already a variable, we don't need
% to replace it by a fresh variable. By not replacing the
% variable Arguments we are possibly able to preserve some 
% of the original inter-variable dependencies within the
% goals - though not all of them!

crossOver([Var|OldArgs], [_|FreshVars],
	  [Var|NewArgs], [Var|NotReplacedVars]) :-

	var(Var),
	!,
	crossOver(OldArgs, FreshVars,
		  NewArgs, NotReplacedVars).

crossOver([_|OldArgs], [FrVr|FreshVars],
	  [FrVr|NewArgs], NotReplacedVars) :-

	crossOver(OldArgs, FreshVars,
		  NewArgs, NotReplacedVars).

%---------------------------------------------------------------

makeNewConstraints(SeedNumber,
		   OldVariables,
		   [V|Variables],
		   [none|Constraints]) :-

	findVarInList(V, OldVariables),
	!,   /* OldVariables need no new Constraint! */

	makeNewConstraints(SeedNumber,
			   OldVariables,
			   Variables,
			   Constraints).

makeNewConstraints(SeedNumber,
		   OldVariables,
		   [V|Variables],
		   [C|Constraints]) :-

	newName(SeedNumber, NewName),

	name(NewName, NameCode),

	name(ex_, FlagCode), /* Flag indicating goalExpansion */

	append(FlagCode, NameCode, IdentCode),
	
        name(ExpansionName, IdentCode),

	C =.. [ExpansionName, V],

	seed(NewSeed),

	makeNewConstraints(NewSeed,
			   OldVariables,
			   Variables,
			   Constraints).

makeNewConstraints(_, _, [], []).

%---------------------------------------------------------------

findVarInList(V, [X|_]) :-

	V == X,
	!.

findVarInList(V, [_|List]) :-

	findVarInList(V, List).

%---------------------------------------------------------------

makeIntermediateFacts([], [], [], _).

/* ReferenceConstraints necessary because of long-range call-chain.*/

makeIntermediateFacts([C|Constraints], [ArgT|ArgumentTerms],
		      [iFact(TypeName/1, [(Fact:-true)])|InterFacts],
		      ReferenceConstraints) :-

        C    =.. [TypeName, _],
	Fact =.. [TypeName, ArgT],
	
	nonvar(ArgT),
        !,
	makeIntermediateFacts(Constraints,
			      ArgumentTerms,
			      InterFacts,
			      ReferenceConstraints).

/* For Existing Variables we do not need a new Constraint */
/* Definition, because they have their Constraint already */

makeIntermediateFacts([none|Constraints], [Var|ArgumentTerms],
		      InterFacts,
		      ReferenceConstraints) :-
	var(Var),
        !,
	makeIntermediateFacts(Constraints,
			      ArgumentTerms,
			      InterFacts,
			      ReferenceConstraints).

%---------------------------------------------------------------

iFactExpansion(IntermediateDef, RULprogram, ReferenceConstr) :-

	seed(StartIndex),

	goalExpansion(StartIndex, ReferenceConstr,
		      IntermediateDef, IntermediateProgram),

	addAnyType(IntermediateProgram, RULprogram).

%---------------------------------------------------------------

/* base case: expansion done */

goalExpansion(_, _, [], []) :- !.

/* trivial case: no iFact, nothing to do */

goalExpansion(Seed, RefConstr,
	      [proc(N/1,Def)|OldProg],
	      [proc(N/1,Def)|NewProg]) :-
	!,	
	goalExpansion(Seed, RefConstr,
		      OldProg, NewProg).

/* case: RUL head of the form t(f(VAR_1,...,VAR_n)) */

goalExpansion(Seed, RefConstr,
	      [iFact(Name/1, [(RULtype:-true)])|OldProg],
	       [proc(Name/1, [(RULtype:-Goal)])|NewProg]) :-

        rulHead(Name, RULtype), /* RUL Syntax achieved */
	!,
	term_variables(RULtype, TermVariables),
	
        createCall(TermVariables, RefConstr, Goal),

	goalExpansion(Seed, RefConstr, OldProg, NewProg).


/* case: constant pseudo RUL head of the form t(a) */

goalExpansion(Seed, RefConstr,
	      [iFact(Name/1, [(RULconstant:-true)])|OldProg],
	       [proc(Name/1, [(RULconstant:-true)])|NewProg]) :-

        RULconstant =.. [Name, A],

	functor(A, A, 0), /* RUL Syntax achieved */
	!,
	goalExpansion(Seed, RefConstr, OldProg, NewProg).

/* case Non-RUL head without variables */

goalExpansion(Seed, RefConstr,
	      [iFact(Name/1, [(OldPred:-true)])|OldProg],
	       [proc(Name/1, [(NewPred:-Goal)])|NewProg]) :-

        ground(OldPred), /* constant */
	
        OldPred =.. [PredName, OldTerm],

	functor(OldTerm, FuncName, Arity),

	Arity > 0, /* Not yet RUL */
	!,
	makeFreshVariables(Arity, FreshVars),

	OldTerm =.. [FuncName|OldArgs],

	crossOver(OldArgs, FreshVars,
		  NewArgs, []),

	NewTerm =.. [FuncName|NewArgs],
	
	/* NewArgs: all variables. */

	NewPred =.. [PredName,NewTerm],

	makeNewConstraints(Seed, [],
			   NewArgs, DummyPredicates),

	delete(DummyPredicates, none, Predicates),

	listToGoal(Predicates, Goal),

	makeIntermediateFacts(DummyPredicates, OldArgs,
			      InterFacts, RefConstr),

	append(OldProg, InterFacts, IntermediateProg),

	seed(NewSeed), /* for further type names */

	goalExpansion(NewSeed, RefConstr,
		      IntermediateProg, NewProg).

/* case: Non-RUL head with Top-Level Variables */

goalExpansion(Seed, RefConstr,
	      [iFact(Name/1, [(OldPred:-true)])|OldProg],
	       [proc(Name/1, [(NewPred:-Goal)])|NewProg]) :-

        \+(ground(OldPred)), /* variables exist */

        OldPred =.. [PredName, OldTerm],

	functor(OldTerm, FuncName, Arity),

	Arity > 0, /* Not yet RUL */
	
	makeFreshVariables(Arity, FreshVars),

	OldTerm =.. [FuncName|OldArgs],

	crossOver(OldArgs, FreshVars,
		  NewArgs, NotReplacedVars),

	NotReplacedVars \== [], /* Top Level Variables exist! */
	!,
	createCall(NotReplacedVars, RefConstr, SubGoal1),

	NewTerm =.. [FuncName|NewArgs],

	NewPred =.. [PredName,NewTerm],

	makeNewConstraints(Seed, NotReplacedVars,
			   NewArgs, DummyPredicates),

	delete(DummyPredicates, none, Predicates),

	listToGoal(SubGList, SubGoal1),

	append(SubGList, Predicates, SubGoal2),

	listToGoal(SubGoal2, Goal),

	makeIntermediateFacts(DummyPredicates, OldArgs,
			      InterFacts, RefConstr),

	append(OldProg, InterFacts, IntermediateProg),

	seed(NewSeed),

	goalExpansion(NewSeed, RefConstr,
		      IntermediateProg, NewProg).


/* case: Non-RUL head with Non-Top-Level Variables */

goalExpansion(Seed, RefConstr,
	      [iFact(Name/1, [(OldPred:-true)])|OldProg],
	       [proc(Name/1, [(NewPred:-Goal)])|NewProg]) :-

        \+(ground(OldPred)), /* variables exist */
	
        OldPred =.. [PredName, OldTerm],

	functor(OldTerm, FuncName, Arity),

	Arity > 0, /* Not yet RUL */
	
	makeFreshVariables(Arity, FreshVars),

	OldTerm =.. [FuncName|OldArgs],

	crossOver(OldArgs, FreshVars,
		  NewArgs, []), /* Variables in Lower Levels */
	!,
	NewTerm =.. [FuncName|NewArgs],

	NewPred =.. [PredName,NewTerm],

	makeNewConstraints(Seed, [],
			   NewArgs, DummyPredicates),

	delete(DummyPredicates, none, Predicates),

	listToGoal(Predicates, Goal),

	makeIntermediateFacts(DummyPredicates, OldArgs,
			      InterFacts, RefConstr),

	append(OldProg, InterFacts, IntermediateProg),

	seed(NewSeed),

	goalExpansion(NewSeed, RefConstr,
		      IntermediateProg, NewProg).

%----------------------------------------------------------

createCall(Variables, ReferenceConstraints, Goals) :-

	attachTypes(ReferenceConstraints,
		    Variables, Result),

	listToGoal(Result, Goals). 

attachTypes(RefConstr, [V|Vars], [G|Output]) :-

	makeGoal(RefConstr, V, G),
	
	attachTypes(RefConstr, Vars, Output).

attachTypes(_, [], []).

makeGoal([RefC|_], Var, RefC) :-

	RefC =.. [_,V],
	Var == V,
	!.

makeGoal([_|RefConstr], Var, Output) :-

	makeGoal(RefConstr, Var, Output).

makeGoal([], Var, any(Var)).

%-------------------------------------------------------------------

addAnyType([proc(any/1, ANY)|Defs], [proc(any/1, ANY)|Defs]) :- !.

addAnyType(Program, [proc(any/1, [(any(_):-true)])|Program]).

%===================================================================
% On symmetric input goals, goalRULification can produce RCD with
% double variables in clause heads which is not RUL. Such clauses
% are corrected as follows [SG:18/04/2002]:

makeUniqueVariables([],[]) :- !.

makeUniqueVariables([proc(Name/1, InDef)|InProg],
                    [proc(Name/1,OutDef)|OutProg]) :-

	muv1(InDef, OutDef),
	!,
	makeUniqueVariables(InProg, OutProg).

%-------------------------------------------------------------------

muv1([],[]) :- !.

muv1([(Head:-true)|InClauses], [(Head:-true)|OutClauses]) :-
        !,
        muv1(InClauses, OutClauses).

muv1([(Head:-Tail)|InClauses], [(Head:-Tail)|OutClauses]) :-

        Head =.. [Name, _],
        rulHead(Name,Head), /* OK */
        !,
        muv1(InClauses, OutClauses).

muv1([IC|InClauses], [OC|OutClauses]) :-

	muv2(IC,OC),
	!,
	muv1(InClauses, OutClauses).

%-------------------------------------------------------------------

muv2((Head:-Tail), (NewHead:-NewTail)) :-

       findDoubleVariables(Head, Doubles),

       rewriteHead(Head, Doubles,
		   NewHead, Replacements),

       rewriteTail(Replacements, Tail, NewTail).

%-------------------------------------------------------------------

findDoubleVariables(Head, Doubles) :-

	term_variables(Head, TermVars),

	Head =.. [_,Funct],

	Funct =.. [_|Vars],

	makeDiff(TermVars, Vars, Doubles).

%-------------------------------------------------------------------

makeDiff([], List, List) :- !.

makeDiff([TV|TermVars], Vars, Doubles) :-

	removeThis(TV, Vars, Result),

	makeDiff(TermVars, Result, Doubles).

%-------------------------------------------------------------------

removeThis(_, [], []) :- !.

removeThis(TV, [V|Vars], Vars) :-

	V == TV,
	!.

removeThis(TV, [X|InVars], [X|OutVars]) :-

	removeThis(TV, InVars, OutVars).
	
%-------------------------------------------------------------------

rewriteHead(Head, Doubles, NewHead, Replacements) :-

	Head =.. [Pred, Funct],

	Funct =.. [Name|OldVariables],

	muv3(OldVariables,
	     Doubles,
	     NewVariables,
	     Replacements),

	NewFunct =.. [Name|NewVariables],

	NewHead =.. [Pred, NewFunct].

%-------------------------------------------------------------------

muv3([],_,[],[]) :- !.

muv3([O|OldVars], Doubles, [N|NewVars], [(O,N)|Replacements]) :-

	findInVarList(O, Doubles),
	!,
	muv3(OldVars, Doubles, NewVars, Replacements).

muv3([V|OldVars], Doubles, [V|NewVars], Replacements) :-

	muv3(OldVars, Doubles, NewVars, Replacements).

%-------------------------------------------------------------------

findInVarList(Var, [V|_]) :- Var == V, !.

findInVarList(Var, [_|List]) :- findInVarList(Var, List).

%-------------------------------------------------------------------

rewriteTail(Replacements, (OldGoal,OldTail), (NewGoal,NewTail)) :-
	
	OldGoal =.. [Name, OldVar],

	findInPairList((OldVar,NewVar),
		       Replacements,
		       RestList),
	!,
	NewGoal =.. [Name, NewVar],
	
	rewriteTail(RestList, OldTail, NewTail).

rewriteTail(Replacements, (Goal,OldTail), (Goal,NewTail)) :-
	!,
	rewriteTail(Replacements, OldTail, NewTail).

rewriteTail(Replacements, OldGoal, NewGoal) :-

	OldGoal =.. [Name, OldVar],

	findInPairList((OldVar,NewVar), Replacements, _),
	!,
	NewGoal =.. [Name, NewVar].

rewriteTail(_, Goal, Goal).

%-------------------------------------------------------------------

findInPairList((OX,N), [(OV,N)|RestList], RestList) :-

	OX == OV,
	!.

findInPairList(Pair, [X|List], [X|RestList]) :-

	findInPairList(Pair, List, RestList).

%############################### END ###############################
%###################################################################

lgTest([trace(par(par(_829,_829),
		  par(_829,_829)),_835)],
       _864,
       rul__constraint__declaration([new_t636(_829),any(_835)],
	[proc(y_ex_new_t627/1,[(y_ex_new_t627(p):-true)]),
	 proc(y_ex_new_t611/1,
	      [(y_ex_new_t611(agent(_782)):-y_ex_new_t627(_782))]),
	 proc(new_t636/1,
	      [(new_t636(agent(_763)):-y_ex_new_t627(_763)),
	       (new_t636(par(_751,_752)):-y_ex_new_t611(_751),
		   y_ex_new_t611(_752))]),
	 proc(new_t634/1,
	      [(new_t634(par(_726,_727)):-new_t636(_726),
		new_t636(_727))]),
	 proc(ub_new_t633/1,
	      [(ub_new_t633(par(_701,_702)):-new_t634(_701),
		new_t634(_702))]),
	 proc(any/1,[(any(_679):-true)])]),
       _866).

/*
compTest([proc(a/1,[(a(f(X)):-c(X))]),
	  proc(b/1,[(b(f(X)):-d(X))]),
	  proc(c/1,[(c(z):-true)]),
	  proc(d/1,[(d(z):-true)])]).


test(last([_252865|_252866],_252862),
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
     proc(any/1,
	  [(any(_284088):-true)]),
     proc(any/1,
	  [(any(_284078):-true)]),
     proc(any/1,
	  [(any(_284068):-true)]),
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


