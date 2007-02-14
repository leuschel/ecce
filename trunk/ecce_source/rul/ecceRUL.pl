%##############################################################
%##### INTERFACE BETWEEN ECCE AND THE RUL TYPING MODULES ######
%##############################################################
%---(C)-Stefan-Gruner-University-of-Southampton-England-2001---

:- module(ecceRUL, [entails/4, widen/6, project_and_check_constraint/3,
	            project_constraints/4, divide_constraint_rul_goal/3, 
                    print_rul/1, print_rul/2, structuralEquivalence/2]).

:- use_module(library(terms)).
:- use_module(library(lists)).

:- use_module(self_check_rul).

:- use_module(msg2,[msg2/3]).

:- use_module(subType,[subType/3]).

:- use_module(compressor,[compress/2]).

:- use_module(upperBound,[upperBound/7]).

:- use_module(gensym2,[seed/1,reset_seed/1]).

:- use_module(shorten,[shorten_LOPSTR_1997/4]).

:- use_module(proSiNo,[normal/1,satisfiable/3,simplify/2,normalisation/2]).

:- use_module(analyticFold,[l_goalRULification/4]).

:- use_module(prePostCon,[isNormal/1,isRULified/1,isRCD/1,isNormalRCD/1]).

:- use_module(auxil,[onlyVariables/1,progSize/3,fillUpWithANY/3,pruneRCD/2]).

:- use_module(unfold2,[l_goalUnfolding/2]).
%#############################################################

:- use_package( .('../ecce_no_rt2') ).


%===============================================
%(PUBLIC)
structuralEquivalence(Structure1, Structure2) :-

	copy_term(Structure1, SameStructure),
	copy_term(Structure2, SameStructure).


% (PUBLIC) WIDEN: =============================================
% Widening is done as follows. We take two lists of input goals
% which must be structural equivalent and in goal normal form,
% for example [g(A,B,C), h(D,E)] and [g(X,X,Y), h(Z,Z)]. More-
% over we take their according RCDs as input. For example, let
% the variables A,B,D,D,E,X,Y,Z be type-constrained as follows:
% t0(A), t1(B), t2(C), t3(D), t4(H), s0(X), s1(Y), s2(Z). Then,
% we build upperBounds according to the given structural order,
% in our example: u0=t0+s0, u1=t1+s0, u2=t2+s1, u3=t3+s2, and
% u4=t4+s2. For the output, we merge the input structures such
% that we have [g(V0,V1,V2), h(V3,V4)] under the constraints
% u0(V0), u1(V1), u2(V2), u3(V3), u4(V4). New Variables are
% constructed by MSG. The new constraint definition program,
% (containing also the new upperBound definitions), is then
% shortened. Thus we have "Widening = UpperBound + Shortening".
% [NEW:17.10.2001]: Original Procedure widen/6 is now renamed
% to widen2/6. New Procedure widen/6 written by Michael serves
% as a "smart interface" to widen2/6 which asserts the pre/post
% conditions which must be fulfilled by the widening procedure.
% Moreover, it avoids too-low-precision widening results by un-
% folding the RULified goals again as soon as possible.========

widen(G1, R1, G2, R2, GRes, RRes3) :-
  
  /* print(calling_naive_widen(G1,R1,G2,R2)),nl, */
  
  naive_widen(G1, R1, G2, R2, GRes, RRes1),!,

  /* print(naive_widen(GRes,RRes1)),nl, */
  
  msg2(G1,G2,MSG),

  /*print(msg(G1,G2,MSG)),nl, */
  
  mnf((GRes = MSG)), /* apply result of MSG to our result */
  
  pp_mnf(proSiNo:simplify(RRes1, RRes2)),
  
  mnf(proSiNo:normalisation(RRes2, RRes3)),!.

naive_widen(G1,R1,G2,R2,G,RFinal) :- /* Michael, 17.10.2001 */
	
   pp_mnf(analyticFold:l_goalRULification(G1,RG1,R1,RR1)),
   pp_mnf(analyticFold:l_goalRULification(G2,RG2,R2,RR2)),
   /* debug_print(calling_widen2(RG1, RR1, RG2, RR2, G, R)),debug_nl, */
   pp_mnf(ecceRUL:widen2(RG1, RR1, RG2, RR2, G, R)),!,
   /* debug_print(result_widen2(G,R)),debug_nl,  */
   proSiNo:simplify(R,RFinal), /* NEU:17/04/2002 */ 
   l_goalUnfolding(G,RFinal).

%----------------------------------------------------------------

:- initialization(assert_pre(ecceRUL:widen2(_G1,_R1, _G2,_R2, _G,_R),

	         (prePostCon:isRCD(_R1),
		  prePostCon:isGoals(_G1),
                  prePostCon:isGoals(_G2),
		  prePostCon:isRCD(_R2),
		  prePostCon:isRULified(_G1),
		  prePostCon:isRULified(_G2),
		  ecceRUL:structuralEquivalence(_G1,_G2),
		  _R1 = rul__constraint__declaration(Constr1IN,_),
		  prePostCon:isNormal(Constr1IN),
		  _R2 = rul__constraint__declaration(Constr2IN,_),
		  prePostCon:isNormal(Constr2IN)))).

:- initialization(assert_post(ecceRUL:widen2(_G1,_R1,_G2,_R2,_G,_R),
	       
	          (prePostCon:isRCD(_R),
		   ecceRUL:structuralEquivalence(_G1,_G),
		   prePostCon:isGoals(_G)))).

%----------------------------------------------------------------

widen2(Goals1IN, RCD1IN,
       Goals2IN, RCD2IN,
       GoalsOUT, RCDOUT) :-

	compress(RCD1IN, ComprRCD1),
	!,
	compress(RCD2IN, ComprRCD2),
	!,
	widen3(Goals1IN, ComprRCD1,
	       Goals2IN, ComprRCD2,
	       GoalsOUT, RCDOUT).

%----------------------------------------------------------------

widen3(Goals1IN, rul__constraint__declaration(Constr1IN, Prog1IN),
       Goals2IN, rul__constraint__declaration(Constr2IN, Prog2IN),
       GoalsOUT, rul__constraint__declaration(ConstrOUT, ProgOUT)) :-
	
	fillUpWithANY(Goals1IN, Constr1IN, Constr1),	
	fillUpWithANY(Goals2IN, Constr2IN, Constr2),
	!,
	renameConstraints(x, Constr1, ConstrX),
	renameConstraints(y, Constr2, ConstrY),
	!,
	copy_term(Prog1IN, Prog1),
	copy_term(Prog2IN, Prog2),
	!,
	tagAllPredicates(x, Prog1, ProgX),
	tagAllPredicates(y, Prog2, ProgY),
	!,
	append([proc(any/1,[(any(_):-true)])|ProgX],
	       ProgY, IntermediateProgram),
	!,
	coordinateVariables(Goals1IN, ConstrX, _, CoordConstr1),
	coordinateVariables(Goals2IN, ConstrY, _, CoordConstr2),
	!,
	extractOverlap(CoordConstr1, CoordConstr2, OverlapConstr),

	/* OverlapConstr: candidates for upperBound construction */
	!,
	upperBounding(OverlapConstr,IntermediateProgram,
		      UpperBoundList, UpperBoundProgram),

	msg2(Goals1IN, Goals2IN, MergedGoals),
	
	coordinateVariables(MergedGoals, _, CoGoals, _),
	
	correlate(CoGoals, UpperBoundList, ConstrUB),
	
	deleteGoalCoordinates(CoGoals, GoalsOUT),
	!,
	shorten_LOPSTR_1997(ConstrUB,ConstrOUT,
			    UpperBoundProgram,
			    ProgOUT).

%----------------------------------------------------------------

deleteGoalCoordinates([CG|CoGoals], [G|Goals]) :-

	deleteCoordinates(CG, G),

	deleteGoalCoordinates(CoGoals, Goals).

deleteGoalCoordinates([], []).

deleteCoordinates(CG, G) :-

	CG =.. [ThisName|CoordVariables],

	unWrap(CoordVariables, Variables),
	
	G  =.. [ThisName|Variables].

%---------------------------------------------------------------

unWrap([[_,[_,V]]|CoordVariables], [V|Variables]) :-

	unWrap(CoordVariables, Variables).

unWrap([], []).

%---------------------------------------------------------------

correlate(_, [], []).

correlate([CG|CoGoals], [UB|UpperBounds], [NewC|OutputConstr]) :-

	inGoal(CG, UB, NewC),
	!,
	correlate([CG|CoGoals], UpperBounds, OutputConstr).

correlate([CG|CoGoals], UpperBounds, OutputConstr) :-

	append(CoGoals, [CG], Permutation),
	
	correlate(Permutation, UpperBounds, OutputConstr).

%--------------------------------------------------------------

inGoal(CG, UB, NewC) :-

	CG =.. [_|CoordVarList],
	UB =.. [Name, CoordVar],

	findCoordVar(CoordVarList,
		     CoordVar,
		     VarName),

	NewC =.. [Name, VarName].

%--------------------------------------------------------------

findCoordVar([[Num1,[Num2,Var]]|_], [Num1,[Num2,_]], Var) :- !.

findCoordVar([_|CoordVarList], CoordVar, OutputVar) :-

	findCoordVar(CoordVarList, CoordVar, OutputVar).

%--------------------------------------------------------------

upperBounding([(CX,CY)|ConstrPairs], InputProgram,
	      [UB|UpperBoundList], OutputProgram) :-

	CX =.. [TX,[Num1,[Num2,_]]],
	CY =.. [TY,[Num1,[Num2,_]]],
	
	seed(Seed),
	
	upperBound(TX, TY, TU, DefUB,
		   InputProgram, Seed, OutNum),

	/* NewSeed: avoid Name Clashes! */
	
	NewSeed is OutNum + 3,
	
	reset_seed(NewSeed),

        UB =.. [TU,[Num1,[Num2,_]]],

	append(InputProgram, DefUB, ProgramUB),

	upperBounding(ConstrPairs, ProgramUB,
		      UpperBoundList, OutputProgram).

upperBounding([], ThisProgram, [], ThisProgram).

%---------------------------------------------------------------

renameConstraints(_, [], []).

renameConstraints(Tag, [C|Constr], [TC|TaggedConstr]) :-

	tagAtoms(Tag, C, TC),

	renameConstraints(Tag, Constr, TaggedConstr).

% (PUBLIC) ENTAILS: ==============================================
% Input is structured as follows: GL1, C1, GL2, C2 whereby GL1 and
% GL2 are Lists of Goals while C1 and C2 are data structures which
% contain Lists of Constraints (belonging to those goals). We may
% assume that the both constraint lists are equally structured for
% example [c11, c12, c13] and [c21, c22, c23]. We say that two con-
% straints c1i and c2j are related to each other if their according
% goals g1x and g2y are also related to each other (via same predi-
% cate name and arity), whereby g1x in GL1 and g2y in GL2. Also GL1
% and GL2 can be assumed to be equally structured. Then we check if
% ALL to-each-other-related constraints of C1 and C2 are in subtype
% relation with respect to a reference program constructed from the
% programs contained by C1 and C2. (The subtype order is C1 < C2,
% thus C1 sub and C2 super.) The procedure fails if not all to-each-
% other-related constraints can be found in subtype relation. Note
% that the inputs must be simplified, normalised and RULified before
% the subtype check can be applied. The I/O-Use of entails is check-
% only (+,+,+,+). Example:
% 
% Input1 [p(A),q(B),r(C),s(D)] [c1(A),c2(B),c3(C),any(D)]
% Input2 [p(E),q(F),r(G),s(H)] [c4(E),c5(F),any(G),c6(H)]
%
% Then we check if c1 is subtype of c4, if c2 is subtype of c5,
% if c3 is a subtype of "any", and if "any" is a subtype of c6.
% =============================================================

entails(G1,R1,G2,R2) :-
  pp_cll(ecceRUL:entails2(G1,R1,G2,R2)).

:- initialization(assert_pre(ecceRUL:entails2(G1,R1,G2,R2),

	      (prePostCon:isGoals(G1),
	       prePostCon:isGoals(G2),
	       ecceRUL:structuralEquivalence(G1,G2),
	       prePostCon:isRULified(G1),
	       prePostCon:isRULified(G2),
               prePostCon:isRCD(R1),
	       prePostCon:isRCD(R2),
               prePostCon:isNormalRCD(R1),
	       prePostCon:isNormalRCD(R2)))).

:- initialization(assert_post(ecceRUL:entails2(_,_,_,_), true)). %redundant

%-----------------------------------------------------------------

entails2(Goals1IN, rul__constraint__declaration(Constr1IN, Prog1IN),
         Goals2IN, rul__constraint__declaration(Constr2IN, Prog2IN)) :-

	fillUpWithANY(Goals1IN, Constr1IN, Constr1),	
	fillUpWithANY(Goals2IN, Constr2IN, Constr2),

	coordinateVariables(Goals1IN, Constr1, _, CoordConstr1),
	coordinateVariables(Goals2IN, Constr2, _, CoordConstr2),

	extractOverlap(CoordConstr1, CoordConstr2, OverlapData),
	
	createReferencePairs(x, y, OverlapData, TypePairs),

	remove_duplicates(TypePairs, ReferencePairs),
	
	/* x, y used for renaming to avoid name clashes */
	
	tagAllPredicates(x, Prog1IN, ProgX),
	tagAllPredicates(y, Prog2IN, ProgY),
	
	append(ProgX, ProgY, ReferenceProg),
	
	checkAllSubTypeProperty(ReferencePairs, ReferenceProg).

%---------------------------------------------------------------

checkAllSubTypeProperty([], _).

checkAllSubTypeProperty([(T1,T2)|Pairs], RULprogram) :-

	subType(T1, T2, RULprogram),

	checkAllSubTypeProperty(Pairs, RULprogram).

%===============================================================

tagAllPredicates(_, [], []).

tagAllPredicates(Tag, [proc(OldName/1,OldDefs)|OldProcs],
		      [proc(TagName/1,TagDefs)|TagProcs]) :-

	reName(Tag, OldName, TagName),

	tagDefinitions(Tag, OldDefs, TagDefs),

	tagAllPredicates(Tag, OldProcs, TagProcs).

%---------------------------------------------------------------

tagDefinitions(_, [], []).

tagDefinitions(Tag, [(OldPred:-OldGoals)|OldClauses],
	            [(TagPred:-TagGoals)|TagClauses]) :-

        OldPred =.. [OldName|Arguments],
	reName(Tag,  OldName,  TagName),
	TagPred =.. [TagName|Arguments],
	
	tagAtoms(Tag, OldGoals, TagGoals),

	tagDefinitions(Tag, OldClauses, TagClauses).

%---------------------------------------------------------------

tagAtoms(Tag, (FirstOldGoal,NextOldGoals),
	      (FirstTagGoal,NextTagGoals)) :-
	!,
	FirstOldGoal =.. [OldName|Arguments],
	reName(Tag, OldName, TagName),
	FirstTagGoal =.. [TagName|Arguments],

	tagAtoms(Tag, NextOldGoals, NextTagGoals).

tagAtoms(Tag, OldGoal, TagGoal) :-

	OldGoal =.. [OldName|Arguments],
	reName(Tag, OldName, TagName),
	TagGoal =.. [TagName|Arguments].

%---------------------------------------------------------------

reName(_, any, any) :- !. /* key-word excluded from renaming! */

reName(_,true,true) :- !. /* key-word excluded from renaming! */

reName(Tag, Old, New) :-

	name(Tag, TagCode),
	name(Old, OldCode),

	append(TagCode,
	       [95|OldCode],
	       NewCode),

	name(New, NewCode).

%---------------------------------------------------------------

createReferencePairs(_, _, [], []).

createReferencePairs(Tag1, Tag2,
		     [(CooPred1,CooPred2)|OldPairs],
		     [(NewName1,NewName2)|NewPairs]) :-

	CooPred1 =.. [OldName1|_],
	CooPred2 =.. [OldName2|_],

	reName(Tag1, OldName1, NewName1),
	reName(Tag2, OldName2, NewName2),
	!,
	createReferencePairs(Tag1, Tag2, OldPairs, NewPairs).

%---------------------------------------------------------------

extractOverlap([StructureX|IDconstrX],
	       [StructureY|IDconstrY],
	       [(StructureX,StructureY)|Pairs]) :-

	StructureX =.. [_,[Id1,[Id2,_]]],
	StructureY =.. [_,[Id1,[Id2,_]]],
	!,
	extractOverlap(IDconstrX, IDconstrY, Pairs).

extractOverlap([ConstrX|IDconstrX],
	       [ConstrY|IDconstrY], Result) :-
	!,
	extractOverlap(IDconstrX,
		       [ConstrY], Pair),

	extractOverlap([ConstrX|IDconstrX],
		       IDconstrY, MorePairs),

	append(Pair, MorePairs, Result).

extractOverlap(_, _, []).


% COORDINATE VARIABLES =================================
% Constraints refer to Goals via Variable Names in RCDs.
% In Clauses, the Subgoals refer to the Clause Heads via
% Variable Names as well. With this procedure, those re-
% ferences are made explicit by attaching numerical co-
% ordinates. ===========================================

coordinateVariables(Goals, Constr, GoalsWithID, ConstrWithID) :-

	attachReferenceIDs(0, Goals, GoalsWithID),
	
	transferReferences(GoalsWithID, Constr, ConstrWithID).

%-------------------------------------------------------------

transferReferences(_,[],[]).

transferReferences(GoalsWithID, [C|Constr], ConstrWithID) :-

	searchReferences(GoalsWithID, C, CID_List),

	/* By construction, the identifiers in IDconstr */
	/* are unique iff the original constraints are  */
	/* in the required normal form.                 */

	transferReferences(GoalsWithID, Constr, IDconstr),

	append(CID_List, IDconstr, ConstrWithID).

%-------------------------------------------------------------

searchReferences([],_,[]).

searchReferences([GID|GoalsWithID], C, ConstrWithID) :-

	GID =.. [_|VarsWithID],

	searchInVars(VarsWithID, C, InGoalResult),

	searchReferences(GoalsWithID, C, CID_List),

	append(InGoalResult, CID_List, ConstrWithID).

%-------------------------------------------------------------

searchInVars([],_,[]).

searchInVars([[Num1,[Num2,V]]|MoreVIDs], C, [CID|MoreCIDs]) :-

	C =.. [Name, Variable],

	V == Variable,

	CID =.. [Name,[Num1,[Num2,Variable]]],
	!,
	searchInVars(MoreVIDs, C, MoreCIDs).

searchInVars([_|VIDs], C, CIDs) :-

	searchInVars(VIDs, C, CIDs).

%=============================================================

attachReferenceIDs(Seed, Goals, Output) :-

	dimension1(Seed, Goals, Dim1),
	
	dimension2(Dim1, Output).

%-------------------------------------------------------------

dimension1(_, [], []).

dimension1(Seed, [Atom|List], [[Number,Atom]|ListID]) :-

	Number is Seed + 1,

	dimension1(Number, List, ListID).

%-------------------------------------------------------------

dimension2([], []).

dimension2([[Number,Atom]|List], [AtomID|ListID]) :-

	proceed(Number, Atom, AtomID),

	dimension2(List, ListID).

%-------------------------------------------------------------

proceed(Number, Pred, PredID) :-

	/* Pre-Condition: Pred is RULified! */
	
	Pred =..[Name|VarList],
	onlyVariables(VarList),
	!,
	propagate(Number, VarList, NumVars),

	dimension1(0, NumVars, VarsID),

	PredID =.. [Name|VarsID].

%-------------------------------------------------------------

propagate(_, [], []).

propagate(ID, [Var|VarList], [[ID,Var]|VarListID]) :-

	propagate(ID, VarList, VarListID).

%=================================================================
% (PUBLIC) PROJECT_AND_CHECK_CONSTRAINT: Serves as an Ecce-conform
% interface to the simplification procedure. =====================

project_and_check_constraint(_Goal, [], []) :- !.

	/* [11/04/2002] Cut inserted to avoid a
	   backtracking simplify([],_) failure. */ 

project_and_check_constraint(_Goal, [Constraint], [Constraint]) :- !,
	
    simplify(Constraint, SConstr), 
    
    l_goalUnfolding(_Goal, SConstr).
    
project_and_check_constraint(_Goal, Constraint, Constraint) :-
    simplify(Constraint, SConstr),    
    l_goalUnfolding(_Goal, SConstr).

% =========================================================
% (PUBLIC) DIVIDE_CONSTRAINT_RUL_GOAL (written by Michael)
% Divides a goal into its ordinary and into its constraint
% part then transfroms constraint part into a form suitable
% for Stefan's modules (e.g., not a list) =================

divide_constraint_rul_goal(Goal,OGoal,CGoal) :-
	
	user:divide_constraint_goal(Goal,OGoal,RulC),
	((RulC = [])
	  -> (CGoal = rul__constraint__declaration([], []) )
	  ; ((RulC = [CGoal]) -> true
	     ; (print('### error in divide_constraint_rul_goal:'), nl,
	        print(divide_constraint_rul_goal(Goal,OGoal,CGoal)), nl))).

%--------------------------------------------------------------------------

/* (PUBLIC) */
print_rul(Goal,RUL) :-
/* prints a rul_constraint in user-friendly style [mal] */	
   print('Ordinary Goal: '),
   print(Goal),
   nl,
   print_rul(RUL).
print_rul([X]) :- !, print_rul(X).
print_rul(rul__constraint__declaration(Goal, Prog)) :- !,
   print('RUL Constraints: '),
   print(Goal),
   nl,
   print('RUL Program:'),
   nl,
   print_rul_prog(Prog).
print_rul(R) :- print(illegal_rul_format(R)), nl.

print_rul_prog([]).
print_rul_prog([Proc|T]) :-
	print_rul_proc(Proc),
	print_rul_prog(T).

print_rul_proc(proc(_,Defs)) :- nl,
	print_rul_defs(Defs).
  
print_rul_defs([]).
print_rul_defs([Def|T]) :-
    print('  '),
	print(Def),
	print('.'),
	nl,
	print_rul_defs(T).

%#################################################################
%############################## END ##############################
%#################################################################
