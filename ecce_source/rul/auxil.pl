%###################################################################
%################### MODULE: AUXILIARY PROCEDURES ##################
%###################################################################

:- module(auxil, [wellDefined/3, removeClause/4, newName/2,
		  onlyVariables/1, errorMsg/2, pruneRCD/2,
		  replaceAllOccurrences/5, prepareTriple/10,
		  fillUpWithANY/3,listToGoal/2,orderSubGoals/2,
		  removeOrphans/3]).

% WARNING !!! The procedures provided by this module are used
% by many other modules! So please don't do any changes unless
% you're sure that your changes don't have unwanted effects in
% other modules (of which you might be not aware) !! Thank you.

:- use_module(library(lists)).
:- use_module(library(terms)).

:- use_module(prePostCon,[isRULhead/1, isRULclause/1]).

%###################################################################

errorMsg(Procedure, Input) :-
	!,
	print('~~~~~~~~~~~~~~~~~~~~~~~~~'),
	nl,
	print(Procedure),
	print(' failed with input'),
	nl,
	print(Input),
	nl,
	print('~~~~~~~~~~~~~~~~~~~~~~~~~'),
	nl.

% (PUBLIC) ORDERSUBGOALS: =================================
% Brings sub goals of a clause in proper order with respect
% to the order of variables in the clause head. The input
% program is required to be RUL! For example the old clause
% {p(f(X,Y)):-q(Y),r(X)} will be replaced by the new clause
% {p(f(X,Y)):-r(X),q(Y)}. =================================

orderSubGoals([proc(N/1, OldDefN)|OldRULprog],
	      [proc(N/1, NewDefN)|NewRULprog]) :-
	
	orderDefinition(OldDefN, NewDefN),
	!,
	orderSubGoals(OldRULprog, NewRULprog).

orderSubGoals([],[]).

%----------------------------------------------------------

orderDefinition([OldClause|OldDef],
		[NewClause|NewDef]) :-

	orderClause(OldClause, NewClause),
	!,
	orderDefinition(OldDef, NewDef).

orderDefinition([],[]).

%----------------------------------------------------------

orderClause((Fact:-true), (Fact:-true)) :- !.

orderClause((Head:-OldSubGoals),
	    (Head:-NewSubGoals)) :-

        isRULclause((Head:-OldSubGoals)),
	
        Head =.. [_,Argument],
	
	Argument =.. [_|Vars],
	!,
	listToGoal(OldList, OldSubGoals),
	
	arrange(Vars, OldList, NewList),
	
	listToGoal(NewList, NewSubGoals).

%----------------------------------------------------------

arrange([V|Vars], [G|OldList], [G|NewList]) :-

	G =.. [_,Var],
	
	V == Var,
	!,
	arrange(Vars, OldList, NewList).

arrange(Vars, [G|OldList], NewList) :-

	append(OldList, [G], InterList),
	!,
	/* terminates iff RUL syntax! */
	
	arrange(Vars, InterList, NewList).

arrange([],[],[]).


% (PUBLIC) LISTTOGOAL: ================================
% a simple structural transformation in both directions.
% =====================================================

listToGoal([Predicate], Predicate) :-
	
	Predicate \= (_,_),
	!.

listToGoal([P|Preds], (P,Goals)) :-
	
	listToGoal(Preds, Goals),
	!.

listToGoal([],_) :-
	
	errorMsg(listToGoal,[]),
	!,
	fail.

% (PUBLIC) FILLUPWITHANY: ===============================
% Unconstrained goal variables get the any-constraint per
% default. The input goals must have the goal normalform.
% [NEW:08/04/2002]: Constraints that do not refer to any
% goal variable are tacitly removed (error-correcing side
% effect for the sake of robustness). ===================
/*
fillUpWithANY([],_,[]).
fillUpWithANY(InGoals, InConstraints, OutConstraints) :-
	term_variables(InGoals, GoalVars),
	removeOrphans(GoalVars, InConstraints, ResultC),
	fillUpWithANY_2(InGoals, ResultC, OutConstraints).*/

%------------------------------------------------------
% Version without implicit projection (i.e. deletion of
% orphans, i.e. those constraints which do not refer to
% any goal; they must be preserved for larger contexts
% like l_goalFolding). Otherwise loss of precision !!! 
 
fillUpWithANY([],_,[]).

fillUpWithANY(InGoals, InConstraints, OutConstraints) :-

	term_variables(InGoals, GoalVars),

	fillUpWithANY_2(InGoals, InConstraints, OutConstraints).

%---------------------------------------------------------------
%(PUBLIC)

removeOrphans([],_,[]) :- !.

removeOrphans(_,[],[]) :- !.

removeOrphans(VarList, [C|ConstrIn], [C|ConstrOut]) :-

	C =.. [_,V],
	
	hasReference(VarList, V),
	!,
	removeOrphans(VarList, ConstrIn, ConstrOut).

removeOrphans(VarList, [_|ConstrIn], ConstrOut) :-

	removeOrphans(VarList, ConstrIn, ConstrOut).

%---------------------------------------------------------

hasReference([X|_],V) :- X == V, !.

hasReference([_|Vars],V) :- hasReference(Vars,V), !.

hasReference([],_) :- !, fail.

%---------------------------------------------------------

fillUpWithANY_2([], Constr, Constr).

fillUpWithANY_2([G|InputGoals], InputConstr, OutputConstr) :-

	term_variables(G, Variables),
	!,
	fillUp(Variables, InputConstr, InterResult),
	!,
	fillUpWithANY_2(InputGoals, InterResult, OutputConstr).

%-------------------------------------------------------------

fillUp([], Constr, Constr).

fillUp([V|Variables], OldConstr, NewConstr) :-

	findVarInConstr(V, OldConstr),
	!,
	fillUp(Variables, OldConstr, NewConstr).

fillUp([V|Variables], OldConstr, NewConstr) :-

	fillUp(Variables, [any(V)|OldConstr], NewConstr).

%-------------------------------------------------------------

findVarInConstr(V, [C|_]) :-

	C =.. [_,Var],
	V == Var,
	!.

findVarInConstr(V, [_|Constraints]) :-

	findVarInConstr(V, Constraints).


% (PUBLIC) PRUNERCD =======================================
% Takes a RULconstraint declaration and removes all clauses
% from the corresponding program which do not contribute to
% the definition of the given constraints. Thus, the output
% program does not contain any dead code any more w.r.t.the
% given constraints. --------------------------------------
% NEW [SG:20.02.2002] More efficient Re-Implementation. The
% old implementation was bullshit, much to slow and clumsy!
% =========================================================

pruneRCD(rul__constraint__declaration([], _),
	 rul__constraint__declaration([],[])) :- !.

pruneRCD(rul__constraint__declaration(Constraints,
				      InputProgram),
	 rul__constraint__declaration(Constraints,
				      OutputProgram)) :-

	standardANY(InputProgram,
		    WorkProgram),
	!,
	cutAndPasteLoop(Constraints,
			WorkProgram,
			[],/* Akku */
			OutputProgram).

%----------------------------------------------------------

standardANY([],[proc(any/1,[(any(_):-true)])]) :- !.

standardANY([proc(any/1,_)|Program], Output) :-
	!,
	standardANY(Program, Output).

standardANY([Proc|Input], [Proc|Output]) :-

	standardANY(Input, Output).

%----------------------------------------------------------

cutAndPasteLoop([],_, OutputAkku, OutputAkku) :- !.

cutAndPasteLoop(_,[], OutputAkku, OutputAkku) :- !.

cutAndPasteLoop([Pred|RelevantTypes],
	        ProgIn, Akku, ProgOut) :-
	
	Pred =.. [Name,_],
	!,
	cutProc(Name, ProgIn,
		Proc, ResProg),
	!,
	append(Proc, Akku,
	       ChargedAkku), /* paste */
	!,
	newReference(RelevantTypes,
		     Proc, NewTypes),
	!,
	cutAndPasteLoop(NewTypes, ResProg,
			ChargedAkku, ProgOut).

%----------------------------------------------------------

cutProc(_,[],[],[]) :- !.

cutProc(Name, [proc(Name/1,Def)|Program],
	      [proc(Name/1,Def)],Program) :- !.

cutProc(Name, [Def|ProgIn], Proc, [Def|ProgOut]) :-

	cutProc(Name, ProgIn, Proc, ProgOut).

%----------------------------------------------------------

newReference(Goals, [], Goals) :- !.

newReference(GoalsIn, [proc(_/1,Def)], GoalsOut) :-

	extractGoals(Def, NewGoals),
	!,
	append(GoalsIn, NewGoals, GoalsOut).

%----------------------------------------------------------

extractGoals([],[]) :- !.

extractGoals([(_:-true)|Clauses], Output) :-
        !,
	extractGoals(Clauses, Output).

extractGoals([(_:-SubGoals)|Clauses], Output) :-

        listToGoal(Out, SubGoals),
	!,
	extractGoals(Clauses, Result),
	!,
	append(Out, Result, Output).


% (PUBLIC) REPLACEALLOCCURRENCES ==========================
% This procedure supports shortening. Given new name P, old
% Name Q and a RUL program, all clause goals Q(X) are repla-
% ced by P(X). Clause heads stay as they are. =============

replaceAllOccurrences(P, Q, OutFlag,
		      [(Head:-InputGoals)|InputDefs],
		      [(Head:-ResultGoals)|ResultDefs]) :-
                                                      
	replaceInClauses(P, Q, InputGoals,
			 ResultGoals, History),
        !,                                         
	replaceAllOccurrences(P, Q, ResultFlag,
			      InputDefs,
			      ResultDefs),
	!,
	computeFlag([ResultFlag|History], OutFlag).
                                          
replaceAllOccurrences(_,_,[],[],[]).

%----------------------------------------------------------

replaceInClauses(_,_, true, true, [noReplacement]) :- !.

replaceInClauses(P, Q,                          
		 (FirstInputGoal, MoreInputGoals),
		 (FirstResultGoal, MoreResultGoals),
		 [Flag|History]) :-
	!,                                            
	replaceGoal(P, Q, FirstInputGoal,
		    FirstResultGoal, Flag),
        !,                       
	replaceInClauses(P, Q,              
			 MoreInputGoals, 
			 MoreResultGoals,
			 History).
                                        
replaceInClauses(P, Q, OneInputGoal, OneResultGoal, [Flag]) :-
                                          
	replaceGoal(P, Q, OneInputGoal, OneResultGoal, Flag).

%----------------------------------------------------------

/* P(X) replaces Q(X). Q is overwritten by P. */

replaceGoal(P, Q, PredQ, PredP, replacement) :-
	
	PredQ =.. [Q, SameArgument],
	PredP =.. [P, SameArgument],                   
	!.                                             

/* goals with other names remain unchanged. */

replaceGoal(_, _, SameGoal, SameGoal, noReplacement).

computeFlag(FlagList, replacement) :-

	memberchk(replacement, FlagList),
	!.

computeFlag(_,noReplacement).


% (PUBLIC) ONLYVARIABLES: ==========================
% Checks if a non-empty List contains only Variables.
% ==================================================

onlyVariables([X]) :- var(X), !.                       
                                                       
onlyVariables([X|Xn]) :-                               
                                                       
	var(X),
	!,
	onlyVariables(Xn).                             


% (PUBLIC) WELLDEFINED: ===========================
% Checks if a type name corresponds to a definition
% with respect to the given program representation.
% =================================================

wellDefined(PredName, PredDef, RULprogram) :-
	
      inProgram(PredName, PredDef, RULprogram),
      !.

wellDefined(PredName,_,_) :-

	errorMsg(wellDefined, PredName),
	!,
	fail.

inProgram(PredName, PredDef,
	  [proc(PredName/1, PredDef)|_]) :- !.

inProgram(PredName, PredDef,
	  [proc(_,_)|RULprogram]) :-
      !,
      inProgram(PredName, PredDef, RULprogram).


% (PUBLIC) REMOVECLAUSE: ===================================
% Takes a predicate and its goals and a definition list, and
% REMOVES from the list THE FIRST clause whose head argument
% is the same as the argument found in the input predicate,
% thus the removed clause q(D):-(...) has the same domain D
% as the input clause p(D):-(...). Note that "Predicate" and
% "RemovedHead" may be identical, thus "removeClause" should
% never fail (supposed that the arguments are properly used).
% I/O-Usage: (+,-,+,-). =====================================

removeClause(AnchorPredicate, RemovedGoals,
	     [(RemovedHead:-RemovedGoals)|RemainingClauses],
	     RemainingClauses) :-
	
      AnchorPredicate =.. [_,AnchorFunctor],
      
      RemovedHead =.. [_,HeadFunctor],

      functor(AnchorFunctor, SameName, SameArity),
      
      functor(HeadFunctor, SameName, SameArity),
      !.


removeClause(AnchorPredicate, RemovedGoals,
	     [SameClause|ScannedClauses],
	     [SameClause|RemainingClauses]) :-
	
      removeClause(AnchorPredicate, RemovedGoals,
		   ScannedClauses, RemainingClauses).


% (PUBLIC) NEWNAME: ============================================
% Produces a new name for a new type to be generated. "name" and
% "append" are SICSTUS built-in predicates. The new name has the
% form 't<N>' where <N>	should be natural number. [116] is ASCII
% for 't'. <N> is intended to be the IndexNumber input such that
% newName(IndexNumber, t<IndexNumber>) is true. I/O-Usage (+,-).
% ==============================================================

newName(Index, NewName) :-
	
      name(Index, ASCII_Index),
      
      name(new_t, ASCII_t),
      
      append(ASCII_t, ASCII_Index, ASCII_NewName),
      
      name(NewName, ASCII_NewName).


% (PUBLIC) PREPARETRIPLE: =====================================
% prepareTriple supports nextUpperBound (see module upperBound)
% in the case that neither t1 nor t2 is syntactically the same
% as t in (t1,t2,t). In this case we must insert these names in
% a list of type name triples. To enforce certain lexicographic
% order we use SICSTUS built-in in "@=<" predicate (see manual).
% When this is done, we can insert the type name triple into the
% triple list. Moreover, prepareTriple supports also the inter-
% section procedure (see module interSection), thus "upperName"
% "UType" (etc.) can also be read as "intersectionName", "IType"
% (etc.). In case that prepareTriple is called with InputTriples
% un-instantiated, a new triple list is generated. =============

prepareTriple(TypeName1, TypeName2, UpperName,
	      Type1, Type2, UType, InputTriples,
	      ReferenceTriples, IndexInput, IndexOutput) :-
	
      TypeName1 @=< TypeName2,
      !,
      insertTriple(TypeName1, TypeName2, UpperName,
		   Type1, Type2, UType, InputTriples,
		   ReferenceTriples, IndexInput, IndexOutput).

prepareTriple(TypeName1, TypeName2, UpperName, Type1, Type2,
	      UType, InputTriples, ReferenceTriples, IndexInput,
	      IndexOutput) :-
	
      insertTriple(TypeName2, TypeName1, UpperName,
		   Type1, Type2, UType, InputTriples,
		   ReferenceTriples, IndexInput, IndexOutput).

% INSERTTRIPLE: ===============================================
% insertTriple supports the prepareTriple procedure. Takes the
% input type names and puts them into a triples. New type names
% for the created new type (upper type or intersection type) are
% provided by index incrementation. "var", "memberchk" are SICS-
% TUS built-in predicates. var(InputTriples) checks if the input
% triple list is still an uninstantiated variable (i.e. the list
% does not yet exist) in which case it has to be created from
% scratch (by unification, see "="). ==========================

insertTriple(TypeName1, TypeName2, UpperName, Type1, Type2,
	     UType, InputTriples, ReferenceTriples, IndexInput,
	     IndexOutput) :-
	
      inTripleList(TypeName1, TypeName2, UpperName,
		   [(Type1,Type2,UType)|InputTriples],
		   ReferenceTriples, IndexInput, IndexOutput).

inTripleList(TypeName1, TypeName2, UpperName, InputTripleList,
	     ReferenceTriples, IndexInput, IndexOutput) :-
	
      var(InputTripleList), /* List does not yet exist */
      !,
      newName(IndexInput, UpperName),
      
      IndexOutput is IndexInput + 1,
      
      InputTripleList = [(TypeName1,TypeName2,UpperName)|_],
      
      memberchk((TypeName1,TypeName2,UpperName),
		ReferenceTriples).

inTripleList(TypeName1, TypeName2, UpperName,
	     [(TypeName1,TypeName2,UpperName)|_],
	     _, SameIndex, SameIndex) :- !.

inTripleList(TypeName1, TypeName2, UpperName,
	     [_|InputTriples], ReferenceTriples,
	     IndexInput, IndexOutput) :-
	
      inTripleList(TypeName1, TypeName2, UpperName,
		   InputTriples, ReferenceTriples,
		   IndexInput, IndexOutput).

%############################################################
% PROGSIZE: =================================================
% counts all head and tail predicates occuring in the clauses
% of the input program. (Not needed at the moment, purpose is
% statistics.)
%
%progSize(Program, Size) :- (0, Program, Size).
%
%progSize(Num, [], Num).
%
%progSize(InitNum, [proc(_/1, Clauses)|ProcDefs], TopNum) :-  
%                                                             
%	countClauses(InitNum, Clauses, OutNum),     
%                                                             
%	NewNum is OutNum + 1,                                  
%                                                             
%	progSize(NewNum, ProcDefs, TopNum).
%
%/* weird case: undefined */
%countClauses(Num, [], Num) :-
%	nl,
%	print('* Warning: Empty Proc-Definition Detected! *'),
%	nl,
%	!.
%
%/* case: only one clause */
%
%countClauses(Num, [(_:-Goals)], OutNum) :-          
%        !,                                                   
%	NextNum is Num + 1,                                    
%				                             
%        countGoals(NextNum, Goals, OutNum).
%
%/* case: more than one clause */
%
%countClauses(Num, [(_:-Goals)|Clauses], OutNum) :-   
%                                                             
%        SuccNum is Num + 1,                                    
%                                                             
%        countGoals(SuccNum, Goals, NextNum),      
%                                                             
%	NewNum is NextNum + 1,                                 
%                                                             
%	countClauses(NewNum, Clauses, OutNum).  
%
%
%/* case: multiple Goal */                                    
%
%countGoals(Num, (_, MoreGoals), OutNum) :-       
%	!,                                                   
%	NextNum is Num + 1,                                    
%                                                             
%	countGoals(NextNum, MoreGoals, OutNum).                         
%/* case: single Goal */                                      
%countGoals(Num, _, Num).                    

%##########################################################
%########################## END ###########################
%##########################################################


orTest([proc(any/1,[(any(_1282):-true)]),
	proc(x_y_ex_new_t229/1,[(x_y_ex_new_t229([]):-true)]),
	proc(x_y_ex_new_t228/1,[(x_y_ex_new_t228(b):-true)]),
	proc(x_ub_new_t232/1,
	     [(x_ub_new_t232([]):-true),
	      (x_ub_new_t232([_2244|_2242]):-x_y_ex_new_t228(_2244),
		  x_y_ex_new_t229(_2242))]),
	proc(y_y_ex_new_t229/1,[(y_y_ex_new_t229([]):-true)]),
	proc(y_y_ex_new_t228/1,[(y_y_ex_new_t228(b):-true)]),
	proc(y_ex_new_t248/1,[(y_ex_new_t248(b):-true)]),
	proc(y_ub_new_t232/1,
	     [(y_ub_new_t232([]):-true),
	      (y_ub_new_t232([_2298|_2296]):-y_y_ex_new_t228(_2298),
		  y_y_ex_new_t229(_2296))]),
	proc(y_ex_new_t246/1,
	     [(y_ex_new_t246([_2275|_2277]):-y_ub_new_t232(_2277),
	       y_ex_new_t248(_2275))])],
       PROG).

arTest([_706,_707],[y_ub_new_t232(_707),y_ex_new_t248(_706)],OUT).
