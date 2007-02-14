%###############################################################
%############# MODULE: Pre- and Post Conditions ################
%###############################################################

:- module(prePostCon, [isRULprog/1, isName/1, isType/1,
	  isValue/1, isRCD/1, isRULhead/1, isRULtail/1,
          isRULclause/1, isConstraint/1, rulHead/2, isSimple/1,
	  isNormal/1, isNormalRCD/1, isOrdered/1, isRULgoal/1,
          isConstrDecl/1, isDefinition/1, isRULified/1,
	  isPrimitive/1, isConsistent/1, isGoals/1]).

% ===========================================================
% PURPOSE: We specify some pre-and post conditions which must
% be true at certain points of an ECCE run. The points where
% these conditions shall hold true, must be declared in the
% source code of those modules which are supported by this
% module. The ECCE system allows for switching the condition
% checks on and off, such that the user can decide on the top
% level if an ECCE run shall be fast (with condition checking
% switched off) or safe (with condition checking switched on).
% As far as style is concerned, we follow the O.O.-Convention
% for unary boolean functions (a.k.a. tests): they all begin
% with the word "is" (or "has"). Proc "rulHead/2" is exported
% for "traditional" reason (otherwise I would have to change
% too much code in other modules). =========================

:- use_module(auxil,[listToGoal/2, wellDefined/3,errorMsg/2,onlyVariables/1]).

:- use_module(library(lists)).
:- use_module(library(terms)).

%###############################################################

isGoals(X) :-

	var(X),
	!,
	errorMsg(isGoals,X),
	fail.

isGoals([]) :- !.

isGoals([_|T]) :- !, isGoals(T).

isGoals(Y) :-

	errorMsg(isGoals,Y),
	!,
	fail.

%===============================================================

isPrimitive(Term) :- /* form: p(X1,...,Xn) */

	isValue(Term),
	
	Term =.. [Pred|Args],

	isName(Pred),
	
	onlyVariables(Args),
	!.

%---------------------------------------------------------------

isRULified([Term]) :-

	isPrimitive(Term),
	!.

isRULified([]) :- !, fail.

isRULified([T|Terms]) :-
	
	isPrimitive(T),
	
	isRULified(Terms).

%---------------------------------------------------------------

isValue(Parameter) :-

	nonvar(Parameter).

isName(Name) :-

	isValue(Name),

	functor(Name, Name, 0).

isType(Type) :- isName(Type).

%--------------------------------------------------------------
% Check whether an input program is RUL w.r.t. the well-known
% definition of RUL.

isRULprog([]) :- !.

isRULprog(Program) :-

	is_list(Program), /* SICSTUS built-in */

	rul(Program,
	    Program),
	!.

%--------------------------------------------------------------

rul([proc(any/1,                  /* ignore any */
	  [(any(V):-true)])|Definitions], Program) :-
	!,
	var(V),
	
	is_list(Program),
	
	rul(Definitions, Program).

rul([proc(ProcName/1, Def)|Definitions], Program) :-

	isType(ProcName),

	is_list(Program),

	unique(ProcName, Program),

	deterministicDef(ProcName, Def),

	rul(Definitions, Program).

rul([],_).

%--------------------------------------------------------------

unique(any, _) :- !. /* ignore any */

unique(ProcName, Program) :-

	extractProcNames(Program, Names),

	findOnlyOnce(ProcName, Names).

%--------------------------------------------------------------

extractProcNames([proc(any/1,_)|Prog], ProcNames) :-

	/* ignore any */
	!,
	extractProcNames(Prog, ProcNames).

extractProcNames([proc(N/1,_)|Program], [N|ProcNames]) :-

	extractProcNames(Program, ProcNames).

extractProcNames([],[]).

%--------------------------------------------------------------

findOnlyOnce(N, [N]) :-
        !,
	isValue(N).

findOnlyOnce(N, [M|Names]) :-

	isValue(N),
	
	isValue(M),
	
	N \== M,
	!,
	findOnlyOnce(N, Names).

findOnlyOnce(N, [N|Names]) :-

	isValue(N),
	
	non_member(N, Names).

%--------------------------------------------------------------

deterministicDef(_, []) :-
	
	errorMsg(deterministicDef, []),
	!,
	fail. /* No empty definitions! */

deterministicDef(Name, Clauses) :-

	isDefinition(Clauses),
	!,
	extractHeads(Clauses, HeadList),
	!,
	combinePairs(HeadList, HeadPairs),
	!,
	unificationTest(Name, HeadPairs).

%--------------------------------------------------------------

extractHeads([(any(_):-_)], []) :- !. /* ignore any */

extractHeads([(P:-_)], [P]) :-

        isValue(P),
	!.

extractHeads([(P:-_)|Clauses], [P|HeadList]) :-

        isValue(P),
	
        extractHeads(Clauses, HeadList).

%--------------------------------------------------------------

unificationTest(Name, [(P,Q)|HeadPairs]) :-

	isValue(Name),

	isValue(P),
	
	isValue(Q),

	P =.. [Name, ArgP],
	
	Q =.. [Name, ArgQ],

	isValue(ArgP),
	
	isValue(ArgQ),

	ArgP \= ArgQ,
	
	unificationTest(Name, HeadPairs).

unificationTest(_,[]). /* OK */

%==============================================================

isDefinition([C]) :- /* must not be empty! */

	isRULclause(C), !.

isDefinition([D|Definition]) :-

	isRULclause(D),
	!,
	isDefinition(Definition).

isDefinition(X) :-

	errorMsg(isDefinition,X),
	!,
	fail.

%==============================================================

isRULclause((any(V):-true)) :- !, var(V). /* Any-Fact */

isRULclause((Fact:-true)) :-         /* Ordinary Fact */
        !,
	isValue(Fact),

	Fact =.. [Pred,Arg1],

	isType(Pred),
	
	isName(Arg1).

isRULclause((Head:-Tail)) :-  /* Genuine Clause, Head not Fact */

        isRULhead(Head),
	
	isRULtail(Tail),

	variableCondition(Head, Tail),
	!.

%--------------------------------------------------------------

variableCondition(Head, Tail) :-

	term_variables(Head,
		       HeadVariables),
	
	term_variables(Tail,
		       TailVariables),

	/* isOrdered not required! */

	varPermutation(HeadVariables,
		       TailVariables).

%--------------------------------------------------------------

varPermutation([],[]).

varPermutation([X|VarsX], [Y|VarsY]) :-

	var(X),

	var(Y),

	X == Y,
	!,
	varPermutation(VarsX, VarsY).

varPermutation([X|Vars], VarsY) :-

	varMember(X, VarsY),
	!,
	append(Vars, [X], VarsX),

	varPermutation(VarsX, VarsY).

%--------------------------------------------------------------

varMember(X, [Y|_]) :-

	var(X),

	var(Y),

	X == Y,
	!.

varMember(X, [_|VarsY]) :-

	varMember(X, VarsY).

%==============================================================

isRULhead(Pred) :-

	term_variables(Pred, Variables),

	Variables \== [], /* New, BugFix [SG:5.10.2001] */

	isValue(Pred),

	Pred =.. [NameP, Functor],

	isName(NameP),

	isValue(Functor),

	Functor =.. [NameF|Arguments],

	isName(NameF),

	/* only variables and no double variables */

	varPermutation(Arguments,
		       Variables),
	!.

%==============================================================

isRULtail(SubGoals) :-

	isValue(SubGoals),

	listToGoal(GoalList, SubGoals),

	allGoals(GoalList),
	!.

%--------------------------------------------------------------

allGoals([]) :- !, fail.

allGoals(Goals) :-

	/* goals: non-empty AND simple */

	isSimple(Goals).

%==============================================================

isRULgoal(any(V)) :- var(V), !.

isRULgoal(Pred) :-

	isValue(Pred),

	Pred =.. [Name,Var],

	isType(Name),

	var(Var),
	!.

%--------------------------------------------------------------

% (PUBLIC)
rulHead(Name, Predicate) :-

	/* obsolete but still exported */
	/* for reasons of legacy code. */
	/* Body code modified in terms */
	/* of new procedures of above. */
                                                       
	Predicate =.. [Name,_],

	isRULhead(Predicate).

%==============================================================

isSimple([]) :- !. /* difference between  */
                   /* Simple and allGoals */

isSimple([C|ComposedConstraint]) :-

	isRULgoal(C),

	isSimple(ComposedConstraint).

%==============================================================

isNormalRCD(rul__constraint__declaration(C,_RULProg)) :- isNormal(C).

%==============================================================

isNormal(CompConstr) :-

	isSimple(CompConstr),
	
	/* simple AND no double variables */

	term_variables(CompConstr, Vars),

	same_length(CompConstr, Vars),
	!.

%==============================================================

isConstrDecl([]).
isConstrDecl([C|Constr]) :-

	isConstraint(C),
	isConstrDecl(Constr).

% ### Some Constraint Declaration Laws: ###
% isConstrDecl(Constr) <== isSimple(Constr).
% isConstrDecl(Constr) <== allGoals(Constr).
% isConstrDecl(Constr) <== isNormal(Constr).
% isSimple(Constr)     <== isNormal(Constr).
% isSimple(Constr)     <== allGoals(Constr).
% isConstraint(C)      <==     isRULgoal(C).

%==============================================================

isOrdered((_:-true)) :- !.

isOrdered((Head:-Tail)) :-

        isRULhead(Head),

	isRULtail(Tail),
	!,
	Head =.. [_,Functor],

	Functor =.. [_|Variables],

	listToGoal(GList, Tail),
	
        /* if this test fails, then apply */
        /* auxil:orderSubGoals/2 onto the */
        /* RUL program of that clause.    */

	testOrder(Variables, GList),
	!.

%--------------------------------------------------------------

testOrder([V|Vars], [G|List]) :-

	var(V),

	isValue(G),

	G =.. [_,X],

	var(X),

	X == V,

	testOrder(Vars, List).

testOrder([],[]).


% CONSTRAINTDATASTRUCTURE:========================================
% Here we define the data structure "rul__constraint__declaration"
% which will be used by ECCE internally. Double underscores ensure
% that no name clash with any user-defined data structures occurs,
% as ECCE does not allow the user to use the double underscore __.
% You can add this predicate into your code wherever you want to
% ensure that an input RCD is really a RUL constraint declaration.
% ================================================================

isRCD(rul__constraint__declaration([],[])) :- !.

isRCD(rul__constraint__declaration(ConstraintDeclaration,
				   ConstraintDefinition)) :-
	
	isConstrDecl(ConstraintDeclaration),

	isRULprog(ConstraintDefinition),

	allDefined(ConstraintDeclaration,
		   ConstraintDefinition),
	!.

%--------------------------------------------------------------

allDefined([C|Constraints], RULprogram) :-

	isConstraint(C),

	C =.. [Name,_],
	
	wellDefined(Name, _CDef, RULprogram),

%%%	isDefinition(CDef), /* redundant */
	
	allDefined(Constraints, RULprogram).

allDefined([],_).

%==============================================================

% isConstraint(C) :- isRULgoal(C), !. /* redundant */

isConstraint(C) :- /* unary */

	isValue(C),

	C =.. [Name,_],

	isType(Name),
	!.

%==============================================================

isConsistent(Prog) :- /* all subgoals must have definitions */

	collectGoalNames(Prog, GN),
	collectProcNames(Prog, PN),

	remove_duplicates(GN, GoalNames),
	remove_duplicates(PN, ProcNames),

	subSet(GoalNames, ProcNames),
	!.

%--------------------------------------------------------------

collectProcNames([proc(P/1,_)|Procs], [P|Names]) :-

	collectProcNames(Procs, Names).

collectProcNames([],[]).

%--------------------------------------------------------------

collectGoalNames([proc(_/1, Def)|Procs], GoalNames) :-

	scanClauses(Def, Names),

	collectGoalNames(Procs, MoreNames),

	append(Names, MoreNames, GoalNames).

collectGoalNames([],[]).

%--------------------------------------------------------------

scanClauses([(_:-SubGoals)|Clauses], GoalNames) :-

        listToGoal(GoalList, SubGoals),

	browseSubGoals(GoalList, Names),

	scanClauses(Clauses, MoreNames),

	append(Names, MoreNames, GoalNames).

scanClauses([],[]).

%--------------------------------------------------------------

browseSubGoals([true|Goals], Names) :-
	!,
	browseSubGoals(Goals, Names).

browseSubGoals([G|Goals], [P|Names]) :-

	G =.. [P,_],

	browseSubGoals(Goals, Names).

browseSubGoals([],[]).

%=======================================================
% COMBINEPAIRS: We don't need twins of the form (P,P) so
% we exclude them by construction for sake of efficiency.
% We also do not need the mirror (B,A) of a pair (A,B).
% ------------------------------------------------------

combinePairs([_], []).                                 
                                                       
combinePairs([X|Names], OutputPairs) :-                
	                                               
	construction(X, Names, XPairs),
	!,
	combinePairs(Names, RestPairs), 
	!,
	append(XPairs, RestPairs, OutputPairs).

construction(Xa, [Xb], []) :-

	Xa == Xb,
	!.

construction(X, [Y], [(X,Y)]) :-

	X \== Y,
	!.

construction(Xa, [Xb|Ys], Pairs) :-

	Xa == Xb,
	!,                                               
	construction(Xa, Ys, Pairs).                    
                                                       
construction(X, [Y|Ys], [(X,Y)|Pairs]) :-      
                                                       
	X \== Y,
	
	construction(X, Ys, Pairs).                    

%##############################################################
%############################# END ############################
%##############################################################
