%###############################################################
%###################### MODULE: SUB-TYPING #####################
%###############################################################

:- module(subType, [subType/3]).

:- use_module(self_check_rul).
:- use_module(auxil,[wellDefined/3,removeClause/4,orderSubGoals/2]).
:- use_module(prePostCon,[isType/1,isRULprog/1]).
%################################################################


% (PUBLIC) SUBTYPE: ===============================================
% check if SubTypeName represents a subtype of the type represented
% by SuperTypeName with respect to the given program represented by
% the RULprogram input parameter.The call to "makePairs" carries an
% anonymous output variable, because "subType" is only an interface
% to makePairs (which makes a list of type name pairs being in sub-
% type relation and checks the occurrance of the input typenames in
% that list). The input program MUST be in RUL otherwise the result
% of the subtype analysis is known to be not reliable! The intended
% I/O-Usage is check-only, thus (+,+,+). ==========================

:- initialization(assert_pre(subType:subType(In1,In2,In3),
	      (prePostCon:isType(In1),
	       prePostCon:isType(In2),
	       prePostCon:isRULprog(In3)))).

:- initialization(assert_post(subType:subType(_,_,_),true)).

%-----------------------------------------------------------------

subType(_, any, _) :- !.

subType(SameTypeName, SameTypeName, _) :- !.

subType(SubTypeName, SuperTypeName, InputProgram) :-

	orderSubGoals(InputProgram, RULprogram),

	makePairs(SubTypeName, SuperTypeName, [], _, RULprogram).

% MAKEPAIRS: =====================================================
% Let t1 be an instance of SubTypeName and t2 an instance of Super-
% TypeName. If the type of t1 is a sub type of the type of t2 with
% respect to a given program then a pair (t1,t2) is appended to an
% input pair-list and the result is output. Trivial: each type is
% a sub type of type "any", and each type is a sub type of itself.
% ================================================================

makePairs(_, any, SamePairs, SamePairs, _) :- !.

makePairs(SameName, SameName, SamePairs, SamePairs, _) :- !.

makePairs(SubTypeName, SuperTypeName,
	  CompletePairList, CompletePairList, _) :-
	
      inPairList(SubTypeName, SuperTypeName,
		 CompletePairList),
      !.

makePairs(SubTypeName, SuperTypeName,
	  InputPairs, OutputPairs, RULprogram) :-
	
      SubTypeName \== any,
      
      SuperTypeName \== any,
      !,
      wellDefined(SubTypeName, SubTypeDef, RULprogram),
      
      wellDefined(SuperTypeName, SuperTypeDef, RULprogram),
      !,
      parseClauses(SubTypeDef, SuperTypeDef,
		   [(SubTypeName,SuperTypeName)|InputPairs],
		   OutputPairs, RULprogram).


% INPAIRLIST: ================================================
% inPairList supports the makePairs-procedure. Checks whether 
% two typenames occur as pair in a list of pairs of typenames.
% Mind the Cut! ==============================================

inPairList(SubTypeName, SuperTypeName,
	   [(SubTypeName,SuperTypeName)|_]) :- !.

inPairList(SubTypeName, SuperTypeName, [_|PairList]) :-
	
      inPairList(SubTypeName, SuperTypeName, PairList).


% PARSECLAUSES: ======================================================
% parseClauses supports makePairs-procedure. Definition of the subtype
% is compared with the clauses of the super type, whereby the list of
% typename pairs (the types of which are in subtype relation) is upda-
% ted. The RULprogram is not used here but it is necessary because of
% far reaching call chains to the "inProgram"-procedure. Given a sub-
% type definition and a supertype definition, the supertype definition
% is decomposed clause by clause and the pairs are constructed via the
% subtype goals and the supertype goals. =============================

parseClauses([], _, SamePairs, SamePairs, _). 

parseClauses([(SubTypePred:-SubTypeGoals)|SubTypeClauses],
	     SuperTypeDef, InputPairs, OutputPairs,
	     RULprogram) :-
	
      removeClause(SubTypePred, GoalsTakenFromSuperTypeDef,
		   SuperTypeDef, RemainingSuperTypeClauses),
      
      subTypePairs(SubTypeGoals, GoalsTakenFromSuperTypeDef,
		   InputPairs, IntermediatePairs, RULprogram),
      
      parseClauses(SubTypeClauses, RemainingSuperTypeClauses,
		   IntermediatePairs, OutputPairs, RULprogram).


% SUBTYPEPAIRS: =======================================================
% Take two goals and a list of type name pairs as input. Scan the goals
% and build new type name pairs from them. Detected pairs are put into
% the output list. Please note the use of the SICSTUS comma-operator in
% the second case: (.,.) looks like a tuple but it isn't! (See SICSTUS
% reference manual.) ==================================================

subTypePairs(true, true, SamePairList, SamePairList, _) :- !.

subTypePairs((Pred1,MoreGoals1), (Pred2,MoreGoals2),
	     InputPairs, OutputPairs, RULprogram) :-
      !,
      pairRecursion(Pred1, Pred2, InputPairs,
		    IntermediatePairs, RULprogram),
      
      subTypePairs(MoreGoals1, MoreGoals2,
		   IntermediatePairs,
		   OutputPairs, RULprogram).

subTypePairs(Goal1, Goal2, InputPairs,
	     OutputPairs, RULprogram) :-
	
      pairRecursion(Goal1, Goal2, InputPairs,
		    OutputPairs, RULprogram).

%----------------------------------------------------------------

pairRecursion(Goal1, Goal2, InputPairs,
	      OutputPairs, RULprogram) :-
	
      functor(Goal1, SubTypeName, 1),
      
      functor(Goal2, SuperTypeName, 1),
      
      makePairs(SubTypeName, SuperTypeName,
		InputPairs, OutputPairs, RULprogram).

%###############################################################
%############################# END #############################
%###############################################################
