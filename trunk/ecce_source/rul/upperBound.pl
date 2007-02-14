%###############################################################
%############# MODULE: UPPER-BOUND CONSTRUCTION ################
%###############################################################

:- module(upperBound, [upperBound/7]).

:- use_module(library(lists)).

:- use_module(self_check_rul).

:- use_module(prePostCon,[isType/1,isRULprog/1,isDefinition/1,isOrdered/1]).

:- use_module(auxil,[newName/2, wellDefined/3,errorMsg/2,orderSubGoals/2,removeClause/4,prepareTriple/10]).

:- use_module(subType,[subType/3]).

:- use_module(gensym2,[seed/1]).

%##############################################################


% (PUBLIC) UPPERBOUND: ========================================
% upperBound takes the names of two regular types together with  
% the representation of a program defining them and it computes  
% a regular type which is a upperbound of both the input types.  
% The generated upperBound type will get a new name also genera- 
% ted by this procedure. In order to generate the new name, the  
% procedure needs some number index as input. Based on the input 
% index, the names of the new predicates (defining the new type) 
% are computed by index incrementation. The result can be found  
% in the output index. The seven parameters of the procedure are 
% used as: (+ + - - + + -). The newly generated type must be an  
% upper type of each of the two input types. As we were dealing  
% with pairs of type names in the sub-typing procedures, so are  
% we dealing with triples of type names in this procedure here.  
% UpperBoundType occurs twice in the upperType-procedure call    
% because of an equal-value requirement with respect to an im-   
% plicitly underlying comparison. ATTENTION: input program must  
% be in RUL! Please note the similarity between this procedure
% and the interSection procedure in the interSection module!
% The ReferenceTriples are required for comparison. [18.9.2001:
% NEW]: Check subtype properties in advance! ==================

:- initialization(assert_pre(upperBound:upperBound(In1,In2,_,_,In3,In4,_Dummy),
	      (prePostCon:isType(In1),
	       prePostCon:isType(In2),
	       prePostCon:isRULprog(In3),
	       number(In4)))).

:- initialization(assert_post(upperBound:upperBound(_,_,Out1,Out2,_,_,_Dummy),
	       (prePostCon:isType(Out1),
		prePostCon:isRULprog(Out2)))).

%--------------------------------------------------------------

upperBound(any,_,any,[],_,Num,Num) :- !.

upperBound(_,any,any,[],_,Num,Num) :- !.

upperBound(SubType,SuperType,SuperType,[],Prog,Num,Num) :-

	subType(SubType, SuperType, Prog),
	!.

upperBound(SuperType,SubType,SuperType,[],Prog,Num,Num) :-

	subType(SubType, SuperType, Prog),
	!.

upperBound(TypeName1, TypeName2, UpBdName,
	   [proc(UpBdName/1,UpBdDef)|ProcDefs],
	   InputProgram, InputIndex, OutputIndex) :-

	orderSubGoals(InputProgram, RULprogram),
	
        wellDefined(TypeName1, TypeDef1, RULprogram),
	
        wellDefined(TypeName2, TypeDef2, RULprogram),
	
        newName(InputIndex, NewName),

	name(NewName, NameCode),

	name(ub_, FlagCode), /* Flag indicating upperBound */

	append(FlagCode, NameCode, IdentCode),
	
        name(UpBdName, IdentCode),
	
        NextIndex is InputIndex + 1,
        
        upperType(TypeDef1, TypeDef2, UpBdDef,
		  UpBdName, TypeName1, TypeName2,
		  UpBdName, InputTriples, ReferenceTriples,
		  NextIndex, NewIndex, RULprogram),
      
        make_UB_def(ReferenceTriples, TypeName1, TypeName2,
		    UpBdName, InputTriples, ProcDefs,
		    NewIndex, OutputIndex, RULprogram),
	!.

upperBound(X,Y,_,_,_,_,_) :-

	errorMsg(upperBound,(X,Y)),
	!,
	fail.

%------------------------------------------------------------

make_UB_def([], _,_,_,_, [], FinalIndex, FinalIndex, _) :- !.

make_UB_def(ReferenceTriples, TypeName1, TypeName2, UpperName,
	    InputTriples, ProcDefs, IndexInput, IndexOutput,
	    RULprogram) :-
	
      eachUpperBound(ReferenceTriples, FirstDefs, IntermediateTriples,
		     TypeName1, TypeName2, UpperName, InputTriples,
		     IndexInput, IntermediateIndex, RULprogram),
      
      make_UB_def(IntermediateTriples, TypeName1, TypeName2,
		  UpperName, InputTriples, RestDefs,
		  IntermediateIndex, IndexOutput, RULprogram),
      
      append(FirstDefs, RestDefs, ProcDefs).


% UPPERTYPE: ==================================================
% Checks if three type definitions for (t1, t2, t) fullfil the  
% upper type condition such that t > t1 and t > t2. There's not 
% much to do in case one of these definitions is found empty []
% (Recursion base case). In the non-trivial case we must treat
% the type definitions clause by clause (which is done calling
% the removeClause procedure) and then further check the remai-
% ning clause bodies. =========================================

upperType([], [], [], _,_,_,_,_,_, FinalIndex, FinalIndex, _) :- !.

upperType([], [(Predicate:-SameGoals)|OtherClauses],
	      [(UpperPred:-SameGoals)|UpperClauses],
	      UpperName, Type1, Type2, UpperBound,
	      InputTriples, ReferenceTriples, IndexInput,
	      IndexOutput, RULprogram) :-

	/* PRECONDITION */
	isOrdered((Predicate:-SameGoals)),
	
      !,
      Predicate =.. [_,         SameFunctor],
      UpperPred =.. [UpperName, SameFunctor],
      
      upperType([], OtherClauses, UpperClauses, UpperName,
		Type1, Type2, UpperBound, InputTriples,
		ReferenceTriples, IndexInput, IndexOutput,
		RULprogram).

upperType([(Predicate:-SameGoals)|OtherClauses], [],
	  [(UpperPred:-SameGoals)|UpperClauses],
	  UpperName, Type1, Type2, UpperBound,
	  InputTriples, ReferenceTriples, IndexInput,
	  IndexOutput, RULprogram) :-
	
	/* PRECONDITION */
	isOrdered((Predicate:-SameGoals)),
	
      !,
      Predicate =.. [_,         SameFunctor],
      UpperPred =.. [UpperName, SameFunctor],
      
      upperType(OtherClauses, [], UpperClauses, UpperName,
		Type1, Type2, UpperBound, InputTriples,
		ReferenceTriples, IndexInput, IndexOutput,
		RULprogram).

upperType([(Pred1:-Goals1)|Clauses1], Clauses2,
	  [(UPred:-UGoals)|UClauses], UName, Type1, Type2,
	  UType, InputTriples, ReferenceTriples, IndexInput,
	  IndexOutput, RULprogram) :-

	/* PRECONDITION */
	isOrdered((Pred1:-Goals1)),
	allOrdered(Clauses2),
	
      removeClause(Pred1, TheRemovedGoals,
		   Clauses2, ResultClauses2),
      !,
      Pred1 =.. [_,     SameFunctor],
      UPred =.. [UName, SameFunctor],
      
      upperBoundBody(Goals1, TheRemovedGoals, UGoals, Type1,
		     Type2, UType, InputTriples, ReferenceTriples,
		     IndexInput, IntermediateIndex, RULprogram),
      
      upperType(Clauses1, ResultClauses2, UClauses, UName,
		Type1, Type2, UType, InputTriples, ReferenceTriples,
		IntermediateIndex, IndexOutput, RULprogram).

upperType([(Pred1:-SameGoals)|Clauses1], Clauses2,
	  [(UPred:-SameGoals)|UClauses], UName, Type1,
	  Type2, UType, InputTriples, ReferenceTriples,
	  IndexInput, IndexOutput, RULprogram) :-

	/* PRECONDITION */
	isOrdered((Pred1:-SameGoals)),
	allOrdered(Clauses2),
	
      Pred1 =.. [_,     SameFunctor],
      UPred =.. [UName, SameFunctor],
      
      upperType(Clauses1, Clauses2, UClauses, UName, Type1,
		Type2, UType, InputTriples, ReferenceTriples,
		IndexInput, IndexOutput, RULprogram).


% UPPERBOUNDBODY: ===============================================
% upperBoundBody supports the upperType-procedure in decomposing  
% the terms under consideration. Eventually we have to check all 
% atoms of a multigoal in order to construct the upper type. The 
% (.,.)-operator used for this dexomposition is SICSTUS built-in.
% No wonder that the structure of this procedure is very similar 
% to the procedure subTypePairs of above! (One could have merged 
% this procedure with the upperBoundAtom procedure, similar to   
% what I did with the makePairs procedure, but I did not do it   
% because this procedure here is already too big and heavy.)
% ==============================================================

upperBoundBody(true, true, true, _,_,_,_,_,
	       FinalIndex, FinalIndex, _) :- !.

upperBoundBody((Atom1,Goals1), (Atom2,Goals2), (AtomU,GoalsU),
	       Type1, Type2, UType, InputTriples, ReferenceTriples,
	       IndexInput,IndexOutput,RULprogram) :-
	
	%user:debug_print('UB-BODY: '),
	%user:debug_print((Atom1,Goals1)),
	%user:debug_print((Atom2,Goals2)),
	%user:debug_nl,
	
      !,
      upperBoundAtom(Atom1, Atom2, AtomU, Type1, Type2, UType,
		     InputTriples, ReferenceTriples, IndexInput,
		     IntermediateIndex, RULprogram),
      
      upperBoundBody(Goals1, Goals2, GoalsU, Type1, Type2,
		     UType, InputTriples, ReferenceTriples,
		     IntermediateIndex, IndexOutput, RULprogram).

      %user:debug_print((AtomU,GoalsU)),
      %user:debug_nl

upperBoundBody(Atom1, Atom2, AtomU, Type1, Type2, UType,
	       InputTriples, ReferenceTriples, IndexInput,
	       IndexOutput, RULprogram) :-
	
      upperBoundAtom(Atom1, Atom2, AtomU, Type1, Type2, UType,
		     InputTriples, ReferenceTriples, IndexInput,
		     IndexOutput, RULprogram).


% EACHUPPERBOUND: ===============================================
% "eachUpperBound" supports the "makeTriples"-procedure. It goes 
% through the triple lists and makes sure that all triple in the 
% list have the property {t > t1 AND t > t2}, where (t1,t2,t) is 
% in the list and ">" is the uppertype relation on the correspon-
% ding types in the input program representation. By the way new 
% indices are generated too. Because of far reaching call depen- 
% dencies, we have to carry on loads of formal parameters here!  
% If everything	goes OK, then UpperType and UpperBound have THE  
% SAME VALUE, as specified in the upperBound-procedure above.
% ==============================================================
                                                                
eachUpperBound([], [], _,_,_,_,_, FinalIndex, FinalIndex, _) :- !.

eachUpperBound([(TypeName1,TypeName2,UTypeName)|ReferenceTriples],
	       [proc(UTypeName/1,UTypeDef)|ProcDefs],
	       IntermediateTriples, Type1, Type2, UType,
	       InputTriples, IndexInput, IndexOutput, RULprogram) :-
	
      wellDefined(TypeName1, TypeDef1, RULprogram),
      wellDefined(TypeName2, TypeDef2, RULprogram),
      
      NextIndex is IndexInput + 1,
      !,
      upperType(TypeDef1, TypeDef2, UTypeDef, UTypeName, Type1,
		Type2, UType, InputTriples, IntermediateTriples,
		NextIndex, NewIndex, RULprogram),
      
      eachUpperBound(ReferenceTriples, ProcDefs, IntermediateTriples,
		     Type1, Type2, UType, InputTriples, NewIndex,
		     IndexOutput, RULprogram).

% UPPERBOUNDATOM: ===============================================
% upperBoundAtom supports the upperBoundBody-procedure. We check 
% that we have reached the level of atoms in our type decomposi- 
% tion process. We are done where we discover that the functors  
% of Pred1, Pred2 and UpperPred are the same, and if their next  
% upper	bound is also well defined. For this purpose we have to  
% check the super type relation holds which can be of course al- 
% so expressed in terms of sub type. This is done by the next-   
% UpperBound procedure. =========================================

upperBoundAtom(Pred1, Pred2, UPred, Type1, Type2, UType,
	       InputTriples, ReferenceTriples, IndexInput,
	       IndexOutput, RULprogram) :-
	
      Pred1 =.. [Name1, SameFunctor],
      Pred2 =.. [Name2, SameFunctor],
      
      nextUpperBound(Name1, Name2, UName, Type1, Type2, UType,
		     InputTriples, ReferenceTriples, IndexInput,
		     IndexOutput, RULprogram),
      
      UPred =.. [UName, SameFunctor].


% NEXTUPPERBOUND: ===============================================
% nextUpperBound supports the upperBoundAtom-procedure. To check 
% if a triple (t1, t2, t) is OK we must prove that t is an upper 
% type of t1 as well as of t2. This means viceversa that both t1 
% and t2 must be subtypes of t such that we can reuse "subType"  
% as defined above. If two of the inputs (t1,t1,t) are the same, 
% there is no need to create a new type-name triple. If all the  
% three names are different, we have to prepare the generation   
% of a new triple for the triple list. =========================
                                            
nextUpperBound(SubType, SuperType, SuperType, _,_,_,_,_,
	       FinalIndex, FinalIndex, RULprogram) :-
	
      subType(SubType, SuperType, RULprogram),
      !.

nextUpperBound(SuperType, SubType, SuperType, _,_,_,_,_,
	       FinalIndex, FinalIndex, RULprogram) :-
	
      subType(SubType, SuperType, RULprogram),
      !.

nextUpperBound(TypeName1, TypeName2, UpperName, Type1,
	       Type2, UType, InputTriples, ReferenceTriples,
	       IndexInput, IndexOutput, _) :-
	
      prepareTriple(TypeName1, TypeName2, UpperName, Type1,
		    Type2, UType, InputTriples, ReferenceTriples,
		    IndexInput, IndexOutput).

%----------------------------------------------------------

allOrdered([]).

allOrdered([C|Clauses]) :-

	isOrdered(C),

	allOrdered(Clauses).

%###############################################################
%############################### END ###########################
%###############################################################
