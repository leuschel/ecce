%###############################################################
%################## MODULE: TYPE INTERSECTION ##################
%##### (Derived and modified from John Gallagher's Tool) #######
%###############################################################

:- module(interSection, [interSection/7]).

:- use_module(library(lists)).

:- use_module(self_check_rul).

:- use_module(subType,[subType/3]).

:- use_module(prePostCon,[isType/1,isRULprog/1,isDefinition/1,isOrdered/1]).

:- use_module(auxil,[wellDefined/3,newName/2,removeClause/4,prepareTriple/10,orderSubGoals/2,errorMsg/2]).

%:- ['debug2.pl']. /* Local: only in Stefan's Environment */
%###############################################################


% (PUBLIC) INTERSECTION: ==========================================
% Given two input type names and an according RUL program (which is
% defining the input types), we check if there exists a non-trivial
% intersection type. If the two input types are in subtype relation,
% the subtype is also the intersection type. Otherwise, we have to
% compute the new intersection type - which will be empty in case
% that the two input types have no common ground instance. A (num-
% ber) input index is necessary for the generation of the new name
% of the intersection type. Another numerical output index (which
% we don't really need) indicates how many subdefinitions have been
% generated in order to define the new intersection type. I/O-Usage
% is thus (+,+,-,-,+,+,-). Please note the similarity between this
% procedure and the upperBound procedure in the upperBound module!
% =================================================================

:- initialization(assert_pre(interSection:interSection(In1,In2,_,_,In3,In4,_Dummy),
	      (prePostCon:isType(In1),
	       prePostCon:isType(In2),
	       prePostCon:isRULprog(In3),
	       number(In4)))).

:- initialization(assert_post(interSection:interSection(_,_,Out1,Out2,_,_,_Dummy),
	       (prePostCon:isType(Out1),
		prePostCon:isRULprog(Out2)))).

interSection(any, any, any, [], _, I, I) :- !.

interSection(ThisType, any, ThisType, [], _, I, I) :- !.

interSection(any, ThisType, ThisType, [], _, I, I) :- !.

interSection(ThisType, SuperType, ThisType, [], RULprog, I,I) :-

	subType(ThisType, SuperType, RULprog),
	!.

interSection(SuperType, ThisType, ThisType, [], RULprog, I,I) :-

	subType(ThisType, SuperType, RULprog),
	!.	

interSection(TypeName1, TypeName2, InterSectionName,
	     [proc(InterSectionName/1, InterSectionDef)|SubDefs],
	     InputProgram, InputIndex, OutputIndex) :-

	orderSubGoals(InputProgram, RULprogram),

	wellDefined(TypeName1, TypeDef1, RULprogram),
	
        wellDefined(TypeName2, TypeDef2, RULprogram),
      
        newName(InputIndex, NewName),

	name(NewName, NameCode),

	name(is_, FlagCode), /* Flag indicating interSection */

	append(FlagCode, NameCode, IdentCode),
	
        name(InterSectionName, IdentCode),
	
        NextIndex is InputIndex + 1,
      
        lowerType(TypeDef1, TypeDef2, InterSectionDef,
		  InterSectionName, TypeName1, TypeName2,
		  InterSectionName, WorkTriples, ReferenceTriples,
		  NextIndex, NewIndex, RULprogram),

        InterSectionDef = [(_:-_)|_], /* Non-Empty Definition! */

        make_IS_def(ReferenceTriples, TypeName1, TypeName2,
		    InterSectionName, WorkTriples, SubDefs,
		    NewIndex, OutputIndex, RULprogram),

	isDefinition(SubDefs),
	!.

interSection(Name1, Name2, _,_,_,_,_) :-

	errorMsg(interSection, (Name1,Name2)),
	!,
	fail.

%-----------------------------------------------------------------

lowerType([], _, [], _,_,_,_,_,_, FinalIndex, FinalIndex, _) :- !.

lowerType(_, [], [], _,_,_,_,_,_, FinalIndex, FinalIndex, _) :- !.

lowerType([(Head1:-Goals1)|Clauses1], TypeDef2,
	  [(IHead:-IGoals)|IClauses], NameIS,
	  TName1, TName2, NameLT, WorkTriples,
	  ReferenceTriples, OldIndex, NewIndex,
	  RULprog) :-

	/* PRECONDITION */
	isOrdered((Head1:-Goals1)),
	
	allOrdered(TypeDef2),

      removeClause(Head1, RmvGoals2,
		   TypeDef2, ResClauses2),
      !,

      Head1=..[_,      SameFunctor],
      IHead=..[NameIS, SameFunctor],

      interSectBody(Goals1, RmvGoals2, IGoals, TName1, TName2,
		    NameLT, WorkTriples, ReferenceTriples,
		    OldIndex, NextIndex, RULprog),

      lowerType(Clauses1, ResClauses2, IClauses, NameIS, TName1,
		TName2, NameLT, WorkTriples, ReferenceTriples,
		NextIndex, NewIndex, RULprog).


lowerType([_|TDef1], TDef2, IDef, IName, Name1, Name2, LName,
	  WTriples, RTriples, OldIndex, NewIndex, RULprogram) :-
	
	lowerType(TDef1, TDef2, IDef, IName, Name1, Name2, LName,
		  WTriples, RTriples, OldIndex, NewIndex, RULprogram).

%---------------------------------------------------------------------

make_IS_def([], _,_,_,_, [], FinalIndex, FinalIndex, _) :- !.

make_IS_def(ReferenceTriples, TName1, TName2,
	    NameIS, WorkTriples, SubDefsIS,
	    InIndex, OutIndex, RULprog) :- 

      eachInterSect(ReferenceTriples, FirstDefPart, NewTriples,
		    TName1, TName2, NameIS, WorkTriples,
		    InIndex, NewIndex, RULprog),

      make_IS_def(NewTriples, TName1, TName2, NameIS,
		  WorkTriples, SecondDefPart, NewIndex,
		  OutIndex, RULprog),

      append(FirstDefPart, SecondDefPart, SubDefsIS).

%----------------------------------------------------------

interSectBody(true, true, true, _,_,_,_,_,
	      FinalIndex, FinalIndex,_ ) :- !.

interSectBody((Atom1,Goals1), (Atom2,Goals2), (AtomI,GoalsI),
	      Type1, Type2, IType, InTriples, RefTriples,
	      InIndex, OutIndex, RULprog) :- 
      !,
      interSectAtom(Atom1, Atom2, AtomI, Type1, Type2, IType,
		    InTriples, RefTriples, InIndex, NextIndex,
		    RULprog),
      
      interSectBody(Goals1, Goals2, GoalsI, Type1, Type2, IType,
		    InTriples, RefTriples, NextIndex, OutIndex,
		    RULprog).

interSectBody(Atom1, Atom2, AtomI, Type1, Type2, IType,
	      InTriples, RefTriples, InIndex, OutIndex,
	      RULprog) :-
	
      interSectAtom(Atom1, Atom2, AtomI, Type1, Type2, IType,
		    InTriples, RefTriples, InIndex, OutIndex,
		    RULprog).

%------------------------------------------------------------

eachInterSect([],[],_,_,_,_,_, FixIndex, FixIndex, _) :- !.

eachInterSect([(Name1,Name2,IName)|RefTriples],
	      [proc(IName/1,IDef)|IProcs], NewTriples,
	      Type1, Type2, IType, InTriples, InIndex,
	      OutIndex, RULprog) :-
	
	wellDefined(Name1, Def1, RULprog),
	wellDefined(Name2, Def2, RULprog),
	
	NewIndex is InIndex + 1,
	!,
	lowerType(Def1, Def2, IDef, IName, Type1, Type2, IType,
		  InTriples, NewTriples, NewIndex, ResultIndex,
		  RULprog),
	
	eachInterSect(RefTriples, IProcs, NewTriples, Type1,
		      Type2, IType, InTriples, ResultIndex,
		      OutIndex, RULprog).

%--------------------------------------------------------------

interSectAtom(Pred1, Pred2, IPred, Type1, Type2, IType,
	      InTriples, RefTriples, InIndex, OutIndex,
	      RULprog) :-

	Pred1 =.. [Name1, SameArgument],
	Pred2 =.. [Name2, SameArgument],

	nextInterSect(Name1, Name2, IName, Type1, Type2, IType,
		      InTriples, RefTriples, InIndex, OutIndex,
		      RULprog),
	
	IPred =.. [IName, SameArgument].

%--------------------------------------------------------------

nextInterSect(SubType, SuperType, SubType, _,_,_,_,_,
	      FixIndex, FixIndex, RULprog) :-
	
      subType(SubType,SuperType,RULprog),
      !.

nextInterSect(SuperType, SubType, SubType, _,_,_,_,_,
	      FixIndex, FixIndex, RULprog) :-
	
      subType(SubType,SuperType,RULprog),
      !.

nextInterSect(Name1, Name2, IName, Type1, Type2, IType,
	      InTriples, RefTriples, InIndex,OutIndex, _) :-
	
	prepareTriple(Name1, Name2, IName, Type1, Type2, IType,
		      InTriples, RefTriples, InIndex,OutIndex).

%--------------------------------------------------------------

allOrdered([]).

allOrdered([C|Clauses]) :-

	isOrdered(C),

	allOrdered(Clauses).

%###############################################################
%############################# END #############################
%###############################################################

