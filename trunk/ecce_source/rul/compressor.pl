%#################################################################
%####################### MODULE: COMPRESSION #####################
%#################################################################

:- module(compressor, [compress/2]).

:- use_module(library(lists)).
:- use_module(library(terms)).

:- use_module(self_check_rul).

:- use_module(prePostCon,[isRCD/1,isNormal/1]).

:- use_module(auxil,[replaceAllOccurrences/5,pruneRCD/2]).

:- use_module(subType,[subType/3]).

%#########################################################
%(PUBLIC) COMPRESS: ======================================
% Isn't it ridiculous what a stupid monkeyDonkey I am! Had
% I written a clumsy lenghty buggy compressor from scratch,
% instead of simply re-using all the bits and pieces which
% are already at hand! Well, eventually I got the idea and
% here is a new and more elegant compressor version. Later
% I might write a more efficient one according to Ullman's
% automaton minimisation but for now this'll be sufficient.
% ========================================================

:- initialization(assert_pre(compressor:compress(In,_),
	      (prePostCon:isRCD(In),
		  In = rul__constraint__declaration(InC,_),
		  prePostCon:isNormal(InC)))).

:- initialization(assert_post(compressor:compress(_,Out),
	       (prePostCon:isRCD(Out),
		   Out = rul__constraint__declaration(OutC,_),
		   prePostCon:isNormal(OutC)))).

%############################################################
%------------------------------------------------------------

allTypes([proc(T/1,_)|RULprog], [T|TypeList]) :-
	!,
	allTypes(RULprog, TypeList).

allTypes([],[]).

%------------------------------------------------------------

equivalentType(any, any, _) :- !.

equivalentType(SameT, SameT, _) :-

	atom(SameT),
	!.

equivalentType(TX, TY, RULprog) :-

	subType(TX, TY, RULprog),
	subType(TY, TX, RULprog),
	!.

equivalentType(_,_,_) :- !, fail.

%------------------------------------------------------------

equivalenceClasses([T|TypeList], [ClassT|Classes], RULprog) :-

	findClass(T, [T|TypeList], ClassT, RestList, RULprog),
	!,
	equivalenceClasses(RestList, Classes, RULprog).

equivalenceClasses([],[],_).

%------------------------------------------------------------

findClass(T, [TY|TypeList], [TY|Class], RestList, RULprog) :-

	equivalentType(T, TY, RULprog),
	!,
	delete(TypeList, TY, SmallerList),
	!,
	findClass(T, SmallerList, Class, RestList, RULprog).

findClass(T, [NE|TypeList], Class, [NE|RestList], RULprog) :-

	/* case: Not Equivalent */

	delete(TypeList, NE, SmallerList),
	!,
	findClass(T, SmallerList, Class, RestList, RULprog).

findClass(_,[],[],[],_).

%============================================================
%[PUBLIC]

compress(FatRCD, SlimRCD) :-

	pruneRCD(FatRCD, BaseRCD),
	!,
	shrinkRCD(BaseRCD, SlimRCD),
	!,
	showResult(FatRCD, SlimRCD).

%============================================================

showResult(rul__constraint__declaration(_,Fat),
	   rul__constraint__declaration(_,Slim)) :-

	length(Fat, Long),
	length(Slim, Short),
	!.
        /*,print('>>> Compression: '),
	print(Short),
	print('/'),
	print(Long),
	print(' in #Proc.Definitions'),
	nl.*/

%------------------------------------------------------------

shrinkRCD(rul__constraint__declaration(OldC,OldP),
	  rul__constraint__declaration(NewC,NewP)) :-

	allTypes(OldP, Types),
	!,
	equivalenceClasses(Types, Classes, OldP),
	!,
	rewriteConstr(Classes, OldC, NewC),
	!,
	rewriteProgrm(Classes, OldP, NewP).

% ==================================================
% Not needed at the moment, but perhaps useful later
% minimise(OldP, NewP) :-
%	allTypes(OldP, Types),!,
%	equivalenceClasses(Types, Classes, OldP),!,
%	rewriteProgrm(Classes, OldP, NewP).
%===================================================

rewriteConstr(Classes, [OC|Old], [NC|New]) :-

	OC =.. [OldName, Var],
	
	identify(OldName, Classes, NewName),
	!,
	NC =.. [NewName, Var],
	
	rewriteConstr(Classes, Old, New).

rewriteConstr(Classes, [Same|Old], [Same|New]) :-

	rewriteConstr(Classes, Old, New).

rewriteConstr(_,[],[]).

%------------------------------------------------------------

identify(OldName, [[NewName|EquiTypes]|_], NewName) :-

	memberchk(OldName, EquiTypes),
	!.

identify(OldName, [_|EquiClasses], NewName) :-

	identify(OldName, EquiClasses, NewName).

identify(_,[],_) :- !, fail.

%------------------------------------------------------------

rewriteProgrm(EqClasses, OldProg, NewProg) :-

	cutEquivalents(EqClasses, OldProg, Result),
	!,
	rewriteCalls(EqClasses, Result, NewProg).

%------------------------------------------------------------

cutEquivalents([[_|EqTypes]|EqClasses], InProg, OutProg) :-

	discardProcs(EqTypes, InProg, Result),
	!,
	cutEquivalents(EqClasses, Result, OutProg).

cutEquivalents([], FIX, FIX).

%------------------------------------------------------------

discardProcs([T|Obsolete], InProg, OutProg) :-

	memberchk(proc(T/1,Def), InProg),
	
	delete(InProg, proc(T/1,Def), Result),
	!,
	discardProcs(Obsolete, Result, OutProg).

discardProcs([_|Obsolete], InProg, OutProg) :-

	/* This case should not occur! (LifeGuard) */
	print('ALARM(1)! Something Strange in Compressor.'),
	nl,
	discardProcs(Obsolete, InProg, OutProg),
	!.

discardProcs([], FIX, FIX).

%------------------------------------------------------------

rewriteCalls([[T,ET|EqTypes]|EqClasses], InProg, OutProg) :-
	
	replaceInProgram(InProg, T, ET, Result),
	!,
	rewriteCalls([[T|EqTypes]|EqClasses], Result, OutProg).

rewriteCalls([[_]|EqClasses], InProg, OutProg) :-
	!,
	rewriteCalls(EqClasses, InProg, OutProg).

rewriteCalls([_|EqClasses], InProg, OutProg) :-

	/* This case should not occur! (LifeGuard) */
	print('ALARM(2)! Something Strange in Compressor.'),
	nl,
	rewriteCalls(EqClasses, InProg, OutProg),
	!.

rewriteCalls([], FIX, FIX).

%------------------------------------------------------------

replaceInProgram([proc(N/1, OldDef)|OldProcs], NewName,
		 OldName, [proc(N/1, NewDef)|NewProcs]) :-

	replaceAllOccurrences(NewName,OldName,_,
			      OldDef, NewDef),
        !,
	replaceInProgram(OldProcs, NewName,
			 OldName, NewProcs).

replaceInProgram([],_,_,[]).

%###############################################################
%############################# END #############################
%###############################################################