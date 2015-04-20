% ######################################################################
% Rough Regular Type Approximation Bottom Up Specialisation for one Atom
% ######################################################################

:- module(scanBUfile, [scanBUprog/2, scanBUprog/3]).

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module('~/CVS/ecce/ecce_source/rul/compressor',[compress/2]).
:- use_module('~/CVS/ecce/ecce_source/rul/proSiNo',
	      [simplify/2, normalisation/2]).

% #####################################################################
% Reads the Bottom-Up-Result from a file and produces an according RCD.
% According to the output structure produced by john's tool we discard
% all the code from the top down to the identifier "my_query_atom_ans".
% The code following that identifier is relevant and will therefore be
% translated in our own syntactical representation. We assume that in
% the to-be-scanned file all clause head variables appear in the same
% order as the according clause tail variables, for example p(X,Y,Z):-
% r(X),s(Y),t(Z) but not p(X,Y,Z):-s(Y),t(Z),r(X). A scan error means
% that John's Tool couldn't construct a valid bottomUp analysis result
% which is equivalent to the "impossible type". Because we cannot have
% the "impossible type" explicitly in our framework, we return a silly
% RUL constraint declaration which, though syntactically correct, will
% never allow a successful application of simplyfy/2. ################


%-------- PUBLIC -------
scanBUprog(QueryPredAtom, 
	   InputFileName,
	   RULconstraint) :- /* The success case */
	nl,
	print('opening: '),
	seeing(Dummy),
	
	see(InputFileName),
	print(InputFileName),
	clauseList(BUprog),
	seen,
	
	see(Dummy),
	print(' done'),
	nl,
	!,
	construct(QueryPredAtom,
		  BUprog,
		  RULconstraint),
	nl,
	print('>> Rul Constraint Declaration has been constructed.'),
	nl,
	!.

scanBUprog(_,_,
           rul__constraint__declaration([silly(doof)],
					[proc(silly/1,
					      [(silly(false):-true)])])).
%-------- PUBLIC -------
scanBUprog(QueryPredAtom,
	   RULconstraint) :-
	
	scanBUprog(QueryPredAtom,
		   '~/CVS/ecce/ecce_source/bottomUp/tmp.pl', /* Default */
		   RULconstraint).

%=======================================================================

clauseList([Clause|List]) :-

	read(Clause),
	Clause \== end_of_file,
	clauseList(List).

clauseList([]).

%=======================================================================

construct(QueryPredAtom, BUprog, OutputRCD) :-

	discardUntilRelevant(QueryPredAtom, BUprog, State1),
	!,
	buildRCD(QueryPredAtom, State1, State2),

	removeDuplicates(State2, State3),

	simplify(State3, State4),

	normalisation(State4, State5),

	compress(State5, OutputRCD).
	
construct(_,_,rul__constraint__declaration([s(a)],[proc(s/1,[(s(b):-true)])])).

%=======================================================================

buildRCD(ReferencePred, ClauseList,
	 rul__constraint__declaration(Constraints, RULprogram)) :-
   
	cutReference(ReferencePred, ClauseList,
		     ReferenceClause, RestList),
	
	makeConstraints(ReferenceClause,
			PreConstraints),

	adoptVariables(ReferencePred, /* We assume ordered Variables! */
		       PreConstraints,
		       Constraints),
	
	makeProgram(RestList, RULprogram).

%=======================================================================

adoptVariables(Atom, OldConstr, NewConstr) :-

	Atom =.. [_|Variables],
	!,
	replaceVariables(Variables,
			 OldConstr,
			 NewConstr).

%-----------------------------------------------------------------------

replaceVariables([],[],[]).

replaceVariables([V|Vars], [OldC|OldConstr], [NewC|NewConstr]) :-

	/* We assume ordered Variables, */
	/* otherwise results are wrong! */

	OldC =.. [Name,_],
	NewC =.. [Name,V],
	!,
	replaceVariables(Vars, OldConstr, NewConstr).

%=======================================================================

makeProgram(Clauses, RULprogram) :-

	identifyDefinitions(Clauses, Definitions),
	
	addConcreteSyntax(Definitions, RULprogram).

%=======================================================================

identifyDefinitions([],[]).

identifyDefinitions([OnlyOne], [[OnlyOne]]).

identifyDefinitions(Clauses, Definitions) :-

	insertSeparators(Clauses, Result),
	
	buildGroups(Result, Definitions).
	
%=======================================================================
	
buildGroups(SeparatedClauses, [Def|Definitions]) :-

	stopScan(SeparatedClauses, Def, RestList),

	buildGroups(RestList, Definitions).

buildGroups([],[]).

%-----------------------------------------------------------------------

stopScan(InputList, Definition, Postfix) :-

	append(Prefix, Postfix, InputList),

	append(Definition, [stop], Prefix),

	non_member(stop, Definition).

%=======================================================================

insertSeparators([(P1:-G1),(P2:-G2)|Clauses],
		 [(P1:-G1),stop,(P2:-G2)|SepClauses]) :-

        P1 =.. [ThisName,_],
	P2 =.. [ThatName,_],
	
	ThisName \== ThatName, /* Begin of New Def */
	!,
	insertSeparators([(P2:-G2)|Clauses],
			 [(P2:-G2)|SepClauses]).

insertSeparators([C1,C2|Clauses],
		 [C1,C2|SepClauses]) :-

	insertSeparators([C2|Clauses],
			 [C2|SepClauses]).

insertSeparators([C],[C,stop]).

%=======================================================================

addConcreteSyntax([[(Pred:-Tail)|PClauses]|Definitions],
		  [proc(P/1, [(Pred:-Tail)|PClauses])|RULprog]) :-

        Pred =.. [P,_], /* Unary RUL Heads */
	
	addConcreteSyntax(Definitions, RULprog).

addConcreteSyntax([],[]).

%=======================================================================

makeConstraints((_:-Tail), Constraints) :-

        commaToList(Tail, Constraints).

%-----------------------------------------------------------------------

commaToList((P,Goals), [P|Preds]) :-

	commaToList(Goals, Preds).

commaToList(Predicate, [Predicate]) :-

	Predicate \= (_,_).

%=======================================================================

cutReference(Atom, ClauseList, RefClause, RestList) :-

	Atom =.. [Name|_],
	
	name(Name, NameCode),
	
	name('__ans',AnsCode),
	
	append(NameCode, AnsCode, SearchCode),
	
	name(Pattern, SearchCode),
	
	searchPattern(Pattern, ClauseList, RefClause),

	RefClause \== notFound,
	!,
	delete(ClauseList, RefClause, RestList).

%-----------------------------------------------------------------------

searchPattern(Name, [(Atom:-Tail)|_], (Atom:-Tail)) :-

        Atom =.. [Name|_],
	!.

searchPattern(Name, [_|Clauses], Result) :-

	searchPattern(Name, Clauses, Result),
	!.

searchPattern(_, [], notFound). /* This case should not occur! */

%=======================================================================

discardUntilRelevant(QueryAtom,Input,Output) :-
       QueryAtom =.. [QueryName|_],
       name(QueryName, QueryCode),
       name('__ans', AnsCode),
       append(QueryCode, AnsCode, IdCode),
       name(Identifier, IdCode),
       discardUntilRelevant2(Identifier,Input,Output).
       
discardUntilRelevant2(Identifier,
		     [(Relevant:-Tail)|Program],
		     [(Relevant:-Tail)|Program]) :-
       Relevant =.. [Identifier|_],!.

discardUntilRelevant2(Identifier, [_|Input], Output) :-

	discardUntilRelevant2(Identifier, Input, Output),
	!.

discardUntilRelevant2(_,[],_) :-
	nl,print('SCAN ERROR: '),print('__ans'),
	print(' NOT FOUND! Assuming call fails.'),
	nl,!,fail.



%=======================================================================
% [SG:19/04/2002] The following procedure became necessary when we detec-
% ted that John's Tool produces non-RUL output. We remove the duplicates
% (variants modulo variable names), which cause the violation of the RUL
% conditions. anyType definition must also be added because John doesn't! 

removeDuplicates(rul__constraint__declaration(OldConstr, OldProg),
		 rul__constraint__declaration(NewConstr, NewProg)) :-

	remove_duplicates(OldConstr, NewConstr),

	remInternalDup([proc(any/1,[(any(_):-true)])|OldProg], NewProg).

%-----------------------------------------------------------------------

remInternalDup([],[]).

remInternalDup([proc(N/1, DefIn)|Input], [proc(N/1, DefOut)|Output]) :-

	removeVariants(DefIn, DefOut),

	remInternalDup(Input, Output).

%-----------------------------------------------------------------------

removeVariants([],[]).

removeVariants([Elem|List], Output) :-

	findVariant(Elem, List),
	!,
	removeVariants(List,Output).

removeVariants([Elem|List], [Elem|Output]) :-

	removeVariants(List, Output).

%-----------------------------------------------------------------------

findVariant(ElemX, [ElemY|_]) :-

	copy_term(ElemX, ElemZ),
	copy_term(ElemY, ElemZ),
	!.

findVariant(Elem, [_|List]) :-

	findVariant(Elem, List).

%=======================================================================

% --- PUBLIC (though not needed at the moment) ---
/*
quickSort([],[]).
quickSort([Element|Input], Output) :-
	partition(Element, Input, Part1, Part2),
	quickSort(Part1, Result1),
	quickSort(Part2, Result2),
	append(Result1, [Element|Result2], Output).
	
partition(_,[],[],[]).
partition(E, [A|Xs], [A|Ys], Zs) :-
	E @< A, % lex-order
	!,
	partition(E, Xs, Ys, Zs).
partition(E, [A|Xs], Ys, [A|Zs]) :-
	partition(E, Xs, Ys, Zs).*/

%=======================================================================
%################################## END ################################
%=======================================================================


%------test--------

t9(rul__constraint__declaration([t164(_40757)],
   [proc(t88/1,[(t88(stop):-true)]),
    proc(t47/1,[(t47(b):-true)]),
    proc(t27/1,[(t27(v2):-true)]),
    proc(t159/1,
	 [(t159(prefix(_40685,_40686)):-t47(_40685),t88(_40686)),
	  (t159(interleave(_40668,_40669)):-t131(_40668),t159(_40669)),
	  (t159(stop):-true)]),
    proc(t131/1,
	 [(t131(interleave(_40636,_40637)):-t131(_40636),t159(_40637)), %%
	  (t131(interleave(_40619,_40620)):-t131(_40619),t159(_40620)), %%
	  (t131(agent(_40603)):-t27(_40603)),
	  (t131(stop):-true)]),
    proc(t164/1,
	 [(t164(interleave(_40576,_40577)):-t131(_40576),t159(_40577)),
	  (t164(stop):-true)])])).

t12(rul__constraint__declaration([t164(_40757)],
   [proc(t131/1,
	 [(t131(interleave(_40636,_40637)):-t131(_40636),t159(_40637)), %%
	  (t131(interleave(_40619,_40620)):-t131(_40619),t159(_40620)), %%
	  (t131(agent(_40603)):-t27(_40603)),
	  (t131(stop):-true)]),
    proc(t164/1,
	 [(t164(interleave(_40576,_40577)):-t131(_40576),t159(_40577)),
	  (t164(stop):-true)])])).


rdTest(rul__constraint__declaration([t29(_5064),t84(_5062),t85(_5060)],
   [proc(t85/1,[(t85(interleave(_36523,_36521)):-t85(_36523),t85(_36521)),
		(t85(interleave(_36508,_36506)):-t85(_36508),t85(_36506)),
		(t85(interleave(_36493,_36491)):-t85(_36493),t85(_36491)),
		(t85(interleave(_36478,_36476)):-t85(_36478),t85(_36476)),
		(t85(agent(_36464)):-t16(_36464)),
		(t85(agent(_36455)):-t16(_36455)),
		(t85(agent(_36446)):-t16(_36446)),
		(t85(agent(_36437)):-t16(_36437))]),
    proc(t84/1,[(t84(interleave(_36408,_36406)):-t84(_36408),t84(_36406)),
		(t84(interleave(_36393,_36391)):-t84(_36393),t84(_36391)),
		(t84(interleave(_36378,_36376)):-t84(_36378),t84(_36376)),
		(t84(interleave(_36363,_36361)):-t84(_36363),t84(_36361)),
		(t84(agent(_36349)):-t14(_36349)),
		(t84(agent(_36340)):-t14(_36340)),
		(t84(agent(_36331)):-t14(_36331)),
		(t84(agent(_36322)):-t14(_36322))]),
    proc(t82/1,[(t82(interleave(_36293,_36291)):-t84(_36293),t85(_36291)),
		(t82(interleave(_36278,_36276)):-t84(_36278),t85(_36276))]),
    proc(t71/1,[(t71(prefix(_36256,_36254)):-t29(_36256),t30(_36254)),
		(t71(prefix(_36241,_36239)):-t29(_36241),t30(_36239)),
		(t71(interleave(_36226,_36224)):-t13(_36226),t15(_36224)),
		(t71(interleave(_36211,_36209)):-t13(_36211),t15(_36209)),
		(t71(agent(_36197)):-t16(_36197)),
		(t71(agent(_36188)):-t16(_36188))]),
    proc(t406/1,[(t406(prefix(_36163,_36161)):-any(_36163),any(_36161)),
		 (t406(prefix(_36148,_36146)):-any(_36148),any(_36146)),
		 (t406(par(_36133,_36131)):-t406(_36133),t406(_36131)),
		 (t406(par(_36118,_36116)):-t406(_36118),t406(_36116)),
		 (t406(interleave(_36103,_36101)):-any(_36103),any(_36101)),
		 (t406(interleave(_36088,_36086)):-any(_36088),any(_36086)),
		 (t406(choice(_36073,_36071)):-any(_36073),any(_36071)),
		 (t406(choice(_36058,_36056)):-any(_36058),any(_36056)),
		 (t406(agent(_36044)):-t273(_36044)),
		 (t406(agent(_36035)):-t273(_36035))]),
    proc(t37/1,[(t37(prefix(_36002,_36000)):-t29(_36002),t30(_36000)),
		(t37(interleave(_35987,_35985)):-t13(_35987),t15(_35985)),
		(t37(agent(_35973)):-t16(_35973))]),
    proc(t34/1,[(t34(z2):-true)]),
    proc(t33/1,[(t33(agent(_35955)):-t34(_35955)),
		(t33(agent(_35946)):-t34(_35946)),
		(t33(agent(_35937)):-t34(_35937)),
		(t33(agent(_35928)):-t34(_35928)),
		(t33(agent(_35919)):-t34(_35919))]),
    proc(t32/1,[(t32(z2):-true)]),
    proc(t31/1,[(t31(agent(_35897)):-t32(_35897)),
		(t31(agent(_35888)):-t32(_35888)),
		(t31(agent(_35879)):-t32(_35879)),
		(t31(agent(_35870)):-t32(_35870)),
		(t31(agent(_35861)):-t32(_35861))]),
    proc(t30/1,[(t30(interleave(_35838,_35836)):-t31(_35838),t33(_35836)),
		(t30(interleave(_35823,_35821)):-t31(_35823),t33(_35821)),
		(t30(interleave(_35808,_35806)):-t31(_35808),t33(_35806)),
		(t30(interleave(_35793,_35791)):-t31(_35793),t33(_35791)),
		(t30(interleave(_35778,_35776)):-t31(_35778),t33(_35776))]),
    proc(t29/1,[(t29(a):-true)]),
    proc(t282/1,[(t282(z2):-true),(t282(v2):-true)]),
    proc(t280/1,[(t280(prefix(_35750,_35748)):-t244(_35750),t245(_35748)),
		 (t280(agent(_35736)):-t266(_35736))]),
    proc(t28/1,[(t28(prefix(_35719,_35717)):-t29(_35719),t30(_35717)),
		(t28(prefix(_35704,_35702)):-t29(_35704),t30(_35702))]),
    proc(t279/1,[(t279(agent(_35683)):-t282(_35683))]),
    proc(t277/1,[(t277(interleave(_35668,_35666)):-t279(_35668),t280(_35666))]),
    proc(t274/1,[(t274(prefix(_35648,_35646)):-t239(_35648),t277(_35646)),
		 (t274(choice(_35633,_35631)):-t249(_35633),t252(_35631))]),
    proc(t273/1,[(t273(z2):-true),
		 (t273(w2):-true),
		 (t273(v2):-true)]),
    proc(t27/1,[(t27(z2):-true)]),
    proc(t266/1,[(t266(z2):-true)]),
    proc(t257/1,[(t257(w2):-true)]),
    proc(t256/1,[(t256(agent(_35612)):-t257(_35612))]),
    proc(t255/1,[(t255(b):-true)]),
    proc(t254/1,[(t254(prefix(_35597,_35595)):-t255(_35597),t256(_35595))]),
    proc(t253/1,[(t253(a):-true)]),
    proc(t252/1,[(t252(prefix(_35577,_35575)):-t253(_35577),t254(_35575))]),
    proc(t251/1,[(t251(stop):-true)]),
    proc(t250/1,[(t250(b):-true)]),
    proc(t249/1,[(t249(prefix(_35557,_35555)):-t250(_35557),t251(_35555))]),
    proc(t245/1,[(t245(stop):-true)]),
    proc(t244/1,[(t244(b):-true)]),
    proc(t239/1,[(t239(a):-true)]),
    proc(t16/1,[(t16(z2):-true)]),
    proc(t15/1,[(t15(agent(_35538)):-t16(_35538)),
		(t15(agent(_35529)):-t16(_35529)),
		(t15(agent(_35520)):-t16(_35520))]),
    proc(t14/1,[(t14(z2):-true)]),
    proc(t13/1,[(t13(agent(_35502)):-t14(_35502)),
		(t13(agent(_35493)):-t14(_35493)),
		(t13(agent(_35484)):-t14(_35484))])])).

