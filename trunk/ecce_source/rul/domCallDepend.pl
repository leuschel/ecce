%##############################################################
%#################### MODULE: DOM-CALL-DEPEND #################
%## A new Module, Supporting the Shortening with a Much More ##
%## Efficient Computation of the Domain-Call-Dependency-List ##
%##############################################################
%---(C)-Stefan-Gruner-University-of-Southampton-England-2002---

:- module(domCallDepend, [allCandidates/2]).

:- use_module(library(lists)).
:- use_module(subType,[subType/3]).
%##############################################################


allCandidates([],[]) :- !.

allCandidates(RULprog, ShorteningCandidates) :-

	sameDomains(RULprog, Domains),
	!,
	inCallChains(Domains, RULprog,
		     PredNamePairList),
	!,
        filterSubTypes(RULprog,
                       PredNamePairList,
                       ShorteningCandidates).

%==============================================================

filterSubTypes(_,[],[]) :- !.

filterSubTypes(Program,
	       [(Super,Sub)|PairsIn],
	       [(Super,Sub)|PairsOut]) :-

	subType(Sub, Super, Program),
	!,
	filterSubTypes(Program,
		       PairsIn,
		       PairsOut).

filterSubTypes(Program, [_|PairsIn], PairsOut) :-

	filterSubTypes(Program, PairsIn, PairsOut).

%==============================================================

sameDomains([],[]) :- !.

sameDomains([ProcDefT|Program],
	    [DomGroup|DomainGroups]) :- 

	domainGroup(ProcDefT,
		    Program,
		    EquiDom,
		    RestProg),
	!,
	remove_duplicates(EquiDom,
			  DomGroup),
	!,
	sameDomains(RestProg,
		    DomainGroups).

%--------------------------------------------------------------

domainGroup(_,[],[],[]) :- !.
	
domainGroup( proc(T/1,DefT),
	    [proc(P/1,DefP)|Program],
	    [T,P|DomGroup], RestProg) :-

	T \== P,
	
	domain(proc(T/1,DefT), DomainT),
	domain(proc(P/1,DefP), DomainP),

	permutation(DomainT, DomainP),
	!,
	/* Case: Same Domain P,T */
	
	domainGroup(proc(T/1,DefT),
		    Program,
		    DomGroup,
		    RestProg),
	!.

domainGroup( ProcDefT,
	    [ProcDefP|Program],
	     DomGroup,
	    [ProcDefP|RestProg]) :-

	/* Case: Different Domains */
	
	domainGroup(ProcDefT,
		    Program,
		    DomGroup,
		    RestProg).

%--------------------------------------------------------------

domain(proc(Name/1,Definition), Domain) :-

	analyseHeads(Name,
		     Definition,
		     [], Domain).

% =============================================================
% Error eliminated [08.02.2002]: Arities are now taken into ac-
% count! For example f/3 is not the same as f/2, which had been
% previously ignored! [analyseHeads/4].  ----------------------

analyseHeads(_, [], AccuOutput, AccuOutput) :- !.           

analyseHeads(any, [(_:-true)], _, []) :-

        /* "any" case: No Domain */
        !.

analyseHeads(P, [(Pred:-true)|Definitions],               
	     Accumulator, Domain) :-                   
                                                       
        /* case: p(c) where c is constant */
	
	Pred =.. [P, ConstSymbol],
	
	functor(ConstSymbol, ConstSymbol, 0),          
	!,                                             
	analyseHeads(P, Definitions,                   
		     [ConstSymbol|Accumulator], Domain).

analyseHeads(P, [(Pred:-_)|Definitions], Accumulator, Domain) :-

        /* case: p(f(X1...Xn)) where f is a function */
	
	Pred =.. [P, Functor],
	
	Functor =.. [FunctSymbol|Arguments],                    
	
	length(Arguments, Arity), /* New: 08.02.2002 */
	
	Dom =.. [FunctSymbol, Arity],
	
	analyseHeads(P, Definitions,                   
		     [Dom|Accumulator], Domain).

%==============================================================

inCallChains([],_,[]) :- !.

inCallChains([Domain|MoreDomains], RULprog, OutPairs) :-

	searchCalls(Domain, RULprog, Pairs),
	!,
	inCallChains(MoreDomains, RULprog, MorePairs),
	!,
	append(Pairs, MorePairs, OutPairs).

%--------------------------------------------------------------

searchCalls([],_,[]) :- !.

searchCalls([T|Types], RULprog, AllPairs) :-

	checkCalls(T, Types,
		   RULprog,
		   PairsT),
	!,
	searchCalls(Types,
		    RULprog,
		    MorePairs),
	!,
	append(PairsT, MorePairs, AllPairs).

%--------------------------------------------------------------

checkCalls(_,[],_,[]) :- !.

checkCalls(T, [P|Preds], RULprog, Pairs) :-

	whoCallsWhom(T, P, RULprog, TP),
	!,
	whoCallsWhom(P, T, RULprog, PT),
	!,
	append(TP, PT, ResultT), /* maybe empty */
	!,
	checkCalls(T, Preds, RULprog, MoreResults),
	!,
	append(ResultT, MoreResults, Pairs).

%--------------------------------------------------------------

whoCallsWhom(S, T, RULprog, [(S,T)]) :-

	/* true if there is a call chain
	   from a head S to a sub goal T 
	   in RULprog. */

	globalDepends(S, T, RULprog),
	!.

whoCallsWhom(_,_,_,[]). /* else case */

%================================================================

globalDepends(P, Q, RULprogram) :-                     
                                                       
	callChain(P, Q, _, RULprogram, [P]).           

% CALLCHAIN: ===================================================
% callChain: See also Definition 3.5, page 601 in J.P. Gallagher
% and D.A.deWaal, "Fast and Precise Regular Approximations of Lo-
% gic Programs" Proc 11th Internat Conf on Logic Programming, MIT
% Press 1994. callChain expresses the transitive closure of the
% "calls" relation (see below). Note: For two predicates P and Q
% there may be more than one unique call chain. Call chains can  
% be of arbitrary length when procedures are (mutually) recursive.
% For this reason we break the rekursion as soon as an element re-
% occurs in the chain. ===========================================
                                                       
callChain(P, Q, [P,Q], RULprogram, _) :- calls(P, Q, RULprogram).
                                        
callChain(P, Q, [P,R|CallChain], RULprogram, VisitedPredNames) :-
                                                       
	calls(P, R, RULprogram),
	
	non_member(R, VisitedPredNames),               
                                                       
	callChain(R, Q, [R|CallChain],
		  RULprogram, [R|VisitedPredNames]).	       

% CALLS: ==========================================================
% calls: See Definition 3.5, page 601 in J.P.Gallagher & D.A.deWaal,
% "Fast and Precise Regular Approximations of Logic Programs", Proc
% 11th Internat Conf on	Logic Programming, MIT Press 1994. We take
% two names of predicates and a program representation and check if
% the predicate are in the call relation wrt. the program represen-
% tation. All parameters are input (+,+,+), otherwise failure. The
% self-explaining auxilliary predicates to "calls" are given below.
% =================================================================
                                  
calls(P, Q, [proc(P/1, DefinitionP)|_]) :-             

	P \== Q, /* we're not interested in P == Q */
	
	inClauseHead(P, DefinitionP),
	
	inClauseBody(Q, DefinitionP).
                                       
calls(P, Q, [_|RULprogram]) :-

        P \== Q, /* we aren't interested in P == Q */
	
	calls(P, Q, RULprogram).
                                            
inClauseHead(P, [(Predicate:-_)]) :- Predicate =.. [P|_].
                                                       
inClauseHead(P, [(Predicate:-_)|Clauses]) :-           
                                                       
        Predicate =.. [P|_],
        
	inClauseHead(P, Clauses).

inClauseHead(P, [(Predicate:-_)|_]) :-                 
                                                       
        Predicate =.. [OtherName|_],
	
	P \== OtherName,
	
	print('* SYNTAX ERROR IN PROGRAM REPRESENTATION *'),
	nl,
	
	fail.

inClauseBody(Q, [(_:-Predicates)|_]) :- checkBody(Q, Predicates). 
                                  
inClauseBody(Q, [_|Clauses]) :- inClauseBody(Q, Clauses).

checkBody(Q, (FirstGoal, _)) :-                        
                                                       
	FirstGoal =.. [Q|_],
	
	Q \== true.                                    

checkBody(Q, (_, NextGoals)) :- checkBody(Q, NextGoals).
                                                       
checkBody(Q, OnlyOnePred) :-                           
                                                       
	OnlyOnePred \=  (_,_),
	
	OnlyOnePred =.. [Q|_],
	
	Q \== true.                                    

%---------------------------------------------------------------
%============================= END =============================
%---------------------------------------------------------------
