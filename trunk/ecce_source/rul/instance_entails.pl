%###################################################################
%##### MODULE INSTANCE_ENTAILS: more refined version of entails ####
%.If two goals are entailed, unfolded versions of them are returned.
%######### (written by Michael Leuschel, October 2001) #############
%###################################################################

:- module(instance_entails, [instance_of_entails/4]).

:- use_module(library(lists)).
:- use_module(library(terms)).

:- use_module(self_check_rul).

:- use_module(analyticFold,[l_goalRULification/4]).

:- use_module(unfold2,[l_goalUnfolding/2]).

:- use_module(ecceRUL,[entails/4]).

:- use_module(prePostCon).

%###################################################################

:- initialization(assert_pre(instance_entails:instance_of_entails(_G1,_G2, R1, R2),
	      (prePostCon:isGoals(_G1),
	       prePostCon:isGoals(_G2),
	       prePostCon:isRCD(R1),
	       prePostCon:isRCD(R2)))).

:- initialization(assert_post(instance_entails:instance_of_entails(_G1,_G2,_R1,_R2),
	       true)).

:- initialization(assert_must_succeed(instance_entails:instance_of_entails(
   [p(X)], rul__constraint__declaration([],[]),
   [p(Z)], rul__constraint__declaration([],[])))).

:- initialization(assert_must_succeed(instance_entails:instance_of_entails(
   [p(A,b)],
   rul__constraint__declaration([t30(A)],
				[proc(t30/1,
				      [(t30([]):-true),
				       (t30([B|C]):-any(B),t30(C))])]),
   [p(Z,b)],						    
   rul__constraint__declaration([t30(Z)],
				[proc(t30/1,
				      [(t30([]):-true),
				       (t30([B|C]):-any(B),t30(C))]),
				 proc(any/1,
				      [(any(D):-true)])])))).

:- initialization(assert_must_succeed(instance_entails:instance_of_entails(
   [p([a|A],b)],
   rul__constraint__declaration([t30(A)],[proc(t30/1,
					       [(t30([]):-true),
						(t30([B|C]):-any(B),t30(C))]),
                                          proc(any/1,
					       [(any(D):-true)])]),
   [p(Z,V)], 
   rul__constraint__declaration([t30(Z)],[proc(t30/1,
					       [(t30([]):-true),
						(t30([B|C]):-any(B),t30(C))]),
                                         proc(any/1,
					      [(any(D):-true)])])))).

:- initialization(assert_must_succeed(instance_entails:instance_of_entails(
   [p(a,b)], rul__constraint__declaration([],[]),
   [p(Z,b)], rul__constraint__declaration([],[])))).

:- initialization(assert_must_fail(instance_entails:instance_of_entails(
   [p(a,b)], rul__constraint__declaration([],[]),
   [p(Z,Z)], rul__constraint__declaration([],[])))).

instance_of_entails(Goal1,Rul1, Goal2,Rul2) :-
	
   mnf(unfold2:l_goalUnfolding(Goal1,Rul1)),
   
   %user:debug_print(l_goalUnfolding(Goal1,Rul1)),
   %user:debug_nl,
   
   mnf(unfold2:l_goalUnfolding(Goal2,Rul2)),
   
   %user:debug_print(l_goalUnfolding(Goal2,Rul2)),
   %user:debug_nl,
   
   user:instance_of(Goal1,Goal2),
   
   print(instance),
   
   pp_mnf(analyticFold:l_goalRULification(Goal1,RG1,Rul1,RR1)),
   
   %user:debug_print(l_goalRULification(Goal1,RG1,Rul1,RR1)),
   %user:debug_nl,
   
   pp_mnf(analyticFold:l_goalRULification(Goal2,RG2,Rul2,RR2)),
   
   %user:debug_print(l_goalRULification(Goal2,RG2,Rul2,RR2)),
   %user:debug_nl,
   
   ecceRUL:entails(RG1,RR1,RG2,RR2),!,
   
   print(entails),nl.

%###################################################################
%###################################################################
