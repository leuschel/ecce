:- module(pattern_unify, [pattern_unify/2], []).

:- use_module(lambda_terms,
	[
	    deref/2,
	    make_lambda/3,
	    new_ref/2,
	    get_ref/2,
	    freshvar/2,
	    bind/2
	]).

:- use_module(hnorm,[hnorm/2]).

:- use_module(library(lists),
	[
	    append/3,
	    length/2
	]).

:- use_module(library(hiordlib),[map/3]).


% Transforming a term to represent substitution under abstractions
lift(Term, N, Term2) :-
	get_ref(Term,X),
	lift_(X,Term,N,Term2).
lift_(const(_,_),Term,_,Term) :-
	!.
lift_(var(_,_),Term,_,Term) :-
	!.
lift_(dB(I),_,N,Term2) :-
	I1 is I + N,
	!,
	new_ref(dB(I1),Term2).
lift_(_,Term,N,Term2) :-
	new_ref(susp(Term,0,N,[]),Term2).

% Transforming a list of arguments to represent eta fluffing
lift_args([], N, L) :-
	!,
	(
	    (N = 0) -> L = []
	;
	    new_ref(dB(N),X),
	    N1 is N - 1,
	    lift_args([],N1,L1),
	    L = [X|L1]
	).
lift_args([A|RArgs],N,[H|T]) :-
	lift(A,N,H),
	lift_args(RArgs,N,T).

% Check if a constant appears in a list of terms
unique_const(_,[],true) :-
	!.
unique_const((C1,D1),[H|RArgs],B) :-
	get_ref(H,const(C2,D2)),
	(
	    ((C1,D1) = (C2,D2)) -> B = false
	;
	    unique_const((C1,D1),RArgs,B)
	).
unique_const(C,[_|RArgs],B) :-
	unique_const(C,RArgs,B).

% Check if a bound variable appears in a list of terms
unique_bv(_,[],true).
unique_bv(I,[H|RArgs],B) :-
	get_ref(H,dB(I2)),
	(
	    (I = I2) -> B = false
	;
	    unique_bv(I,RArgs,B)
	).
unique_bv(I,[_|RArgs],B) :-
	unique_bv(I,RArgs,B).

% Check if a list of terms meets the LLambda requirements for the
% arguments of a flexible term.
check_flex_args([],_,true).
check_flex_args([H|RArgs],FTS,B):-
	get_ref(H,const(C,CTS)),
	unique_const((C,CTS),RArgs,B2),
	(
	    ((CTS > FTS), (B2 = true)) -> check_flex_args(RArgs,FTS,B)
	;
	    B = false
	).
check_flex_args([H|RArgs],FTS,B):-
	get_ref(H,dB(I)),
	unique_bv(I,RArgs,B2),
	(
	    (B2 = true) -> check_flex_args(RArgs,FTS,B)
	;
	    B = false
	).
check_flex_args([_|_],_,false).

% Returns a nonzero index if a bound variable appears in a list of
% arguments; the index is the position from the right, representing
% the de Bruijn index of the abstraction capturing the argument. Zero
% signals absence in the argument list. Arguments in the list are
% expected to be (head) normalized and dereferenced
bvindex((_,[],_),0).
bvindex((T,[H|RArgs],N),Pos) :-
	T = dB(I),
	get_ref(H,dB(J)),
	(
	    (I = J) -> Pos = N
	;
	    N1 is N - 1,
	    bvindex((T,RArgs,N1),Pos)
	).
bvindex((T,[_|RArgs],N),Pos) :-
	N1 is N - 1,
	bvindex((T,RArgs,N1),Pos).

% Returns a nonzero index if a constant appears in a list of
% arguments; the index is the position from the right, representing
% the de Bruijn index of the abstraction capturing the argument. Zero
% signals absence in the argument list. Arguments in the list are
% expected to be (head) normalized and dereferenced
cindex((_,[],_),0).
cindex((T,[H|RArgs],N),Pos) :-
	T = const(_,_),
	get_ref(H,HRef),
	HRef = const(_,_),
	(
	    (T = HRef) -> Pos = N
	;
	    N1 is N - 1,
	    cindex((T,RArgs,N1),Pos)
	).
cindex((T,[_|RArgs],N),Pos) :-
	N1 is N - 1,
	cindex((T,RArgs,N1),Pos).

% Given a flexible term (v1 a11 ... a1n) and another term of the form 
% (... (v2 a21 ... a2m) ...) where v1 and v2 are distinct variables,
% ts1 and ts2 are the timestamps associated with v1 and v2, a1 and a2
% represent the sequence a11 ... a1n and a21 ... a2m and lev
% represents the number of abstractions under which v2 appears
% embedded in the second term, return a triple consisting of:
%      (i) a truth value indicating whether a pruning or raising
%          substitution is needed for v2,
%     (ii) a list of terms b1 ... bk such that the term 
%             lam ... lam (... (v2' b1 ... bk) ...) 
%          represents a unifying substitution for v1; these terms 
%          consist of constants from a11 ... a1n over which v2 is
%          possibly raised and inversions of a pruned a21 ... a2m, and
%    (iii) the arguments c1 ... ck of a possible "raising" and pruning
%          substitution for v2 matching the arguments b1 ... bk for
%          v2' in (ii).
% The composition of the arguments lists can be understood
% qualitatively as follows: If (ts1 < ts2) then the initial part of
% b1 ... bk is the indices of constants from a11 ... a1n that do
% not appear in a21 ... a2m and that have a timestamp less than or 
% equal to ts2 (this corresponds to raising v2) and the rest of 
% b1 ... bk are the indices (relative to the list a11 ... a1n) of 
% the constants in a21 ... a2m that appear in a11 ... a1n (these are 
% the arguments that must not be pruned). Correspondingly, the first
% part of the list c1 ... ck are the constants from a11 ... a1n over
% which v2 is "raised" and the second part are the indices relative
% to a21 ... a2m of the constants that are not pruned. If (ts1 >= ts2)
% then each of b1 ... bk is either a constant in a21 ... a2m that
% does not appear in a11 ... a1n and which has a timestamp less than
% ts1 (this corresponds to first raising v1 and then reducing the
% substitution generated for v1) or it is the index, relative to 
% a11 ... a1n, of the terms in a21 ... a2m that are in 
% a11 ... a1n. The list c1 ... ck in this case are simply the indices
% relative to a21 ... a2m of the terms in a21 ... a2m that are
% preserved (i.e. not pruned) in b1 ... bk.
%
% This definition assumes that the terms in a1 and a2 are in
% head-normal and dereferenced form and that a1 satisfies the LLambda 
% requirements. If a2 does not satisfy these requirements, an
% exception will be raised.
raise_and_invert(TS1,TS2,A1,A2,Lev,(PRorRA,TermsList,ArgsList)) :-
	length(A2,N2),
	(
	    (TS1 < TS2) ->
	     length(A1, N1),
	     L1 is N1 + Lev,
	     raise_var(A1, Lev, L1, TS2, (Raised,Args11,Args12)),
	     prune(A2, A1, Lev, N2, (Pruned,Args21,Args22)),
	     (
		 (Raised = true) -> PRorRA = Raised
	     ;
		 PRorRA = Pruned
	     ),
	     append(Args11,Args21,TermsList),
	     append(Args12,Args22,ArgsList)
	;
	    prune_and_raise(A2,A1,N2,Lev,TS1,(PRorRA,TermsList,ArgsList))
	).

% Generates the collection of constants in as that have a time stamp
% < ts2 and the de Bruijn indices, assuming n is the index for the
% abstraction corresponding to the first term in args, for these constants.
% This serves to raise v2 in the case that ts1 < ts2. The boolean
% component of the triple returned is set to true if there is any raising to
% be done. The terms in args are assumed to be constants or de Bruijn indices
% as head normalized and dereferenced arguments satisfying the LLambda
% restriction would be.
raise_var([],_,_,_,(false,[],[])).
raise_var([H|RArgs],Lev,N,TS2,R) :-
	get_ref(H, dB(_)),
	!,
	N1 is N - 1,
	raise_var(RArgs, Lev, N1, TS2, R).
raise_var([H|RArgs],Lev,N,TS2,R) :-
	get_ref(H,const(_,CTS)),
	N1 is N - 1,
	raise_var(RArgs,Lev,N1,TS2,(Raised,Inds,Consts)),
	(
	    (CTS =< TS2) ->
	     N2 is N + Lev,
	     new_ref(dB(N2),Ref),
	     R = (true,[Ref|Inds],[H|Consts])
	;
	    R = (Raised,Inds,Consts)
	).

% It "prunes" those items in args that are not bound by an embedded
% abstraction and that do not appear in a1 and inverts those items that are
% not pruned and that are not bound by an embedded abstraction; n is assumed
% to be the length of args here and hence yields the index of the leftmost
% argument position. This pruning computation is relevant to the case when
% (ts1 < ts2). The terms in args are assumed to be constants or de Bruijn
% indices.
prune([],_,_,0,(false,[],[])).
prune([H|RArgs],A1,Lev,N,R) :-
	get_ref(H,dB(I)),
	N1 is N - 1,
	prune(RArgs,A1,Lev,N1,(Pruned,Inds1,Inds2)),
	(
	    (I > Lev) ->
	     I2 is I - Lev,
	     length(A1,Length),
	     L1 is Length + Lev,
	     bvindex((dB(I2),A1,L1),J),
	     (
		(J = 0) -> R = (true,Inds1,Inds2)
	     ;
		 J1 is J + Lev,
		 new_ref(dB(J1),RR1),
		 new_ref(dB(N),RR2),
		 R = (Pruned,[RR1|Inds1],[RR2|Inds2])
	     )
	;
	    new_ref(dB(N),Ref),
	    R = (Pruned,[H|Inds1],[Ref|Inds2])
	).
prune([H|RArgs],A1,Lev,N,R) :-
	get_ref(H,C),
	C = const(_,_),
	N1 is N - 1,
	prune(RArgs,A1,Lev,N1,(Pruned,Inds1,Inds2)),
	length(A1,Length),
	L1 is Length + Lev,
	cindex((C,A1,L1),J),
	(
	    (J = 0) -> R = (true,Inds1,Inds2)
	;
	    J1 is J + Lev,
	    new_ref(dB(J1),RR1),
	    new_ref(dB(N),RR2),
	    R = (Pruned,[RR1|Inds1],[RR2|Inds2])
	).

% Revelant to the case when ts1 > ts2. In this case, it prunes those
% constants and de Bruijn indices not bound by an embedded abstraction that
% do not appear in a1 and, in the case of constants, that do not have a
% timestamp < ts1. Constants that do have a timestamp greater than or equal
% to ts1 are preserved via a raising of v1. As in prune, n is assumed to be
% the length of the list args. The terms in args are assumed to be constants
% or de Bruijn indices.
prune_and_raise([], _, 0, _, _, (false,[],[])).
prune_and_raise([H|RArgs], A1, N, Lev, TS1, R) :-
	get_ref(H, dB(I)),
	N1 is N - 1,
	prune_and_raise(RArgs, A1, N1, Lev, TS1, (Pruned,Inds1,Inds2)),
	(
	    (I > Lev) ->
	     I2 is I - Lev,
	     length(A1,Length),
	     L1 is Length + Lev,
	     bvindex((dB(I2),A1,L1),J),
	     (
		(J = 0) -> R = (true,Inds1,Inds2)
	     ;
		 J1 is J + Lev,
		 new_ref(dB(J1),RR1),
		 new_ref(dB(N),RR2),
		 R = (Pruned,[RR1|Inds1],[RR2|Inds2])
	     )
	;
	    new_ref(dB(N),RRR),
	    R = (Pruned, [H|Inds1], [RRR|Inds2])
	).
prune_and_raise([H|RArgs], A1, N, Lev, TS1, R) :-
	get_ref(H, RefH),
	RefH = const(_,CTS),
	N1 is N - 1,
	prune_and_raise(RArgs, A1, N1, Lev, TS1, (Pruned,Inds1,Inds2)),
	(
	    (CTS =< TS1) ->
	     new_ref(dB(N),RRR),
	     R = (Pruned,[H|Inds1],[RRR|Inds2])
	;
	    length(A1,Length),
	    L1 is Length + Lev,
	    cindex((RefH,A1,L1),I),
	    (
		(I = 0) -> R = (true, Inds1, Inds2)
	    ;
		 I1 is I + Lev,
		 new_ref(dB(I1),RR1),
		 new_ref(dB(N),RR2),
		 R = (Pruned,[RR1|Inds1],[RR2|Inds2])
	    )
	).

% Generating the arguments of a pruning substitution for the case when trying
% to unify two flexible terms of the form (v t1 ... tn) and lam ... lam (v s1
% ... sm) where there are j abstractions at the head of the second term. The
% first two arguments to prune_same_var are the two lists of arguments, the
% third argument is j (i.e. the number of abstractions at the head) and the
% last argument is n+j. It is assumed that type consistency has been checked
% beforehand, i.e. n+j is indeed equal to m and also that the two argument
% lists are known to be of the form required for LLambda unification. The
% computation essentially does the eta fluffing of the first term on the fly
% (i.e. each ti has to be lifted over j abstractions and and j new arguments
% bound by these abstractions are added to the first term).
prune_same_var([],[],_,_,[]).
prune_same_var([],[X|A2],J,BL,R) :-
	get_ref(X,dB(I)),
	(
	    (I = J) ->
	     new_ref(dB(BL),Ref),
	     J1 is J - 1,
	     BL1 is BL - 1,
	     prune_same_var([],A2,J1,BL1,RR),
	     R = [Ref|RR]
	;
	    J1 is J - 1,
	    BL1 is BL - 1,
	    prune_same_var([],A2,J1,BL1,R)
	).
prune_same_var([],[_|A2],J,BL,R) :-
	J1 is J - 1,
	BL1 is BL - 1,
	prune_same_var([],A2,J1,BL1,R).
prune_same_var([X|A1],[Y|A2],J,BL,R) :-
	get_ref(X,CX),
	get_ref(Y,CY),
	CX = const(_,_),
	CY = const(_,_),
	(
	    (CX = CY) ->
	     new_ref(dB(BL),Ref),
	     BL1 is BL - 1,
	     prune_same_var(A1,A2,J,BL1,RR),
	     R = [Ref|RR]
	;
	     BL1 is BL - 1,
	     prune_same_var(A1,A2,J,BL1,R)
	).
prune_same_var([X|A1],[Y|A2],J,BL,R) :-
	get_ref(X,dB(I1)),
	get_ref(Y,dB(I2)),
	(
	    I1J is I1 + J,
	    (I1J = I2) ->
	     new_ref(dB(BL),Ref),
	     BL1 is BL - 1,
	     prune_same_var(A1,A2,J,BL1,RR),
	     R = [Ref|RR]
	;
	     BL1 is BL - 1,
	     prune_same_var(A1,A2,J,BL1,R)
	).
prune_same_var([_|A1],[_|A2],J,BL,R) :-
	BL1 is BL - 1,
	prune_same_var(A1,A2,J,BL1,R).

% Given a term of the form (app (h1,a1)) where h1 is a variable and
% another term t2, generate an LLambda substitution fpr h1 if this is 
% possible, making whatever pruning and raising substitution that are
% necessary to variables appearing within t2. Exceptions can be
% raised from this code if a non LLambda situation is discovered or
% there is failure in unification or a type mismatch (possible if an
% a priori type checking has not been done) is encountered. The
% second argument is assumed to be in head normal form, the first and
% second are assumed to be dereferenced and the last argument is the
% number of arguments in a1.
%
% The unification computation is split into two parts, one that
% examines the top level structure of t2 and the other that descends
% into its nested subparts. This organization is useful primarily
% because h1, the variable head, of the first term can appear at the
% toplevel in t2 without sacrificing unifiability but not in a nested
% part. 
makesubst(H1,T2,A1,N,Term) :-
	get_ref(H1,var(_,TS1)),
	map(A1,hnorm,A1p),
	check_flex_args(A1p,TS1,B),
	(
	    (B = true) -> toplevel_subst(T2,H1,A1p,TS1,0,N,Term)
	;
	    throw(unification_error(not_llambda))
	).

% processing toplevel structure in generating a substitution.  First descend
% under abstractions. Then if the term is a variable, generate the simple
% substitution. Alternatively, if it is an application with the variable
% being bound as its head, then generate the pruning substitution. In all
% other cases, pass the task on to nested_subst. An optimization is possible
% in the case that the term being examined has no outer abstractions
% (i.e. lev = 0) and its head is a variable with a time stamp greater than
% that of the variable being bound. In this case it may be better to invoke
% raise_and_invert directly with the order of the "terms" reversed.
%
% The incoming term is assumed to be head normalized and dereferenced.
toplevel_subst(Term,H1,A1,TS1,Lev,N,Term2) :-
	get_ref(Term,Ref),
	!,
	toplevel_subst_(Ref,Term,H1,A1,TS1,Lev,N,Term2).
toplevel_subst_(lam(N2,T2),_,H1,A1,TS1,Lev,N,Term2) :-
	deref(T2,T3),
	LevN is Lev + N2,
	!,
	toplevel_subst(T3,H1,A1,TS1,LevN,N,Term2).
toplevel_subst_(var(_,_),Term,H1,_,_,Lev,N,Term2) :-
	!,
	(
	    (H1 == Term) ->
	     (
		 (N = 0, Lev = 0) -> Term2 = H1
	     ;
		 throw(unification_error(types_mistmach))
	     )
	;
	    LevN is Lev + N,
	    make_lambda(LevN, Term, Term2)
	).
toplevel_subst_(app(H2,A2),Term,H1,A1,TS1,Lev,N,Term2) :-
	deref(H2,H2deref),
	get_ref(H2deref,Ref),
	!,
	map(A2,hnorm,A2p),
	toplevel_subst_app(Ref,Term,H2deref,A2p,H1,A1,TS1,Lev,N,Term2).
toplevel_subst_(_,Term,H1,A1,TS1,Lev,N,Term2) :-
	LevN is Lev + N,
	nested_subst(Term,A1,H1,TS1,Lev,N,T3),
	make_lambda(LevN,T3,Term2).

toplevel_subst_app(var(_,TS2),Term,H2,A2,H1,A1,TS1,Lev,N,Term2) :-
	!,
	(
	    (H1 == H2) ->
	     check_flex_args(A2,TS2,B),
	     (
		 (B = true) -> 
		 BindLen is Lev + N,
		 length(A2,LengthA2),
		 (
		     (BindLen = LengthA2) ->
		      freshvar(TS1,H1p),
		      prune_same_var(A1,A2,Lev,BindLen,Args),
		      new_ref(app(H1p,Args),Ref),
		      make_lambda(BindLen,Ref,Term2)
		 ;
		     throw(unification_error(types_mistmach))
		 )
	     ;
		 throw(unification_error(not_llambda))
	     )
	;
	    LevN is Lev + N,
	    nested_subst(Term,A1,H1,TS1,Lev,N,T3),
	    make_lambda(LevN,T3,Term2)
	).
toplevel_subst_app(_,Term,_,_,H1,A1,TS1,Lev,N,Term2) :-
	LevN is Lev + N,
	nested_subst(Term,A1,H1,TS1,Lev,N,T3),
	make_lambda(LevN,T3,Term2).

% Generating a substitution term and performing raising and pruning
% substitutions corresponding to a non top-level (sub)term. In this case the
% variable being bound cannot appear embedded inside the term. This code
% assumes that its term argument is head normalized and
% dereferenced. Exceptions can be raised if unification fails or if LLambda
% conditions are found to be violated.
nested_subst(Term,A1,H1,TS1,Lev,N,Term2) :-
	get_ref(Term,Ref),
	nested_subst_(Ref,Term,A1,H1,TS1,Lev,N,Term2).
nested_subst_(const(C,CTS),Term,A1,_,TS1,Lev,N,Term2) :-
	(
	    (CTS =< TS1) -> Term2 = Term
	;
	    cindex((const(C,CTS),A1,N),J),
	    (
		(J = 0) ->
		 throw(unification_error(occurs_check))
	    ;
		J2 is J + Lev,
		new_ref(dB(J2),Term2)
	    )
	).
nested_subst_(dB(I),Term,A1,_,_,Lev,N,Term2) :-
	(
	    (I =< Lev) -> Term2 = Term
	;
	    I2 is I - Lev,
	    bvindex((dB(I2),A1,N),J),
	    (
		(J = 0) ->
		 throw(unification_error(occurs_check))
	    ;
		J2 is J + Lev,
		new_ref(dB(J2),Term2)
	    )
	).
nested_subst_(var(X,TS2),Term,A1,H1,TS1,Lev,_,Term2) :-
	(
	    get_ref(H1,RefH1),
	    (var(X,TS2) = RefH1) ->
	     throw(unification_error(occurs_check))
	;
	    raise_and_invert(TS1,TS2,A1,[],Lev,(Changed,A1p,A2p)),
	    (
		((Changed = true), (TS1 < TS2)) ->
		 (
		     (TS1 < TS2) -> freshvar(TS1,TT)
		 ;
		     freshvar(TS2,TT)
		 ),
		 bind(Term, app(TT,A2p)),
		 new_ref(app(TT,A1p),Term2)
	    ;
		new_ref(app(Term,A1p),Term2)
	    )
	).
nested_subst_(lam(N,T),_,A1,H1,TS1,Lev,_,Term2) :-
	N1 is Lev + N,
	deref(T,T2),
	nested_subst(T2,A1,H1,TS1,N1,N,TT2),
	new_ref(lam(N,TT2),Term2).
nested_subst_(app(H2,A2),_,A1,H1,TS1,Lev,N,Term2) :-
	deref(H2,H2p),
	nested_subst_app(H2p,A2,A1,H1,TS1,Lev,N,Term2).

nested_subst_app(Term,A2,A1,H1,TS1,Lev,N,Term2) :-
	get_ref(Term,Ref),
	nested_subst_app_(Ref,Term,A2,A1,H1,TS1,Lev,N,Term2).
nested_subst_app_(const(_,_),Term,A2,A1,H1,TS1,Lev,N,Term2) :-
	nested_subst(Term,A1,H1,TS1,Lev,N,X),
	map(A2,
	    (''(XX,Z) :- (hnorm(XX,YY), nested_subst(YY,A1,H1,TS1,Lev,N,Z))),
	    Y),
	new_ref(app(X,Y),Term2).
nested_subst_app_(dB(_),Term,A2,A1,H1,TS1,Lev,N,Term2) :-
	nested_subst(Term,A1,H1,TS1,Lev,N,X),
	map(A2,
            (''(XX,Z) :- (hnorm(XX,YY), nested_subst(YY,A1,H1,TS1,Lev,N,Z))),
	    Y),
	new_ref(app(X,Y),Term2).
nested_subst_app_(var(L2,TS2),Term,A2,A1,H1,TS1,Lev,_,Term2) :-
	get_ref(H1,RefH1),
	(
	    (var(L2,TS2) = RefH1) ->
	     throw(unification_error(occurs_check))
	;
	    map(A2,hnorm,A2map),
	    check_flex_args(A2map,TS2,B),
	    (
		(B = true) ->
		 raise_and_invert(TS1,TS2,A1,A2map,Lev,(Changed,A1p,A2p)),
		 (
		     (Changed = true) ->
		      (
			  (TS1 < TS2) -> freshvar(TS1,TT)
		      ;
			  freshvar(TS2,TT)
		      ),
		      length(A2map, LengthA2),
		      new_ref(app(TT,A2p),TTT),
		      make_lambda(LengthA2,TTT,TermAux),
		      bind(Term,TermAux),
		      new_ref(app(TT,A1p),Term2)
		 ;
		     (TS1 < TS2) ->
		      (
			  freshvar(TS1,Hp),
			  bind(Term,Hp),
			  new_ref(app(Hp,A1),Term2)
		      ;
			  new_ref(app(Term,A1p),Term2)
		      )
		 )
	    ;
		throw(unification_error(not_llambda))
	    )
	).

% Unifying the arguments of two rigid terms with the same head, these
% arguments being given as lists. Exceptions are raised if unification fails
% or if there are unequal numbers of arguments; the latter will not arise if
% type checking has been done.
unify_list([],[]) :-
	!.
unify_list([A1|As1],[A2|As2]) :-
	!,
	hnorm(A1,A1norm),
	hnorm(A2,A2norm),
	unify(A1norm,A2norm),
	unify_list(As1,As2).
unify_list(_,_) :-
	throw(unification_error(types_mistmach)).

% Unifying a constant with a term that is not a variable or an
% application. If it is a lambda, binders need to be equalized and so this
% becomes an application-term unification problem.
unify_const_term(T1,T2) :-
	get_ref(T1,const(A,B)),
	get_ref(T2,const(C,D)),
	!,
	(
	    (const(A,B) = const(C,D)) -> true
	;
	    throw(unification_error(const_clash))
	).
unify_const_term(T1,T2) :-
	get_ref(T1,const(_,_)),
	get_ref(T2,lam(N,TT)),
	!,
	lift_args([],N,A1),
	new_ref(app(T1,A1),TT2),
	unify_app_term(T1,A1,TT2,TT).
unify_const_term(_,_) :-
	throw(unification_error(const_clash)).

% Unifying a bound variable with a term that is not a variable, an
% application or a constant. If it is a lambda, binders need to be equalized
% and this becomes an application-term unification problem.
unify_bv_term(T1,T2) :-
	get_ref(T1,DB1),
	get_ref(T2,DB2),
	DB1 = dB(N1),
	DB2 = dB(N2),
	!,
	(
	    (N1 = N2) -> true
	;
	    throw(unification_error(const_clash))
	).
unify_bv_term(T1,T2) :-
	get_ref(T1,dB(_)),
	get_ref(T2,lam(N,TT)),
	!,
	lift(T1,N,T1p),
	lift_args([],N,A1p),
	new_ref(app(T1p,A1p),TT2),
	unify_app_term(T1p,A1p,TT2,TT).
unify_bv_term(_,_) :-
	throw(unification_error(const_clash)).

% Unifying an application, supplied as the (dereferenced) head and arguments
% list in addition to the term itself, with another dereferenced and head
% normalized term different from a variable.
unify_app_term(Term,A1,T1,Term2) :-
	get_ref(Term,Ref1),
	get_ref(Term2,Ref2),
	unify_app_term_(Ref1,Ref2,Term,A1,T1,Term2).
unify_app_term_(var(_,_),_,Term,A1,_,T2) :-
	length(A1,N),
	makesubst(Term,T2,A1,N,TT),
	bind(Term,TT),
	!.
unify_app_term_(const(X,Y),app(H2,A2),_,A1,T1,_) :-
	C1 = (X,Y),
	!,
	deref(H2,H2p),
	get_ref(H2p,RefH2),
	(
	    (RefH2 = const(Z,T)) ->
	     (
		 (C1 = (Z,T)) -> unify_list(A1,A2)
	     ;
		 throw(unification_error(const_clash))
	     )
	;
	    (
		(RefH2 = dB(_)) -> throw(unification_error(const_clash))
	    ;
		length(A2,M),
		makesubst(H2p,T1,A2,M,TT),
		bind(H2p,TT)
	    )
	).
unify_app_term_(dB(N1),app(H2,A2),_,A1,T1,_) :-
	deref(H2,H2p),
	get_ref(H2p,RefH2),
	!,
	(
	    (RefH2 = const(_,_)) -> throw(unification_error(const_clash))
	;
	    (RefH2 = dB(N2)) ->
	     (
		 (N1 = N2) -> unify_list(A1,A2)
	     ;
		 throw(unification_error(const_clash))
	     )
	;
	    length(A2,M),
	    makesubst(H2p,T1,A2,M,TT),
	    bind(H2p,TT)
	).
unify_app_term_(_,lam(N,TT2),Term,A1,_,_) :-
	!,
	lift(Term,N,H1p),
	lift_args(A1,N,A1p),
	new_ref(app(H1p,A1p),T1p),
	unify_app_term(H1p,A1p,T1p,TT2).
unify_app_term_(_,_,_,_,_,_) :-
	throw(unification_error(const_clash)).

% The main unification procedure. When called with terms as arguments, it
% either succeeds and realizes the unification substitutions as side effects
% or it raises an exception to indicate nonunifiability or to signal a case
% outside of the LLambda subset. When an exception is raised, it is necessary
% to catch this and at least undo bindings for variables made in the attempt
% to unify. This has not been included in the code at present.
%
% This procedure assumes that the two terms it gets are in dereferenced, head
% normal form and that there are no iterated lambdas or applications at the
% top level. Any necessary adjustment of binders through the eta rule is done
% on the fly.
unify(Term1,Term2) :-
	get_ref(Term1,Ref1),
	get_ref(Term2,Ref2),
	unify_(Ref1,Term1,Ref2,Term2).
unify_(var(_,_),Term1,_,Term2) :-
	!,
	makesubst(Term1,Term2,[],0,TT),
	bind(Term1,TT).
unify_(app(H1,A1),Term1,_,Term2) :-
	!,
	deref(H1,H1deref),
	unify_app_term(H1deref,A1,Term1,Term2).
unify_(_,Term1,app(H2,A2),Term2) :-
	!,
	deref(H2,H2deref),
	unify_app_term(H2deref,A2,Term2,Term1).
unify_(const(_,_),Term1,_,Term2) :-
	!,
	unify_const_term(Term1,Term2).
unify_(_,Term1,const(_,_),Term2) :-
	!,
	unify_const_term(Term2,Term1).
unify_(dB(_),Term1,_,Term2) :-
	!,
	unify_bv_term(Term1,Term2).
unify_(_,Term1,dB(_),Term2) :-
	!,
	unify_bv_term(Term2,Term1).
unify_(lam(N1,T1),_,lam(N2,T2),_) :-
	(
	    (N1 > N2) ->
	     N1p is N1 - N2,
	     make_lambda(N1p,T1,TT1),
	     deref(T2,TT2),
	     unify(TT1,TT2)
	;
	     deref(T1,TT1),
	     N2p is N2 - N1,
	     make_lambda(N2p,T2,TT2),
	     unify(TT1,TT2)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The interface to the functionality of this module
pattern_unify(T1, T2) :-
	hnorm(T1, T11),
	hnorm(T2, T22),
	unify(T11, T22).


