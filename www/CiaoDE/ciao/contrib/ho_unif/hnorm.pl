:- module(hnorm, [hnorm/2], [functions]).

:- use_module(lambda_terms,
	[
	    deref/2,
	    add_dummies/4,
	    make_lambda/3,
	    new_ref/2,
	    get_ref/2,
	    bind/2
	]).

:- use_module(library(lists),
	[
	    nth/3,
	    append/3,
	    length/2
	]).

:- use_module(library(hiordlib),[map/3]).


make_explicit(Term, 0, 0, [], Term) :-
	!.
make_explicit(lam(N,T), OL, NL, E, Term2) :-
	add_dummies(E, N, NL, E2),
	OL1 is OL + N,
	NL1 is NL + N,
	new_ref(susp(T, OL1, NL1, E2), Ref),
	new_ref(lam(N, Ref), Term2).

hn_lazy(Term, OL, NL, E, WHNF, E2) :-
	get_ref(Term, Ref),
	hn_lazy_(Ref, Term, OL, NL, E, WHNF, E2).
hn_lazy_(const(_,_), Term, _, _, _, _, (Term,0,0,[])).
hn_lazy_(var(_,_), Term, _, _, _, _, (Term,0,0,[])).
hn_lazy_(dB(_), Term, 0, 0, [], _, (Term,0,0,[])) :-
	!.
hn_lazy_(dB(I), _, OL, NL, E, WHNF, E2) :-
	(
	    (I > OL) ->
	     I2 is I - OL + NL,
	     new_ref(dB(I2),Term),
	     E2 = (Term, 0, 0, [])
	;
	    nth(I,E,Element),
	    hn_lazy_dB(Element, NL, WHNF, E2)
	).
hn_lazy_(lam(_,_), Term, OL, NL, E, true, (Term,OL,NL,E)) :-
	!.
hn_lazy_(lam(N,T), _, OL, NL, E, false, (T4,0,0,[])) :-
	deref(T,T2),
	(
	    (OL=0, NL=0) ->
	     hn_lazy(T2,0,0,[],false,(T3,_,_,_))
	;
	    OL2 is OL + N,
	    NL2 is NL + N,
	    add_dummies(E,N,NL,E2),
	    hn_lazy(T2,OL2,NL2,E2,false,(T3,_,_,_))
	),
	hn_lazy_lam(T3,N,T4).
hn_lazy_(app(T1,Args), Term, OL, NL, E, WHNF, (Tp,OLp,NLp,Ep)) :-
	deref(T1,T2),
	hn_lazy(T2,OL,NL,E,true,(F,FOL,FNL,FE)),
	length(Args,Length),
	contract_and_norm(F,Args,Term,OL,FOL,NL,FNL,FE,E,Length,
                          false,WHNF,(Tp,OLp,NLp,Ep)),
	(
	    (OL=0, NL=0) ->
	     (
		 (OLp=0, NLp=0) -> bind(Term,Tp)
	     ;
		 true
	     )
	;
	    true
	).
hn_lazy_(susp(T,OL,NL,E), Term, OL2, NL2, E2, WHNF, EE) :-
	hn_lazy(T,OL,NL,E,WHNF,(EE1,EE2,EE3,EE4)),
	make_explicit(EE1,EE2,EE3,EE4,TT),
	bind(Term,TT),
	(
	    (OL2=0, NL2=0) -> EE = (EE1,EE2,EE3,EE4)
	;
	    hn_lazy(Term,OL2,NL2,E2,WHNF,EE)
	).


hn_lazy_dB(dum(L), NL, _, (Term,0,0,[])) :-
	NL1 is NL - L,
	new_ref(dB(NL1),Term).
hn_lazy_dB(bndg(Term,L), NL, WHNF, E) :-
	deref(Term,Term2),
	get_ref(Term2, Term3),
	hn_lazy_dB_(Term3,Term2,L,NL,WHNF,E).
hn_lazy_dB_(susp(T2,OL2,NL2,E2),_,L,NL,WHNF,E) :-
	NLp is NL2 + NL - L,
	hn_lazy(T2,OL2,NLp,E2,WHNF,E),
	E = (EE1,EE2,EE3,EE4),
	make_explicit(EE1,EE2,EE3,EE4,EE),
	(
	    (NL = L) ->
	     bind(susp(T2,OL2,NL2,E2),EE)
	;
	    true
	).
hn_lazy_dB_(_,Term,L,NL,WHNF,E) :-
	NLp is NL - L,
	hn_lazy(Term,0,NLp,[],WHNF,E).

hn_lazy_lam(Term, N, Term2) :-
	get_ref(Term, X),
	(
	    (X = lam(N2,T)) ->
	     NN is N + N2,
	     new_ref(lam(NN,T), Term2)
	;
	    new_ref(lam(N,Term), Term2)
	).

contract_and_norm(Term,Args,OrigTerm,OL,FOL,NL,FNL,FE,EE,NArgs,Changed,WHNF,E) :-
	get_ref(Term,RefTerm),
	contract_and_norm_(RefTerm,Term,OrigTerm,Args,OL,FOL,NL,FNL,FE,EE,NArgs,
	                   Changed,WHNF,E).
contract_and_norm_(lam(N,T),_,OrigTerm,Args,OL,FOL,NL,FNL,FE,EE,NArgs,_,
                   WHNF,E) :-
	!,
	extend_env(N,Args,OL,NL,FNL,FE,(Rargs,NEnv)),
	(
	    (N >= NArgs) ->
	     N1 is N - NArgs,
	     FOL1 is FOL + NArgs,
	     make_lambda(N1,T,T2),
	     hn_lazy(T2,FOL1,FNL,NEnv,WHNF,E)
	;
	    FOL1 is FOL + N,
	    hn_lazy(T,FOL1,FNL,NEnv,true,(Tp,OLp,NLp,Ep)),
	    NArgs1 is NArgs - N,
	    contract_and_norm(Tp,Rargs,OrigTerm,OL,OLp,NL,NLp,Ep,EE,NArgs1,
                              true,WHNF,E)
	).
contract_and_norm_(app(T,Argsp),_,_,Args,OL,_,NL,_,_,E,_,_,_,(X,0,0,[])) :-
	!,
	(
	    (OL=0, NL=0) ->
	     append(Argsp, Args, ArgsList),
	     new_ref(app(T,ArgsList),X)
	;
	    map(Args,(''(X1,X2) :- new_ref(susp(X1,OL,NL,E),X2)),MapList),
	    append(Argsp,MapList,ArgsList),
	    new_ref(app(T,ArgsList),X)
	).
contract_and_norm_(_,Term,OrigTerm,Args,OL,_,NL,_,_,E,_,Changed,_,
                   (X,0,0,[])) :-
	(
	    (OL=0, NL=0) ->
	     (
		 (Changed = true) -> new_ref(app(Term,Args),X)
	     ;
		 X = OrigTerm
	     )
	;
	    map(Args,(''(X1,X2) :- new_ref(susp(X1,OL,NL,E),X2)),MapList),
	    new_ref(app(Term,MapList),X)
	).

extend_env(0, Args, _, _, _, E, (Args,E)).
extend_env(_, [], _, _, _, E, ([],E)).
extend_env(N, [A|Rargs], OL, NL, FNL, E, X) :-
	N1 is N - 1,
	argwrap(A,OL,NL,E,Y),
	extend_env(N1, Rargs, OL, NL, FNL, [bndg(Y,FNL)|E], X).

argwrap(A, OL, NL, E, X) :-
	(
	    (OL = 0, NL = 0) -> X = A
	;
	    new_ref(susp(A,OL,NL,E),X)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Head normalization function. Reduction is done destructively and the 
% head normalized result is returned in dereferenced form. Also note that 
% iterated abstractions and applications are collected at the top level.
% The function expects a dereferenced term as input
hnorm(Term1, Term2) :-
	deref(Term1, X),
	hn_lazy(X, 0, 0, [], false, (Term2, _, _, _)).


