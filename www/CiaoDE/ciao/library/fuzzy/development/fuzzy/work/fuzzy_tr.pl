:- module(fuzzy_tr,[fuzzy_pred/3],[]).

:- use_module(library(aggregates), [findall/4]).
:- use_module(library(terms),[copy_args/3]).
:- use_module(library(messages),[error_message/2]).

:- data fpred/1.

:- data fclause/2.

:- data frule/3.

:- data fnegclause/3.
%:- data fagrclause/5.

:- data faggr/1.

:- include(library('fuzzy/ops')).
:- include(library('clpr/ops')).

build_cl(X1,M1,X2,M2,Name,(H :- ExpMax , ExpMin  ),Comp):-
	M1 == M2,!,
	functor(H,Name,2),
	arg(2,H,M1),
	arg(1,H,X),
	( 
	    X1 =< X2 ->
	    ( 
		Comp == comp ->
		ExpMax = (X .=<. X2)
	    ;
		ExpMax = (X .<. X2)
	    ),
	    ExpMin = ( X.>=. X1)
	;
	    (
		Comp == comp ->
		ExpMin = ( X.>=. X2)
	    ;
		ExpMin = ( X.>. X2)
	    ),
	    ExpMax = (X .=<. X1)
	).

build_cl(X1,M1,X2,M2,Name,(H :- ExpMax , ExpMin, X1MX2 * M .=. M1MM2 * X + Resto),Comp):-
	functor(H,Name,2),
	arg(2,H,M),
	arg(1,H,X),
	X1MX2 is X1 - X2,
	M1MM2 is M1 - M2,
	Resto is M2 * X1 - M1 * X2,
	( 
	    X1 =< X2 ->
	    ( 
		Comp == comp ->
		ExpMax = (X .=<. X2)
	    ;
		ExpMax = (X .<. X2)
	    ),
	    ExpMin = ( X.>=. X1)
	;
	    (
		Comp == comp ->
		ExpMin = ( X.>=. X2)
	    ;
		ExpMin = ( X.>. X2)
	    ),
	    ExpMax = (X .=<. X1)
	).
	
build_cls(Name,[(X1,M1),(X2,M2)],[CL]):- 
	   build_cl(X1,M1,X2,M2,Name,CL,comp).

build_cls(Name,[(X1,M1),(X2,M2)|Lista],[CL|Cls]):-
	    build_cl(X1,M1,X2,M2,Name,CL,incomp),
	    build_cls(Name,[(X2,M2)|Lista],Cls).
	
add_fuzzy_clause(Cls,Cls1):-
	findall(c(H,B,T),retract_fact(frule(H,B,T)),L,[]),
	add_fuzzy_clause0(L,Cls,Cls1).

add_fuzzy_clause0([],Cls,Cls).
add_fuzzy_clause0([c(H,B,Type)|L],Cls1,Cls2):-
	Type == fact,!,
	assertz_fact(frule(H,B,Type)),
	add_fuzzy_clause0(L,Cls1,Cls2).
add_fuzzy_clause0([c(H,B,Type)|L],Cls,Cls2):-
	obtain_crisp(B,Crisps,[],NB),
	assertz_fact(frule(H,NB,Type)),
	fuzzyfing(Crisps,Cls,Cls1),
	add_fuzzy_clause0(L,Cls1,Cls2).

fuzzyfing([],C,C).
fuzzyfing([Name/Ar|Crisp],Cls,Cls2):-
	A is Ar + 1,
	atom_concat('$f_',Name,F),
	( 
	    fpred(F/A) ->  Cls1 = Cls
	;
	    assertz_fact(fpred(F/A)),
	    functor(N,Name,Ar),
	    functor(H,F,A),
	    copy_args(Ar,H,N),
	    arg(A,H,M),
	    Cls = [(H:-if(N,M = 1,M = 0))|Cls1]
	),
	fuzzyfing(Crisp,Cls1,Cls2).


obtain_crisp((A,B),Crisps,Tail,(NA,NB)):-
	obtain_crisp(A,Crisps,T1,NA),
	obtain_crisp(B,T1,Tail,NB).

obtain_crisp({A},Crisp,Crisp,A).  % to manage crisp calls
obtain_crisp(A,Crisp,Crisp,A):-
	functor(A,F,Ar),
	fpred(F/Ar),!.
obtain_crisp(A,[Name/Ar|Crisp],Crisp,H):-
	functor(A,Name,Ar),
       	Ar1 is Ar + 1,
	atom_concat('$f_',Name,F),
	functor(H,F,Ar1),
	copy_args(Ar,H,A).



fuzzy_pred(end_of_file,Cls,_) :-
	!,
        assertz_fact(fpred('=>'/_)),
	add_fuzzy_clause(Cls,Cls1),
	findall(CL,(retract_fact(frule(H,B,T)),
	            add_contr(H,B,T,CL)
		   ),
		   Cls1,Negated
	       ),
	findall(CL,(retract_fact(fnegclause(N,O,A)),
	            add_neg(N,O,A,CL)
		   ),
		   Negated,[end_of_file]
	       ),
%		   Negated,Aggreg
%	       ),
% 	findall(CL,(retract_fact(fagrclause(F,F1,F2,A,Type)),
% 	            add_agg(F,F1,F2,A,Type,CL)
% 		   ),
% 		   Aggreg,[end_of_file]
% 	       ),
	retractall_fact(fpred(_)).
	
fuzzy_pred( (Name :# fuzzy_predicate Lista),Cls,_):-
	!,
	( 
	    assertz_fact(fpred(Name/2)),
	    list(Lista) ->
	     build_cls(Name,Lista,Cls)
	;
	    error_message("fuzzy predicate ~p is not well defined",[Name])
	).
	
fuzzy_pred((H := ),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,true,fact)).

fuzzy_pred((H := B),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,B,less)).

fuzzy_pred((H :~  ),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,true,fact)).

% explicit aggregator:
fuzzy_pred((H :~ B0),[],_):-
	nonvar(B0),
	functor(B0,Aggr,1),
	faggr(Aggr), !,
	arg(1,B0,B),
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,B,Aggr)).

fuzzy_pred((:- aggr A),(:- op(1190,fx,A)),_):-
	asserta_fact(faggr(A)), !.

% default
fuzzy_pred((H :~  B),[],_):-
	!,
	functor(H,F,A),
	(
	    fpred(F/A) -> true
	;
	    assertz_fact(fpred(F/A))
	),
	assertz_fact(frule(H,B,min)).


fuzzy_pred((Npred :# fnot Opred/A),[],_):-
	!,
	(
	    fpred(Npred/A) -> true
	;
	    assertz_fact(fpred(Npred/A))
	),
	assertz_fact(fnegclause(Npred,Opred,A)).


% fuzzy_pred((F :# aggr_dprod (F1/A,F2/A)),[],_):-
% 	!,
% 	(
% 	    fpred(F/A) -> true
% 	;
% 	    assertz_fact(fpred(F/A))
% 	),
% 	assertz_fact(fagrclause(F,F1,F2,A,dprod)).

% fuzzy_pred((F :# aggr_dluka (F1/A,F2/A)),[],_):-
% 	!,
% 	(
% 	    fpred(F/A) -> true
% 	;
% 	    assertz_fact(fpred(F/A))
% 	),
% 	assertz_fact(fagrclause(F,F1,F2,A,dluka)).

% fuzzy_pred((F :# aggr_max (F1/A,F2/A)),[],_):-
% 	!,
% 	(
% 	    fpred(F/A) -> true
% 	;
% 	    assertz_fact(fpred(F/A))
% 	),
%  	assertz_fact(fagrclause(F,F1,F2,A,max)).

% fuzzy_pred((F :# aggr_prod (F1/A,F2/A)),[],_):-
% 	!,
% 	(
% 	    fpred(F/A) -> true
% 	;
% 	    assertz_fact(fpred(F/A))
% 	),
% 	assertz_fact(fagrclause(F,F1,F2,A,prod)).

% fuzzy_pred((F :# aggr_luka (F1/A,F2/A)),[],_):-
% 	!,
% 	(
% 	    fpred(F/A) -> true
% 	;
% 	    assertz_fact(fpred(F/A))
% 	),
% 	assertz_fact(fagrclause(F,F1,F2,A,luka)).

% fuzzy_pred((F :# aggr_min (F1/A,F2/A)),[],_):-
% 	!,
% 	(
% 	    fpred(F/A) -> true
% 	;
% 	    assertz_fact(fpred(F/A))
% 	),
%  	assertz_fact(fagrclause(F,F1,F2,A,min)).


fuzzy_pred(( F :# fuzzy Name/Ar ),[(H:-if(N,M = 1,M = 0))],_):-
	!,
	A is Ar + 1,
	assertz_fact(fpred(F/A)),
	functor(N,Name,Ar),
	functor(H,F,A),
	copy_args(Ar,H,N),
	arg(A,H,M).


fuzzy_pred(A,A,_).


addconstrain([],Mu,(Mu .>=.0,Mu .=<.1)).
addconstrain([X|RestV],Mu,(X.>=.Mu,RestC)):-
	addconstrain(RestV,Mu,RestC).


add_contr(H,B,less,(H :- B,AB)):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
 	addconstrain(ListVar,Mu,AB).

add_contr(H,_B,fact,(H:- Mu .>=.0,Mu .=<.1 )):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu).

add_contr(H,B,min,(H :- B,AB)):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
	AB = (minim(ListVar,Mu),Mu .>=.0,Mu .=<.1).

add_contr(H,B,luka,(H :- B,AB)):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
	AB = (lukalist(ListVar,Mu),Mu .>=.0,Mu .=<.1).

add_contr(H,B,prod,(H :- B,AB)):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
        (
	    ListVar == [] ->
	    AB = (Mu .>=.0,Mu .=<.1)
	;
	    transprod(ListVar,Prod),
	    AB = (Mu .=. Prod,Mu .>=.0,Mu .=<.1)
	).

add_contr(H,B,max,(H :- B,AB)):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
	AB = (maxim(ListVar,Mu),Mu .>=.0,Mu .=<.1).

add_contr(H,B,dluka,(H :- B,AB)):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
	AB = (dlukalist(ListVar,Mu),Mu .>=.0,Mu .=<.1).

add_contr(H,B,dprod,(H :- B,AB)):- !,
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
	AB = (dprodlist(ListVar,Mu),Mu .>=.0,Mu .=<.1).
 
add_contr(H,B,Any,(H :- B,AB)):-
 	functor(H,_,Ar),
 	arg(Ar,H,Mu),
 	memfunct(B,ListVar,[]),
	AB = (inject(ListVar,Any,Mu),Mu .>=.0,Mu .=<.1).
 
add_neg(N,O,A,(NewPred :- OldPred,Mu .=. 1 - MuO)):-
	(
	    fpred(O/A) ->
	    functor(OldPred,O,A),
	    functor(NewPred,N,A),
	    Arm1 is A - 1,
	    copy_args(Arm1,OldPred,NewPred),
	    arg(A,NewPred,Mu),
	    arg(A,OldPred,MuO)
	;
	    error_message("~p is not a fuzzy predicate",[O/A])
	).
			
% add_agg(F,F1,F2,A,Type,(H :- B1,B2,Res)):-
% 	(
% 	    (fpred(F1/A),fpred(F2/A)) ->
% 	    functor(B1,F1,A),
% 	    functor(B2,F2,A),
% 	    functor(H,F,A),
% 	    Arm1 is A - 1,
% 	    copy_args(Arm1,H,B1),
% 	    copy_args(Arm1,H,B2),
% 	    arg(A,H,Mu),
% 	    arg(A,B1,Mu1),
% 	    arg(A,B2,Mu2),
% 	    functor(Res,Type,3),
% 	    arg(1,Res,Mu1),
% 	    arg(2,Res,Mu2),
% 	    arg(3,Res,Mu)
% 	;
% 	    error_message("~p : attempt to aggregate non fuzzy predicate",[F])
% 	).
	     




transprod([X],X).
transprod([X|RestV],(X * RestP)):-
	transprod(RestV,RestP).

memfunct((A,B),ListVar,Tail):-
	!,
 	memfunct(A,ListVar,TListVar),
 	memfunct(B,TListVar,Tail).
memfunct({_A},R,R).  % to manage crisp calls
memfunct(A,[X|R],R):-
 	functor(A,F,Ar),
 	fpred(F/Ar),!,
 	arg(Ar,A,X).
memfunct(_,R,R).
