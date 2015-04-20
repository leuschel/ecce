%:- module(catcher_tr,[catcher_def/3]).
:- module(catcher_tr,_).
:- use_module(library(terms),[atom_concat/2]).

:- data instrumented/4.

instrument_pred(Pred, T, C, M, F, N, IPred) :-
	member(Pred,['!', true]) ->
	IPred = Pred
 ;
	atom_number(Ta,T),
	atom_number(Ca,C),
	atom_concat(['in the body, clause ', Ca, '  term ', Ta], Message),
	IPred = catch(Pred,Error,catcher_handler(M,F,N,Pred,Message,Error)).

instrument_body((Pred,Body), T, C, M, F, N, (IPred, IBody)) :-
	!,
	instrument_pred(Pred, T, C, M, F, N, IPred),
	T1 is T + 1,
	instrument_body(Body, T1, C, M, F, N, IBody).
instrument_body(Pred, T, C, M, F, N, IPred) :-
	instrument_pred(Pred, T, C, M, F, N, IPred).

catcher_def(end_of_file, end_of_file, M) :-
	retractall_fact(instrumented(_,_,M,_)).

catcher_def(OrigClause, Clauses, M) :-
	(   OrigClause = (Head :- Body) ->
	    true
	;
	    (   OrigClause = (:- _) ->
		fail
	    ;
		Head = OrigClause,
		Body = true
	    )
	),
	functor(Head,F,N),
	atom(F),
%	number_codes(N,NC),
%	atom_codes(NA,NC),
%	atom_concat(['catcher$',M,':',F,'/',NA], CatchFunctor),
%	atom_concat(['catcher$',F], CatchFunctor),
%	CatchHead =.. [CatchFunctor|Args],
	(   instrumented(F,N,M,C0) ->
	    retract_fact(instrumented(F,N,M,_)),
	    C is C0 + 1
	;
%	    length(DummyArgs,N),
% 	    DummyHead =.. [F|DummyArgs],
% 	    DummyBody =.. [CatchFunctor|DummyArgs],
% 	    Clauses = [ ( DummyHead :- !,catch(DummyBody,Error,catcher_handler(M,DummyHead,Error) ) ),
% 	                ( CatchHead :- IBody ) ],
	    C = 1
	),
	assertz_fact(instrumented(F,N,M,C)),
	instrument_body(Body,1,C,M,F,N,IBody),
	(   N = 0 ->
	    Clauses = [ ( Head :- IBody ) ]
	;
	    length(DummyArgs,N),
	    Head =.. [_|Args],
	    DummyHead =.. [F|DummyArgs],
	    instrument_head(Args, DummyArgs, 1, C, M, F, N, IHead),
	    Clauses = [ ( DummyHead :- IHead, IBody ) ]
	),!.

instrument_head([Arg], [DummyArg], A, C, M, F, N, IArg) :-
	instrument_arg(Arg, DummyArg, A, C, M, F, N, IArg).

instrument_head([Arg|Args],[DummyArg|DummyArgs], A, C, M, F, N, (IArg, IHead)) :-
	!,
	instrument_arg(Arg, DummyArg, A, C, M, F, N, IArg),
	A1 is A + 1,
	instrument_head(Args, DummyArgs, A1, C, M, F, N, IHead).

instrument_arg(Arg, DummyArg, A, C, M, F, N, IArg) :-
	atom_number(Aa,A),
	atom_number(Ca,C),
	atom_concat(['in the head, clause ', Ca, ' argument ', Aa], Message),
	IArg = catch(DummyArg=Arg,Error,catcher_handler(M,F,N,Arg,Message,Error)).
