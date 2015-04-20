:- module(profiler_tr,[profiler_def/3],[assertions]).

:- use_module(library(lists),[append/3,length/2]).
:- use_module(library('profiler/profiler_utils'),
   [profile_reset/0]).

% Note: You must not use the use_module directive here with
% a module suitable to be profiled.

%:- use_module(library(patterns)).

:- data instrumented/3.
:- data profile/3.
:- data noprofile/3.

% do_profile(F,N,M) :-
% 	(   profile(PatternF,PatternN,M),
% 	    match_pattern_pred(PatternF,F),
% 	    match_pattern_pred(PatternN,N)
% 	;
% 	    \+ profile(_,_,_)
% 	),
% 	\+ (
% 	    noprofile(NoPatternF,NoPatternN,M),
% 	    match_pattern_pred(NoPatternF,F),
% 	    match_pattern_pred(NoPatternN,N)
% 	).

do_profile(F,N,M) :-
	(   profile(F,N,M)
	;
	    \+ profile(_,_,_)
	),
	\+ (
	    noprofile(F,N,M)
	).

ipred_recursivity(OrigFunctor,NewFunctor,Arity,Pred,NewPred) :-
	(
	    functor(Pred,OrigFunctor,Arity),
	    Pred =.. [_|Args] ->
	    NewPred =.. [NewFunctor|Args]
	;
	    NewPred = Pred
	).	

avoid_recursivity(OrigFunctor, NewFunctor, Arity,
	(Pred,Body), (NewPred,NewBody)) :-
	ipred_recursivity(OrigFunctor,NewFunctor,Arity,Pred,NewPred),
	avoid_recursivity(OrigFunctor,NewFunctor,Arity,Body,NewBody).
avoid_recursivity(OrigFunctor,NewFunctor,Arity,Pred,NewPred) :-
	ipred_recursivity(OrigFunctor,NewFunctor,Arity,Pred,NewPred).

profiler_def(A,B,M) :-
	profiler_def_(A,B,M).

% profiler_def_(0,Clause,_M) :-
% 	Clause = .

profiler_def_(end_of_file, end_of_file, M) :-
	retractall_fact(instrumented(_,_,M)),
	retractall_fact(profile(_,_,M)).

profiler_def_((:- profile(F/N)), [], M) :-
	assertz_fact(profile(F,N,M)).
profiler_def_((:- profile((A,B))), [], M) :-
	profiler_def((:- profile(A)), [], M),
	profiler_def((:- profile(B)), [], M).

profiler_def_((:- noprofile(F/N)), [], M) :-
	assertz_fact(noprofile(F,N,M)).
profiler_def_((:- noprofile((A,B))), [], M) :-
	profiler_def((:- noprofile(A)), [], M),
	profiler_def((:- noprofile(B)), [], M).

profiler_def_(OrigClause, Clauses, M) :-
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
	(   do_profile(F,N,M) ->
	    Head =.. [_|Args],
	    atom_concat('prof$',F, ProfFunctor),
	    ProfHead =.. [ProfFunctor|Args],
	    (   avoid_recursivity(F,ProfFunctor,N,Body,NewBody) -> true
	    ;
		display('problems in avoid_recursivity\n')
	    ),
	    (   instrumented(F,N,M) ->
		Clauses = [ ( ProfHead :- NewBody ) ]
	    ;
		length(DummyArgs,N),
		DummyHead =.. [F|DummyArgs],
		DummyBody =.. [ProfFunctor|DummyArgs],
		atom_concat(M,':',P0), atom_concat(P0,F,P),
		Clauses = [ ( cost_center(P,N) ),
		            ( DummyHead :-
			      profile__hook_cc_call(Prev_cc),
			      profile__hook_cc_fail(Prev_cc),
			      DummyBody,
			      profile__hook_cc_exit(Prev_cc,Active_cc),
			      profile__hook_cc_redo(Active_cc) ),
			    ( ProfHead :- NewBody ) ],
		assertz_fact(instrumented(F,N,M))
	    )
	;
	    Clauses=OrigClause
	).
