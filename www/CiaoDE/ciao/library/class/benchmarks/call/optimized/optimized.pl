%%---------------------------------------------------------------------------
%% Method Calling benchmarking for O'CIAO
%%
%%---------------------------------------------------------------------------

:- module(optimized,[main/0],[objects]).

:- use_module(library(system)).
:- use_module(library(prolog_sys)).
:- use_module(engine(internals)).

%%---------------------------------------------------------------------------

:- use_module(call_at_module).
:- use_class(call_at_class).

%%---------------------------------------------------------------------------

main :-
	set_prolog_flag(gc,off),
	Inst new call_at_class,
	get_ratio(400000,Inst,Ratio),
	dump_ratio(Ratio).

dump_ratio((R,K)) :-
	inform_user(['Ratio on OPTIMIZED predicate calling ',R,'% ',K]),
	true.

get_ratio(Cycles,Inst,(R,K)) :-
	repeat_and_time(Cycles,Inst,T1),
	repeat_and_time(Cycles,call_at_module,T2),
	T1 > 0, T2 > 0,
	!,
	( T1>T2 ->
	  ( K = slower, R is ceiling(((T1-T2)/T2)*100) )
	;
	  ( K = faster, R is ceiling(((T2-T1)/T1)*100) )
	),
	true.
get_ratio(_,_,_) :-
	inform_user(['Not enought clock resolution: exiting']),
	fail.

%%---------------------------------------------------------------------------

repeat(0,_) :- !.

repeat(N,Inst) :-
	M is N-1,
	( Inst:static_access -> true ; true ),
	repeat(M,Inst).

%%---------------------------------------------------------------------------

repeat_nul(0,_) :- !.

repeat_nul(N,X) :-
	M is N-1,
	( true -> true ; true ),
	repeat_nul(M,X).

%%---------------------------------------------------------------------------

repeat_and_time(Cycles,At,Time) :-
	elapsed_time(T0),
	repeat(Cycles,At),
	elapsed_time(T1),
	repeat_nul(Cycles,true),
	elapsed_time(T2),
	T is ((T1-T0) - (T2-T1)),
	Time is T/Cycles,
	inform_user([At,' : ',Time]),
	true.

%%---------------------------------------------------------------------------

elapsed_time(T) :-
	statistics(runtime,[T,_]).
