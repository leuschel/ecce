
:- module(staticobj_vs_module,[main/0],[objects]).

:- use_module(library(system)).
:- use_module(library(prolog_sys)).
:- use_module(engine(internals)).

%%---------------------------------------------------------------------------

:- use_module(call_at_module).
:- use_class(call_at_class).

:- cl instance_of call_at_class.

%%---------------------------------------------------------------------------

main :-
	set_prolog_flag(gc,off),
	get_ratio(2400000,Ratio),
	dump_ratio(Ratio).

dump_ratio((R,K)) :-
	inform_user(['Ratio on STATIC calling versus static object ',
                     'calling: ',R,'% ',K]),
	true.

get_ratio(Cycles,(R,K)) :-
	repeat_and_time_at_class(Cycles,T1),
	repeat_and_time_at_module(Cycles,T2),
	T1 > 0, T2 > 0,
	!,
	( T1>T2 ->
	  ( K = slower, R is ceiling(((T1-T2)/T2)*100) )
	;
	  ( K = faster, R is ceiling(((T2-T1)/T1)*100) )
	),
	true.
get_ratio(_,_) :-
	inform_user(['Not enought clock resolution: exiting']),
	fail.

%%---------------------------------------------------------------------------

repeat_at_class(0) :- !.

repeat_at_class(N) :-
	M is N-1,
	( cl:static_access -> true ; true ),
	repeat_at_class(M).

%%---------------------------------------------------------------------------

repeat_at_module(0) :- !.

repeat_at_module(N) :-
	M is N-1,
	( call_at_module:static_access -> true ; true ),
	repeat_at_module(M).

%%---------------------------------------------------------------------------

repeat_nul(0) :- !.

repeat_nul(N) :-
	M is N-1,
	( true -> true ; true ),
	repeat_nul(M).

%%---------------------------------------------------------------------------

repeat_and_time_at_module(Cycles,Time) :-
	elapsed_time(T0),
	repeat_at_module(Cycles),
	elapsed_time(T1),
	repeat_nul(Cycles),
	elapsed_time(T2),
	T is ((T1-T0) - (T2-T1)),
	Time is T/Cycles,
	inform_user([call_at_module,' : ',Time,' ms.']),
	true.

repeat_and_time_at_class(Cycles,Time) :-
	elapsed_time(T0),
	repeat_at_class(Cycles),
	elapsed_time(T1),
	repeat_nul(Cycles),
	elapsed_time(T2),
	T is ((T1-T0) - (T2-T1)),
	Time is T/Cycles,
	inform_user([call_at_class,' : ',Time,' ms']),
	true.

%%---------------------------------------------------------------------------

elapsed_time(T) :-
	statistics(runtime,[T,_]).
