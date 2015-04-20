%%---------------------------------------------------------------------------
%% new/2 benchmarking for O'CIAO
%%
%%---------------------------------------------------------------------------

:- module(no_destroy,[main/1]).


:- use_module(library(system)).
:- use_module(library(prolog_sys)).
:- use_module(engine(internals)).

:- use_package(objects).

%%---------------------------------------------------------------------------

:- use_class(no_destroy_class).

%%---------------------------------------------------------------------------

main([NumInstancesAsAtom]) :-
	set_prolog_flag(gc,off),
	atom_codes(NumInstancesAsAtom,NumInstCodes),
	number_codes(NumInstances,NumInstCodes),
	repeat_and_time(NumInstances,Time),
%	AvgTime is Time / NumInstances,
	dump_data(NumInstances,Time).

%%---------------------------------------------------------------------------

dump_data(X,Y) :-
	XX is X /1000,
	display(XX),
	display(' '),
	display(Y),
	nl.

%%---------------------------------------------------------------------------

elapsed_time(T) :-
	statistics(runtime,[T,_]).

%%---------------------------------------------------------------------------

:- data p/1.

repeat(0,_) :- !.

repeat(N,Class) :-
	M is N-1,
	_X new Class,
	repeat(M,Class).

%%---------------------------------------------------------------------------

nul_repeat(0,_) :- !.

nul_repeat(N,Class) :-
	M is N-1,
	nul_repeat(M,Class).

%%---------------------------------------------------------------------------

repeat_and_time(Cycles,Time) :-
	elapsed_time(T0),
	repeat(Cycles,no_destroy_class),
	elapsed_time(T1),
	nul_repeat(Cycles,_),
	elapsed_time(T2),
	T is ((T1-T0) - (T2-T1)),
	Time is T/Cycles.
