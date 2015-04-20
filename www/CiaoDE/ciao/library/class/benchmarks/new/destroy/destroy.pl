%%---------------------------------------------------------------------------
%% new/2 benchmarking for O'CIAO
%%
%%---------------------------------------------------------------------------

:- module(destroy,[main/1]).


:- use_module(library(system)).
:- use_module(library(prolog_sys)).
:- use_module(engine(internals)).

:- use_package(objects).

%%---------------------------------------------------------------------------

:- use_class(destroy_class).

%%---------------------------------------------------------------------------

main([NumInstancesAsAtom]) :-
	set_prolog_flag(gc,off),
	atom_codes(NumInstancesAsAtom,NumInstCodes),
	number_codes(NumInstances,NumInstCodes),
	repeat_and_time(NumInstances,0,Time),
	AvgTime is Time / NumInstances,
	dump_data(NumInstances,AvgTime).

%%---------------------------------------------------------------------------

dump_data(X,Y) :-
	XX is X/1000,
	display(XX),
	display(' '),
	display(Y),
	nl.

%%---------------------------------------------------------------------------

elapsed_time(T) :-
	statistics(runtime,[T,_]).

%%---------------------------------------------------------------------------

repeat_and_time(0,T,T) :- !.

repeat_and_time(Cycles,Acc,Time) :-
	N is Cycles-1,
	creation_time(T),
	NewAcc is Acc+T,
	!,
	repeat_and_time(N,NewAcc,Time).

creation_time(T) :-
	elapsed_time(T0),
	X new destroy_class,
	elapsed_time(T1),
	destroy(X),
	T is (T1-T0).
