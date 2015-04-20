%%---------------------------------------------------------------------------
%% assert/retract benchmarking for O'CIAO
%%
%%---------------------------------------------------------------------------

:- module(main_on_assrt,[main/1],[objects]).

:- use_module(library(system)).
:- use_module(library(prolog_sys)).
:- use_module(engine(internals)).

%%---------------------------------------------------------------------------

:- use_module(assrt_at_module).
:- use_class(assrt_at_class).

%%---------------------------------------------------------------------------

main([CyclesAsAtom]):-
	set_prolog_flag(gc,off),
	atom_codes(CyclesAsAtom,CyclesCodes),
	number_codes(Cycles,CyclesCodes),
	module_concat(assrt_at_module,callme(Cycles),AtModule),
	Inst new assrt_at_class,
	arg(1,Inst,ID),
	module_concat(assrt_at_class,'obj$callme'(Cycles,ID),AtClass),
	elapsed_time(_),
	elapsed_time(T0),
	repeat(Cycles,AtModule),
	elapsed_time(T1),
	repeat(Cycles,AtClass),
	elapsed_time(T2),
	repeat(Cycles,true),
	elapsed_time(T3),
	Loop     is T3-T2,
	AsModule is (T1-T0)-Loop,
	AsClass  is (T2-T1)-Loop,
	Ratio    is 100/(AsClass/AsModule),
	dump_ratio(Ratio).

dump_ratio(Ratio) :-
	display(Ratio),
	display(' % slower'),nl,
	true.

%%---------------------------------------------------------------------------

elapsed_time(T) :-
	statistics(runtime,[T,_]).

%%---------------------------------------------------------------------------

repeat(0,_) :- !.
repeat(N,C) :-
	M is N-1,
	'$meta_call'(C),
	!,
	repeat(M,C).
