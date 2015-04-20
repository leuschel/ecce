%%---------------------------------------------------------------------------
%% new/2 benchmarking for O'CIAO
%%
%%---------------------------------------------------------------------------

:- module(main_on_attr,[main/1],[objects]).


:- use_module(library(system)).
:- use_module(library(prolog_sys)).
:- use_module(engine(internals)).

%%---------------------------------------------------------------------------

:- use_class(class1attr).
:- use_class(class5attr).
:- use_class(class10attr).
:- use_class(class50attr).
:- use_class(class100attr).
:- use_class(class200attr).
:- use_class(class350attr).
:- use_class(class500attr).

%%---------------------------------------------------------------------------

validate(1,class1attr).
validate(5,class5attr).
validate(10,class10attr).
validate(50,class50attr).
validate(100,class100attr).
validate(200,class200attr).
validate(350,class350attr).
validate(500,class500attr).

main([NumAttrAsAtom,CyclesAsAtom]) :-
	set_prolog_flag(gc,off),
	statistics(program,[P1,_]),
	statistics(symbols,[S1,_]),
	atom_codes(NumAttrAsAtom,NumAttrCodes),
	number_codes(NumAttr,NumAttrCodes),
	atom_codes(CyclesAsAtom,CyclesAsCodes),
	number_codes(Cycles,CyclesAsCodes),
	validate(NumAttr,Class),
	repeat_and_time(Cycles,Class,T),
	dump_data(NumAttr,T),
	statistics(program,[P2,_]),
	statistics(symbols,[S2,_]),
	Ps is P2-P1,
	Ss is S2-S1,
	display('# Program area usage: '), display(Ps), nl,
	display('# Symbol area usage:  '), display(Ss), nl,
	true.

%main :-
%	set_prolog_flag(gc,off),
%	repeat_and_time(1000,class1attr,T0),
%	repeat_and_time(1000,class5attr,T1),
%	repeat_and_time(1000,class10attr,T2),
%	repeat_and_time(1000,class50attr,T3),
%	repeat_and_time(1000,class100attr,T4),
%	repeat_and_time(100,class200attr,T5),
%	repeat_and_time(100,class500attr,T6),
%	dump_data(1,T0),
%	dump_data(5,T1),
%	dump_data(10,T2),
%	dump_data(50,T3),
%	dump_data(100,T4),
%	dump_data(200,T5),
%	dump_data(500,T6),
%	true.

%%---------------------------------------------------------------------------

dump_data(X,Y) :-
	display(X),
	display(' '),
	display(Y),
	nl.

%%---------------------------------------------------------------------------

elapsed_time(T) :-
	statistics(runtime,[T,_]).

%%---------------------------------------------------------------------------

repeat(0,_) :- !.

repeat(N,Class) :-
	M is N-1,
	X new Class,
	destroy X,
	repeat(M,Class).

%%---------------------------------------------------------------------------

nul_repeat(0,_) :- !.

nul_repeat(N,Class) :-
	M is N-1,
	nul_repeat(M,Class).

%%---------------------------------------------------------------------------

repeat_and_time(Cycles,Class,Time) :-
	elapsed_time(T0),
	repeat(Cycles,Class),
	elapsed_time(T1),
	nul_repeat(Cycles,Class),
	elapsed_time(T2),
%	B is T2-T1,BN is B/Cycles,
%	inform_user(['# Bucle: ',B,' ',BN]),
%	N is T1-T0,NN is N/Cycles,
%	inform_user(['# new-destroy: ',N,' ',NN]),
	T is ((T1-T0) - (T2-T1)),
	Time is T/Cycles.
