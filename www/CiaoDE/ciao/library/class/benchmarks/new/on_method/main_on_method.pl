%%---------------------------------------------------------------------------
%% new/2 benchmarking for O'CIAO
%%
%%---------------------------------------------------------------------------

:- module(main_on_method,[main/1],[]).


:- use_module(library(system)).
:- use_module(library(dummy)).
:- use_module(engine(internals)).

:- use_package(objects).

%%---------------------------------------------------------------------------

:- use_class(class1method).
:- use_class(class5method).
:- use_class(class10method).
:- use_class(class50method).
:- use_class(class100method).
:- use_class(class200method).
:- use_class(class500method).

%%---------------------------------------------------------------------------

validate(1,class1method).
validate(5,class5method).
validate(10,class10method).
validate(50,class50method).
validate(100,class100method).
validate(200,class200method).
validate(500,class500method).

main([NumAttrAsAtom,CyclesAsAtom]) :-
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
	T is ((T1-T0) - (T2-T1)),
	Time is T/Cycles.

%%---------------------------------------------------------------------------

average(L,R) :-
	average_aux(L,0,0,R).

average_aux([],Acc,N,R) :-
	R is Acc/N.

average_aux([I|Next],Acc,N,R) :-
	M is N+1,
	NAcc is Acc+I,
	average_aux(Next,NAcc,M,R).
