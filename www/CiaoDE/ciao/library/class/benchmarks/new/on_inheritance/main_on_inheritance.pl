%%---------------------------------------------------------------------------
%% new/2 benchmarking for O'CIAO
%%
%%---------------------------------------------------------------------------

:- module(main_on_inheritance,[main/1],[]).


:- use_module(library(system)).
:- use_module(library(dummy)).
:- use_module(engine(internals)).

:- use_package(objects).

%%---------------------------------------------------------------------------

:- use_class(class0inh).
:- use_class(class1inh).
:- use_class(class2inh).
:- use_class(class3inh).
:- use_class(class4inh).
:- use_class(class5inh).
:- use_class(class6inh).
:- use_class(class7inh).
:- use_class(class8inh).
:- use_class(class9inh).
:- use_class(class10inh).

%%---------------------------------------------------------------------------

main([LevelAsAtom,CyclesAsAtom]) :-
	atom_codes(CyclesAsAtom,CyclesAsCodes),
	number_codes(Cycles,CyclesAsCodes),
	atom_codes(LevelAsAtom,LevelAsCodes),
	number_codes(Level,LevelAsCodes),
	Level >= 0,
	Level =< 10,
	atom_concat(class,LevelAsAtom,Aux),
	atom_concat(Aux,inh,Class),
	repeat_and_time(Cycles,Class,T),
	pair(Level,T).

%%---------------------------------------------------------------------------

pair(X,Y) :-
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
	_ new Class,
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
	display('# Time '),display(T),nl,
	Time is T/Cycles.
