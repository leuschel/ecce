% UNDER DEVELOPMENT.
%-------------------
%
% To run this example, just use this module from the top-level and
% type benchmark.
%

:- module(benchmark,[]).

:- use_module(library('javall/javart')).
:- use_module(library(lists)).
:- use_module(library(prolog_sys)).
:- export(benchmark/0).
:- export(main/0).

:- dynamic connected/1.

:- set_prolog_flag(multi_arity_warnings,off).

main:-
	benchmark.

benchmark:-
	java_start,
	bench_strings(1000,TimeStrings),

	bench_frames(100,TimeFrames),

	display('Time creating strings:'),display(TimeStrings),nl,
	display('Time creating Frames: '),display(TimeFrames),nl,
	java_stop.

bench_strings(N,Time) :-
	elapsed_time(_),
	bench_strings(N),
	elapsed_time(Time).

bench_strings(0).
	
bench_strings(N) :-
	javart:java_create_object('java.lang.String'('Prueba'),_Str),
	N1 is N - 1,
	bench_strings(N1).

bench_frames(N,Time) :-
	elapsed_time(_),
	bench_frames(N),
	elapsed_time(Time).

bench_frames(0).
bench_frames(N) :-
	javart:java_create_object('java.awt.Frame'('Prueba'),_Frame),
	N1 is N - 1,
	bench_strings(N1).

elapsed_time(T) :-
	statistics(walltime,[_,T]).
