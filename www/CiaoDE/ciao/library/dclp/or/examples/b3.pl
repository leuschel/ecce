:- use_package(fd).

:- use_package(remote).
:- include(library(hlc)).

:- use_module(library(system)).

:- use_package(actmods).
:- use_module(library('actmods/webbased_locate')).
:- use_active_module(binder, 
	                     [address/2, 
			      add_address/2, 
			      remove_address/1, 
			      get_all_agencies/1]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).

:- use_module(library(lists)).

main(L) :-
	statistics(walltime,_),
	address(manager, Manager),
	L1 = [A, B, C, D, E, F], 
	L1 in 0..9,
	append(L1, [S], L),
       S in 0..1000000,
	all_different(L),
	A .>=. 1,
	A6 .=. 100000 * A,
	A5 .=. 10000 * B,
	A4 .=. 1000 * C,
	A3 .=. 100 * D,
	A2 .=. 10 * E,
	R1 .=. A2 + F,
	R2 .=. A3 + R1,
	R3 .=. A4 + R2,
	R4 .=. A5 + R3,
	S  .=. A6 + R4,
	dlabeling(L, 1000, 0) @ Manager,
	retrieve_solution(L) @ Manager,
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
	

sum_test(L, Manager):-
	L1 = [A, B, C, D, E, F], 
	L1 in 0..9,
	append(L1, [S], L),
       S in 0..1000000,
	different_fd(L),
	A .>=. 1,
	A6 .=. 100000 * A,
	A5 .=. 10000 * B,
	A4 .=. 1000 * C,
	A3 .=. 100 * D,
	A2 .=. 10 * E,
	R1 .=. A2 + F,
	R2 .=. A3 + R1,
	R3 .=. A4 + R2,
	R4 .=. A5 + R3,
	S  .=. A6 + R4,
	dlabeling(L, 1000, 0) @ Manager,
	retrieve_solution(L) @ Manager.
