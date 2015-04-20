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
	L=[A,B,C,D],
	A in 0 .. 16,
	B in 0 .. 12,
	C in 7 .. 12 .&. 15 .. 17,
	D in 0 .. 100,
	A .=. 2*B,
	D .=. 3 * A,
	C .=. A + B,
  	dlabeling(L, all, 0) @ Manager,
	retrieve_solution(L) @ Manager,
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
