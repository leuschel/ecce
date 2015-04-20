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
	L = [S,E,N,D,M,O,R,Y],
	L in 0 .. 9,
	all_different(L),
	M .>. 0,
	S .>. 0,
	1000*S + 100*E + 10*N + D +
       1000*M + 100*O + 10*R + E .=.
       10000*M + 1000*O + 100*N + 10*E + Y,
       dlabeling(L, one, 0) @ Manager,
       statistics(walltime,[_, Time]),
       format("Used ~d milliseconds~n", Time).
