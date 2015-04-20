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

main(DGR) :-
	statistics(walltime,_),
	address(manager, Manager),
	DGR = [D,G,R,O,E,N,B,A,L,T],
	DGR in 0..9,
	D .>. 0,
	G .>. 0,
	all_different(DGR),
	100000*D + 10000*O + 1000*N + 100*A + 10*L + D +
       100000*G + 10000*E + 1000*R + 100*A + 10*L + D .=.
       100000*R + 10000*O + 1000*B + 100*E + 10*R + T,
       dlabeling(DGR, one, 0) @ Manager,
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
