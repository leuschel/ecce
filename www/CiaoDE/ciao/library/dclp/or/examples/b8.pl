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
	test_alpha(L, Manager),
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
	

test_alpha(List, Manager) :-
	List=[A,B,C,E,F,G,J,L,N,O,R,T,U,Y,Z],
	List in 1..26, 
	all_different(List),
	sum_alpha([B,A,L,L,E,T], 45), 
	sum_alpha([C,E,L,L,O], 43),
	sum_alpha([C,O,N,C,E,R,T], 74),
	sum_alpha([F,L,U,T,E], 30),
	sum_alpha([F,U,G,U,E], 50),
	sum_alpha([G,L,E,E], 66),
	sum_alpha([J,A,Z,Z], 58),
	sum_alpha([L,Y,R,E], 47),

	dlabeling(List, one, 0) @ Manager,
	retrieve_solution(List) @ Manager.


sum_alpha([],_).
sum_alpha([A], N) :- A = N.
sum_alpha([A,B|R], N) :-
	C .=. A + B,
	sum_alpha([C|R], N).	
