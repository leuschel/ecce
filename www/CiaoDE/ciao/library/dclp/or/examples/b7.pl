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
	alpha(L, Manager),
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

go_alpha(Manager):-    
    alpha(LD, Manager),
    display(LD), nl.

alpha(LD, Manager):-
        LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],
        LD in 1..26,
        all_different(LD),
        sum_alpha([B,A,L,L,E,T], 45),
        sum_alpha([C,E,L,L,O], 43),
        sum_alpha([C,O,N,C,E,R,T], 74),
        sum_alpha([F,L,U,T,E], 30),
        sum_alpha([F,U,G,U,E], 50),
        sum_alpha([G,L,E,E], 66),
	 sum_alpha([J,A,Z,Z], 58),
	 sum_alpha([L,Y,R,E], 47),
        sum_alpha([O,B,O,E], 53),
        sum_alpha([O,P,E,R,A], 65),
        sum_alpha([P,O,L,K,A], 59),
        sum_alpha([Q,U,A,R,T,E,T], 50),
        sum_alpha([S,A,X,O,P,H,O,N,E], 134),
        sum_alpha([S,C,A,L,E], 51),
        sum_alpha([S,O,L,O], 37),
        sum_alpha([S,O,N,G], 61),
        sum_alpha([S,O,P,R,A,N,O], 82),
        sum_alpha([T,H,E,M,E], 72),
        sum_alpha([V,I,O,L,I,N], 100),
	 sum_alpha([W,A,L,T,Z], 34),

	 dlabeling(LD, one, 0) @ Manager,
	 retrieve_solution(LD) @ Manager.

sum_alpha([],_).
sum_alpha([A], N) :- A = N.
sum_alpha([A,B|R], N) :-
	C .=. A + B,
	sum_alpha([C|R], N).	
