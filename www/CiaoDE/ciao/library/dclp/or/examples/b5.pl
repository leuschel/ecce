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

main(N, M) :-
	statistics(walltime,_),
	address(manager, Manager),
	constrain_values(N, N, Qs),
	all_different(Qs),!,
	dlabeling(Qs, M, 0) @ Manager,
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
	
constrain_values(0, _N, []).
constrain_values(N, Range, [X|Xs]):-
        N > 0, 
        X in 1 .. Range,
        N1 is N - 1,
        constrain_values(N1, Range, Xs),
        no_attack(Xs, X, 1).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
	Nb1 is Nb + 1,
	no_attack(Ys, Queen, Nb1),
	Queen .<>. Y + Nb,
	Queen .<>. Y - Nb.	
