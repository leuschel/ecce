:- module(client_constr, [main/1]).

%% This is to understand the remote calls
:- use_package(remote).  
:- use_package(clpq).  

%% This is to understand locally the concurrency-related calls
:- use_package(hlc).


:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(aggregates)).
:- use_module(library(sockets), [hostname_address/2]).


main([]):- fail.
main([Host]):-
 %%         variables1(10, Host),
 %%         variables1(50, Host),
 %%         variables1(100, Host),
 %%         variables2(10, Host),
 %%         variables2(50, Host),
        variables2(100, Host),
        server_stop(Host).

variables1(N, Host):-
        format("All variables > 0, size ~d~n", [N]),
        length(List, N),
        set_constraints1(List),
        send_it(N, List, Host).

variables2(N, Host):-
        format("Fibonacci constraints, size ~d~n", [N]),
        length(List, N),
        format("List prepared~n", []),
        set_constraints2(List),
        format("Constraints set~n", []),
        send_it(N, List, Host),
        format("Constraints send~n", []).


set_constraints2([_X, _Y]):- !.
set_constraints2([X, Y, Z|Rest]):-
        X .=. Y + Z,
        X .>. Y,
        X .>. Z,
        set_constraints2([Y,Z|Rest]).


set_constraints1([]).
set_constraints1([X|Xs]):-
        X .>. 0,
        set_constraints1(Xs).


send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host).
send_it(_N, _List, _Host).

list_of_integers(0, []).
list_of_integers(N, [N|Ns]):-
        N > 0,
        N1 is N - 1,
        list_of_integers(N1, Ns).

send_thing(Size, Thing, Host):-
        statistics(walltime, _),
        received(Thing, Result) @ Host,
        statistics(walltime, [_, Time]),
        (
            Result = ok ->
            format("Time for size ~d: ~d~n", [Size, Time])
        ;
            format("Error acknowledgind result of size ~d~n", [Size])
        ).
        

%% received(_, ok).
